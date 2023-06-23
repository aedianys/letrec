open Ast
open Format

let eta_expand = function
  | Texp_function _ as e -> e
  | e -> Texp_function [Tpat_id "*id", (Texp_apply (e, Texp_ident "*id"))]

let rec uninspectable set = function
  | Texp_let (rec_flag, bindings, body) ->
    Texp_let (rec_flag, bindings, uninspectable set body)

  | Texp_uminus e -> Texp_uminus (uninspectable set e)

  | Texp_tuple es -> Texp_tuple (List.map (uninspectable set) es)

  | Texp_apply (e1, e2) ->
    Texp_apply (uninspectable set e1, uninspectable set e2)

  | Texp_binop (b, e1, e2) ->
    Texp_binop (b, uninspectable set e1, uninspectable set e2)

  | Texp_constant c -> Texp_constant c

  | Texp_ident x as id -> if List.mem x set then
    (Texp_constant Tuninspectable) else id

  | Texp_function mat ->
    Texp_function (List.map (fun (p,e) -> p, uninspectable set e) mat)

  | Texp_match (e, mat) ->
    let mat' = (List.map (fun (p,e') -> p, uninspectable set e') mat) in
    Texp_match (uninspectable set e, mat')

  | Texp_construct (constr, e) ->
    Texp_construct (constr, uninspectable set e)

  | Texp_sequence (e1, e2) ->
    Texp_sequence (uninspectable set e1, uninspectable set e2)

let transpil file =
  let rec transpil_binding acc vb =
    let vb' = { vb with vb_expr = transpil_exp vb.vb_expr } in
    let acc' = match vb.sd with
      | Dynamic -> { acc with dyn = vb'::acc.dyn }
      | Static_bloc -> { acc with bloc = vb'::acc.bloc }
      | Constant -> { acc with const = vb'::acc.const }
      | Static_fun -> { acc with funs = vb'::acc.funs }
    in match vb.vb_pat with
      | Tpat_id id -> { acc' with var_set = id::acc.var_set }
      | _ -> assert false

  and process_bindings acc body =
    let tail = List.fold_left
      (fun e vb -> Texp_sequence(Texp_set (vb.vb_pat, vb.vb_expr), e))
      body acc.bloc in

    let funs = List.map
      (fun vb -> {vb with vb_expr = eta_expand vb.vb_expr}) acc.funs in
    let tail = if funs <> [] then Texp_let (true, funs, tail) else tail in

    let const = List.map
      (fun vb -> {vb with vb_expr = uninspectable acc.var_set vb.vb_expr})
      acc.const in
    let tail = if const <> [] then Texp_let (false, const, tail) else tail in

    let bloc = List.map (fun vb -> vb.vb_pat) acc.bloc in
    let tail = if bloc <> [] then Texp_new (bloc, tail) else tail in

    if acc.dyn <> [] then Texp_let (false, acc.dyn, tail) else tail
  
  and transpil_exp = function
    | Texp_let (true, bindings, body) ->
      let p = List.map (fun vb -> vb.vb_pat) bindings in
      let acc = {dyn = []; bloc = []; const = []; funs = []; var_set = []} in
      let acc = List.fold_left transpil_binding acc bindings in
      process_bindings acc (transpil_exp body)

    | Texp_let (false, bindings, body) ->
      let elist =
        List.map (fun vb -> { vb with vb_expr = transpil_exp vb.vb_expr })
        bindings in
      Texp_let (false, elist, transpil_exp body)

    | Texp_uminus e -> Texp_uminus (transpil_exp e)

    | Texp_tuple es -> Texp_tuple (List.map transpil_exp es)

    | Texp_apply (e1, e2) -> Texp_apply (transpil_exp e1, transpil_exp e2)

    | Texp_binop (b, e1, e2) -> Texp_binop (b, transpil_exp e1, transpil_exp e2)

    | Texp_constant c -> Texp_constant c

    | Texp_ident x -> Texp_ident x

    | Texp_function mat ->
      let mat' = List.map (fun (p,e) -> p, transpil_exp e) mat in
      Texp_function mat'

    | Texp_match (e, mat) ->
      let mat' = List.map (fun (p,e') -> p, transpil_exp e') mat in
      Texp_match (transpil_exp e, mat')

    | Texp_construct (constr, e) -> Texp_construct (constr, transpil_exp e)

    | Texp_sequence (e1, e2) -> Texp_sequence (transpil_exp e1, transpil_exp e2)
  
  in List.map (fun x -> transpil_exp x.def) file

(*PRINTER*)

let print_list print lp sep rp ff = function
  | [] -> ()
  | x :: l ->
      fprintf ff "%s%a" lp print x;
      List.iter (fprintf ff "%s%a" sep print) l;
      fprintf ff "%s" rp

let rec print_exp ff = function
  | Texp_let (rec_flag, bindings, body) ->
    let _rec = if rec_flag then "rec " else "" in
    if body <> (Texp_constant Tconst_unit) then
      fprintf ff "let %s%a in %a" _rec
        (print_list print_binding "" "\nand " "") bindings
        print_exp body
    else
      fprintf ff "let %s%a" _rec
        (print_list print_binding "" "\nand " "") bindings
  
  | Texp_uminus e -> fprintf ff "- %a" print_exp e

  | Texp_tuple el -> fprintf ff "%a" (print_list print_exp "(" ", " ")") el
  
  | Texp_apply (e1,e2) -> fprintf ff "%a %a" print_exp e1 print_exp e2

  | Texp_binop (b,e1,e2) ->
    fprintf ff "%a %a %a" print_exp e1 print_binop b print_exp e2
  
  | Texp_constant c -> print_constant ff c

  | Texp_ident id -> fprintf ff "%s" id

  | Texp_function mat ->
    (match mat with
    | (p,e)::[] ->
      fprintf ff "fun %a -> %a" print_pattern p print_exp e
    | _ -> fprintf ff "function@.%a" print_pattern_matching mat)

  | Texp_match (e,mat) -> 
    fprintf ff "match %a with@.%a" print_exp e print_pattern_matching mat
  
  | Texp_construct (c,e) -> fprintf ff "%s (%a)" c print_exp e

  | Texp_sequence (e1,e2) -> fprintf ff "%a; %a" print_exp e1 print_exp e2

  | Texp_new (pl,body) ->
    fprintf ff "new %a@.in %a"
      (print_list print_pattern "" " and " "") pl
      print_exp body
  
  | Texp_set (p,e) ->
    fprintf ff "set %a <- (%a)" print_pattern p print_exp e

and print_binding ff vb =
  (*debug:*) 
  fprintf ff "%a %a %a= %a" print_sd vb.sd
  print_pattern vb.vb_pat
    (print_list pp_print_string "" " " " ") vb.vb_args
    print_exp vb.vb_expr

and print_pattern_matching ff mat =
  let print_row ff (p,e) =
    fprintf ff "| %a -> %a@." print_pattern p print_exp e in
  fprintf ff "%a" (print_list print_row "" "" "") mat

and print_pattern ff = function
  | Tpat_any -> fprintf ff "_"
  | Tpat_id id -> fprintf ff "%s" id
  | Tpat_tuple pl -> print_list print_pattern "(" ", " ")" ff pl
  | Tpat_or (p1,p2) -> fprintf ff "%a | %a" print_pattern p1 print_pattern p2
  | Tpat_constant c -> print_constant ff c
  | Tpat_construct (c,p) -> fprintf ff "%s %a" c print_pattern p
  | Tpat_alias (p,a) -> fprintf ff "%a as %s" print_pattern p a

and print_constant ff = function
  | Tconst_unit -> fprintf ff "()"
  | Tconst_int n -> fprintf ff "%d" n
  | Tconst_string s -> fprintf ff "\"%s\"" s
  | Tconst_bool b -> fprintf ff "%b" b
  | Tuninspectable -> fprintf ff "_uninspectable"

and print_binop ff = function
  | Tbinop_add -> fprintf ff "+"
  | Tbinop_sub -> fprintf ff "-"
  | Tbinop_mul -> fprintf ff "*"
  | Tbinop_div -> fprintf ff "/"
  | Tbinop_mod -> fprintf ff "%%"
  | Tbinop_eq  -> fprintf ff "="
  | Tbinop_peq -> fprintf ff "=="
  | Tbinop_neq -> fprintf ff "<>"
  | Tbinop_lt  -> fprintf ff "<"
  | Tbinop_le  -> fprintf ff "<="
  | Tbinop_gt  -> fprintf ff ">"
  | Tbinop_ge  -> fprintf ff ">="
  | Tbinop_and -> fprintf ff "&&"
  | Tbinop_or  -> fprintf ff  "||"
  | Tbinop_concat -> fprintf ff "^"

and print_sd ff = function
  | Static_fun -> fprintf ff "*functions*"
  | Static_bloc -> fprintf ff "*blocs*"
  | Dynamic -> fprintf ff "*dynamics*"
  | Constant -> fprintf ff "*constants*"

let print_def ff def =
  fprintf ff "%a;;@." print_exp def

let print_def_list oc def_list =
  let ff = formatter_of_out_channel oc in
  print_list print_def "" "\n" "" ff def_list

let print_file oc file =
  let def_list = List.map (fun d -> d.def) file in
  print_def_list oc def_list