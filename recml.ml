open Format
open Lexing

let usage = "usage: recml [options] file.ml"

let parse_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".ml") then
      raise (Arg.Bad "no .ml extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1



let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let check_def (def:Ast.def) =
  try Mode.check_exp def.def
  with Mode.Illegal_expr vb ->
    report def.loc;
    let id = match vb.vb_pat with Tpat_id id -> id | _ -> assert false in
    eprintf "illegal expression : %s @." id

let () =
  let f = open_in file in
  let lb = Lexing.from_channel f in
  try
    let file' = (String.sub file 0 (String.length file - 3)) ^ ".cml" in
    let p = Parser.file Lexer.next_token lb in
    close_in f;
    if !parse_only then exit 0;
    List.iter check_def p;
    (*let oc = open_out file' in
    Format.pp_print_string (Format.formatter_of_out_channel oc) (Ast.show_file p)*)
    let p' = Compil.transpil p in
    let oc = open_out file' in
    Compil.print_def_list oc p'
  with
  | Lexer.Lexing_error s ->
    report (lexeme_start_p lb, lexeme_end_p lb);
    eprintf "lexical error: %s@." s;
    exit 1
  | Parser.Error ->
    report (lexeme_start_p lb, lexeme_end_p lb);
    eprintf "syntax error@.";
    exit 1
  | e ->
    eprintf "Anomaly: %s\n@." (Printexc.to_string e);
    exit 2