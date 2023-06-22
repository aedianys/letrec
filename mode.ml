open Ast

exception Illegal_expr of binding

(** {1 Static or dynamic size} *)

module Ident = Map.Make(String)

let classify_expression : Ast.exp -> sd =
  let rec classify_expression env = function
    (* binding and variable cases *)
    | Texp_let (_, bindings, e) ->
        let env = classify_value_bindings env bindings in
        classify_expression env e
    | Texp_ident ident ->
      begin
        try Ident.find ident env
        with Not_found -> Dynamic
      end

    (* non-binding cases *)
    | Texp_sequence (_, e) ->
      classify_expression env e

    | Texp_construct _ 
    | Texp_tuple _ ->
        Static_bloc

    | Texp_apply _ 
    | Texp_binop _
    | Texp_uminus _ 
    | Texp_match _ ->
        Dynamic

    | Texp_function _ ->
        Static_fun

    | Texp_constant _ ->
        Constant

  and classify_value_bindings env bindings =
    let old_env = env in
    let add_value_binding env binding =
      match binding.vb_pat with
      | Tpat_id id ->
          let size = classify_expression old_env binding.vb_expr in
          binding.sd <- size;
          Ident.add id size env
      | _ ->
          (* Note: we don't try to compute any size for complex patterns *)
          env
    in
    List.fold_left add_value_binding env bindings
          
  in classify_expression Ident.empty

module Mode = struct
  type t = Ignore | Delay | Guard | Return | Dereference

  let equal = ((=) : t -> t -> bool)

  let rank = function
    | Ignore -> 0
    | Delay -> 1
    | Guard -> 2
    | Return -> 3
    | Dereference -> 4

  let join m m' =
    if rank m >= rank m' then m else m'

  let compose m' m = match m', m with
    | Ignore, _ | _, Ignore -> Ignore
    | Dereference, _ -> Dereference
    | Delay, _ -> Delay
    | Guard, Return -> Guard
    | Guard, ((Dereference | Guard | Delay) as m) -> m
    | Return, Return -> Return
    | Return, ((Dereference | Guard | Delay) as m) -> m
end

type mode = Mode.t = Ignore | Delay | Guard | Return | Dereference

module Env :
sig
  type t

  val single : string -> Mode.t -> t
  (** Create an environment with a single identifier used with a given mode.
  *)

  val empty : t
  (** An environment with no used identifiers. *)

  val find : string -> t -> Mode.t
  (** Find the mode of an identifier in an environment.  The default mode is
      Ignore. *)

  val unguarded : t -> string list -> string list
  (** unguarded e l: the list of all identifiers in l that are dereferenced or
      returned in the environment e. *)

  val dependent : t -> string list -> string list
  (** dependent e l: the list of all identifiers in l that are used in e
      (not ignored). *)

  val join : t -> t -> t
  val join_list : t list -> t
  (** Environments can be joined pointwise (variable per variable) *)

  val compose : Mode.t -> t -> t
  (** Environment composition m[G] extends mode composition m1[m2]
      by composing each mode in G pointwise *)

  val remove : string -> t -> t
  (** Remove an identifier from an environment. *)

  val take: string -> t -> Mode.t * t
  (** Remove an identifier from an environment, and return its mode *)

  val remove_list : string list -> t -> t
  (** Remove all the identifiers of a list from an environment. *)

  val equal : t -> t -> bool
end = struct
  module M = Map.Make(String)

  (** A "t" maps each rec-bound variable to an access status *)
  type t = Mode.t M.t

  let equal = M.equal Mode.equal

  let find (id: string) (tbl: t) =
    try M.find id tbl with Not_found -> Ignore

  let empty = M.empty

  let join (x: t) (y: t) =
    M.fold
      (fun (id: string) (v: Mode.t) (tbl: t) ->
         let v' = find id tbl in
         M.add id (Mode.join v v') tbl)
      x y

  let join_list li = List.fold_left join empty li

  let compose m env =
    M.map (Mode.compose m) env

  let single id mode = M.add id mode empty

  let unguarded env li =
    List.filter (fun id -> Mode.rank (find id env) > Mode.rank Guard) li

  let dependent env li =
    List.filter (fun id -> Mode.rank (find id env) > Mode.rank Ignore) li

  let remove = M.remove

  let take id env = (find id env, remove id env)

  let remove_list l env =
    List.fold_left (fun env id -> M.remove id env) env l
end
 
let rec pat_bound_idents : Ast.pattern -> string list = function
  | Tpat_any | Tpat_constant _ -> []
  | Tpat_id id -> [id]
  | Tpat_tuple ps -> List.concat_map pat_bound_idents ps
  | Tpat_or (p,_) -> pat_bound_idents p
  | Tpat_construct (id, p) -> pat_bound_idents p
  | Tpat_alias (p,id) -> pat_bound_idents p @ [id]

let remove_pat pat env =
  Env.remove_list (pat_bound_idents pat) env

let remove_patlist pats env =
  List.fold_right remove_pat pats env

type term_judg = Mode.t -> Env.t
type bind_judg = Mode.t -> Env.t -> Env.t

let option : 'a. ('a -> term_judg) -> 'a option -> term_judg =
  fun f o m -> match o with
    | None -> Env.empty
    | Some v -> f v m 
let list : 'a. ('a -> term_judg) -> 'a list -> term_judg =
  fun f li m ->
    List.fold_left (fun env item -> Env.join env (f item m)) Env.empty li
let array : 'a. ('a -> term_judg) -> 'a array -> term_judg =
  fun f ar m ->
    Array.fold_left (fun env item -> Env.join env (f item m)) Env.empty ar

let single : string -> term_judg = Env.single
let remove_id : string -> term_judg -> term_judg =
  fun id f m -> Env.remove id (f m)
let remove_ids : string list -> term_judg -> term_judg =
  fun ids f m -> Env.remove_list ids (f m)

let join : term_judg list -> term_judg =
  fun li m -> Env.join_list (List.map (fun f -> f m) li)

let empty = fun _ -> Env.empty

(* A judgment [judg] takes a mode from the context as input, and
   returns an environment. The judgment [judg << m], given a mode [m']
   from the context, evaluates [judg] in the composed mode [m'[m]]. *)
let (<<) : term_judg -> Mode.t -> term_judg =
  fun judg m -> fun m' -> judg (Mode.compose m' m)

(* A binding judgment [binder] expects a mode and an inner environment,
   and returns an outer environment. [binder >> judg] computes
   the inner environment as the environment returned by [judg]
   in the ambient mode. *)
let (>>) : bind_judg -> term_judg -> term_judg =
  fun binder term mode -> binder mode (term mode)

let rec expression : Ast.exp -> term_judg = function
    | Texp_ident ident ->
      single ident
    | Texp_let (rec_flag, bindings, body) ->
      value_bindings rec_flag bindings >> expression body
    | Texp_match (e, cases) ->
      (fun mode ->
        let pat_envs, pat_modes =
          List.split (List.map (fun c -> case c mode) cases) in
        let env_e = expression e (List.fold_left Mode.join Ignore pat_modes) in
        Env.join_list (env_e :: pat_envs))
    | Texp_constant _ ->
      empty
    | Texp_apply (e, arg) ->
        join [expression e; expression arg] << Dereference
    | Texp_binop (op,e1,e2) ->
        join [expression e1; expression e2] << Dereference
    | Texp_tuple exprs ->
      list expression exprs << Guard
    | Texp_construct (_, e) ->
      expression e << Guard
    | Texp_sequence (e1, e2) ->
      (*
        G1 |- e1: m[Guard]
        G2 |- e2: m
        --------------------
        G1 + G2 |- e1; e2: m

        Note: `e1; e2` is treated in the same way as `let _ = e1 in e2`
      *)
      join [
        expression e1 << Guard;
        expression e2;
      ]
    | Texp_function cases ->
      (*
         (Gi; _ |- pi -> ei : m[Delay])^i
         --------------------------------------
         sum(Gi)^i |- function (pi -> ei)^i : m

         Contrarily to match, the value that is pattern-matched
         is bound locally, so the pattern modes do not influence
         the final environment.
      *)
      let case_env c m = fst (case c m) in (*TODO*)
      list case_env cases << Delay

(* G |- let (rec?) (pi = ei)^i : m -| G' *)
and value_bindings : bool -> binding list -> bind_judg =
  fun rec_flag bindings mode bound_env ->
    let all_bound_pats =
      List.map (fun vb -> vb.vb_pat) bindings in
    let outer_env = remove_patlist all_bound_pats bound_env in
    let bindings_env =
      match rec_flag with
      | false ->
        (*
           (Gi, pi:_ |- ei : m[mbody_i])^i   (pi : mbody_i -| D)^i
           ------------------------------------------------------------
           Sum(Gi) + (D - (pi)^i) |- let (pi=ei)^i : m -| D
        *)
          let binding_env {vb_pat; vb_expr; _} m =
            let m' = Mode.compose m (pattern vb_pat bound_env) in
            remove_pat vb_pat (expression vb_expr m') in
          list binding_env bindings mode
      | true ->
        (*
           (Gi, (xj : mdef_ij)^j |- ei : m[mbody_i])^i   (xi : mbody_i -| D)^i
           G'i = Gi + mdef_ij[G'j]
           -------------------------------------------------------------------
           Sum(G'i) + (D - (pi)^i) |- let rec (xi=ei)^i : m -| D

           The (mdef_ij)^i,j are a family of modes over two indices:
           mdef_ij represents the mode of use, within e_i the definition of x_i,
           of the mutually-recursive variable x_j.

           The (G'i)^i are defined from the (Gi)^i as a family of equations,
           whose smallest solution is computed as a least fixpoint.

           The (Gi)^i are the "immediate" dependencies of each (ei)^i
           on the outer context (excluding the mutually-defined
           variables).
           The (G'i)^i contain the "transitive" dependencies as well:
           if ei depends on xj, then the dependencies of G'i of xi
           must contain the dependencies of G'j, composed by
           the mode mdef_ij of use of xj in ei.

           For example, consider:

             let rec z =
               let rec x = ref y
               and y = ref z
               in f x

           this definition should be rejected as the body [f x]
           dereferences [x], which can be used to access the
           yet-unitialized value [z]. This requires realizing that [x]
           depends on [z] through [y], which requires the transitive
           closure computation.

           An earlier version of our check would take only the (Gi)^i
           instead of the (G'i)^i, which is incorrect and would accept
           the example above.
        *)
          (* [binding_env] takes a binding (x_i = e_i)
             and computes (Gi, (mdef_ij)^j). *)
          let binding_env {vb_pat = x_i; vb_expr = e_i; _} =
            let mbody_i = pattern x_i bound_env in
            (* Gi, (x_j:mdef_ij)^j  *)
            let rhs_env_i = expression e_i (Mode.compose mode mbody_i) in
            (* (mdef_ij)^j (for a fixed i) *)
            let mutual_modes =
              let mdef_ij {vb_pat = x_j; _} = pattern x_j rhs_env_i in
              List.map mdef_ij bindings in
            (* Gi *)
            let env_i = remove_patlist all_bound_pats rhs_env_i in
            (* (Gi, (mdef_ij)^j) *)
            (env_i, mutual_modes) in
          let env, mdef =
            List.split (List.map binding_env bindings) in
          let rec transitive_closure env =
            let transitive_deps env_i mdef_i =
              (* Gi, (mdef_ij)^j => Gi + Sum_j mdef_ij[Gj] *)
              Env.join env_i
                (Env.join_list (List.map2 Env.compose mdef_i env)) in
            let env' = List.map2 transitive_deps env mdef in
            if List.for_all2 Env.equal env env'
            then env'
            else transitive_closure env'
          in
          let env'_i = transitive_closure env in
          Env.join_list env'_i
    in Env.join bindings_env outer_env


and case : Ast.pattern * Ast.exp -> mode -> Env.t * mode
  = fun (c_lhs, c_rhs) ->
    (*
       Ge |- e : m    Gg |- g : m[Dereference]
       G := Ge+Gg     p : mp -| G
       ----------------------------------------
       G - p; m[mp] |- (p (when g)? -> e) : m
    *)
    (fun m ->
       let env = expression c_rhs m in
       (remove_pat c_lhs env), Mode.compose m (pattern c_lhs env))

(* p : m -| G
   with output m and input G

   m is the mode under which the scrutinee of p is placed.
*)
and pattern : Ast.pattern -> Env.t -> mode = fun pat env ->
  (*
    mp := | Dereference if p is destructuring
          | Guard       otherwise
    me := sum{G(x), x in vars(p)}
    --------------------------------------------
    p : (mp + me) -| G
  *)
  let m_pat = if is_destructuring_pattern pat
              then Dereference
              else Guard
  in
  let m_env =
    pat_bound_idents pat
    |> List.map (fun id -> Env.find id env)
    |> List.fold_left Mode.join Ignore
  in
  Mode.join m_pat m_env

and is_destructuring_pattern : Ast.pattern -> bool = function
    | Tpat_any -> false
    | Tpat_id _ -> false
    | Tpat_alias (pat, _) -> is_destructuring_pattern pat
    | Tpat_constant _ -> true
    | Tpat_tuple _ -> true
    | Tpat_construct _ -> true
    | Tpat_or (l,r) ->
        is_destructuring_pattern l || is_destructuring_pattern r

let is_valid_recursive_expression idlist = function
  | Texp_function _ ->
     (* Fast path: functions can never have invalid recursive references *)
     true, Static_fun
  | expr -> let sd = classify_expression expr in
     match sd with
     | Static_fun | Static_bloc | Constant ->
        (* The expression has known size *)
        let ty = expression expr Return in
        Env.unguarded ty idlist = [], sd
     | Dynamic ->
        (* The expression has unknown size *)
        let ty = expression expr Return in
        Env.unguarded ty idlist = [] && Env.dependent ty idlist = [], sd

let check_recursive_bindings valbinds =
  let ids = List.filter_map
    (fun vb -> (match vb.vb_pat with Tpat_id id -> Some id | _ -> None))
    valbinds in
  List.iter
    (fun vb -> match is_valid_recursive_expression ids vb.vb_expr with
      | false, _ -> raise(Illegal_expr vb)
      | true, sd -> vb.sd <- sd
    )
    valbinds

let rec check_exp = function
  | Texp_uminus _ | Texp_constant _ | Texp_ident _ -> ()

  | Texp_let (rec_flag,binds,exp) ->
    if rec_flag then check_recursive_bindings binds;
    List.iter (fun vb -> check_exp vb.vb_expr) binds;
    check_exp exp

  | Texp_tuple exps -> List.iter check_exp exps

  | Texp_apply (f, args) -> check_exp f; check_exp args

  | Texp_binop (op,x,y) -> check_exp x; check_exp y

  | Texp_function pm -> List.iter (fun x -> snd x |> check_exp) pm

  | Texp_match (exp, pm) ->
    check_exp exp; List.iter (fun x -> snd x |> check_exp) pm

  | Texp_construct (id, exp) -> check_exp exp

  | Texp_sequence (exp1, exp2) -> check_exp exp1; check_exp exp2

