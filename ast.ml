type local = Lexing.position * Lexing.position
let pp_local _ _ = ()
let show_local _ = ""

type sd = Static_fun | Static_bloc | Dynamic | Constant
[@@deriving show]
(* Syntaxe abstraite issue de l'analyse syntaxique *)

type ident = string [@@deriving show]

type constant =
  | Tconst_unit
  | Tconst_int of int
  | Tconst_string of string
  | Tconst_bool of bool
  | Tuninspectable
  [@@deriving show]

type binop =
  | Tbinop_add | Tbinop_sub | Tbinop_mul | Tbinop_div | Tbinop_mod          (* + - * / % *)
  | Tbinop_eq | Tbinop_neq | Tbinop_lt | Tbinop_le | Tbinop_gt | Tbinop_ge  (* == != < <= > >= *)
  | Tbinop_and | Tbinop_or                                                  (* && || *)
  | Tbinop_concat
  | Tbinop_peq (*==*)
  [@@deriving show]

type pattern =
  | Tpat_any
  | Tpat_id of ident
  | Tpat_tuple of pattern list
  | Tpat_or of pattern * pattern
  | Tpat_constant of constant
  | Tpat_construct of ident * pattern
  | Tpat_alias of pattern * ident

and pattern_matching = (pattern * exp) list

and exp = 
  | Texp_let of bool * (binding list) * exp
  | Texp_uminus of exp
  | Texp_tuple of exp list
  | Texp_apply of exp * exp
  | Texp_binop of binop * exp * exp
  | Texp_constant of constant
  | Texp_ident of ident
  | Texp_function of pattern_matching
  | Texp_match of exp * pattern_matching
  | Texp_construct of ident * exp
  | Texp_sequence of exp * exp
  | Texp_new of pattern list * exp
  | Texp_set of pattern * exp

and binding = { vb_pat: pattern; vb_expr: exp; vb_args: ident list; mutable sd: sd }
[@@deriving show]

type compil_bindings =
  { dyn: binding list;
    bloc: binding list;
    const: binding list;
    funs: binding list;
    var_set: ident list }

type def = { def: exp; loc: local } [@@deriving show]

type file = def list [@@deriving show]