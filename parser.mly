
/* Analyseur syntaxique pour Petit-Java */

%{
  open Ast
%}

%token <Ast.constant> CST
%token <Ast.ident> IDENT
%token <string> CONSTRUCT
%token FUN FUNCTION MATCH WITH AS
%token LET REC IN AND
%token EQUAL PEQ
%token EOF
%token PLUS MINUS TIMES DIV MOD CONCAT
%token OR BAND NEQ LT LEQ GT GEQ
%token LP RP LB RB
%token COMMA SEMI PIPE ARROW UNSC DOUBLESEMI

/* Définitions des priorités et associativités des tokens */

%nonassoc IN
%nonassoc below_SEMI
%left     SEMI
%nonassoc DOUBLESEMI                    /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND                           /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc AS
%left     PIPE                          /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    ARROW                         /* function_type (t -> t -> t) */
%right    OR                            /* expr (e || e || e) */
%right    BAND                          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     LT GT                         /* expr (e OP e OP e) */
%left     PLUS MINUS                    /* expr (e OP e OP e) */
%left     MOD TIMES                     /* expr (e OP e OP e) */
%nonassoc prec_unary_minus              /* unary - */
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc LP
%nonassoc apply


/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.file> file

%%

file:
| es = def+ EOF
  { es }
;

def:
| e = exp DOUBLESEMI
  { {def = e; loc = $loc} }
;

mut_let:
| LET rec_flag = REC? def = separated_nonempty_list(AND, binding) e = option(preceded(IN,exp))
  { match e with
    | Some exp -> Texp_let (Option.is_some rec_flag, def, exp)
    | None -> Texp_let (Option.is_some rec_flag, def, Texp_constant (Tconst_unit)) }
;

binding:
| name = ident params = ident* EQUAL e = exp
  { { vb_pat = (Tpat_id name); vb_expr = e; vb_args = params; sd = Dynamic } }
;

exp:
| es = exp_list %prec below_COMMA
  { Texp_tuple es }
| LP e = exp RP
  { e }
| e = mut_let
  { e }
| e1 = exp SEMI e2 = exp
  { Texp_sequence (e1, e2) }
| f = exp arg = exp %prec apply
  { Texp_apply (f, arg) }
| e1 = exp o = binop e2 = exp
  { Texp_binop (o, e1, e2) }
| MINUS e = exp %prec prec_unary_minus
  { Texp_uminus e }
| c = constant
  { Texp_constant c}
| s = ident
  { Texp_ident s }
| FUN LP RP ARROW e = exp
  { Texp_function [(Tpat_constant Tconst_unit,e)]}
| FUN x = ident ARROW e = exp
  { Texp_function [(Tpat_id x, e)] }
| FUNCTION cases = pattern_matching
  { Texp_function cases }
| MATCH e = exp WITH cases = pattern_matching
  { Texp_match (e, cases) }
| c = CONSTRUCT params = exp
  { Texp_construct (c, params) }
| l = const_list_exp
  { Texp_construct ("List", Texp_tuple l)}
;

const_list_exp:
| LB RB
  { [] }
| LB l = list_elmts_exp SEMI? RB
  { l }
;

list_elmts_exp:
| e = exp
  { [e] }
| e = exp SEMI l = list_elmts_exp
  { e::l }

exp_list:
| es = exp_list COMMA e = exp
  { (e::es) }
| e1 = exp COMMA e2 = exp
  { [e2;e1] }
;

constant:
| c = CST
  { c }
| LP RP
  { Tconst_unit }
;

pattern_matching:
| PIPE? cases = separated_nonempty_list(PIPE, case)
  { cases }
;

case:
| p = pattern ARROW e = exp
  { (p, e) }
;

pattern:
| ps = pattern_list %prec below_COMMA
  { Tpat_tuple (List.rev ps) }
| UNSC
  { Tpat_any }
| LP p = pattern RP
  { p }
| p = pattern AS a = IDENT
  { Tpat_alias (p,a) }
| p1 = pattern PIPE p2 = pattern
  { Tpat_or (p1,p2) }
| c = constant
  { Tpat_constant c }
| c = CONSTRUCT params = pattern
  { Tpat_construct (c, params) }
| l = const_list_pat
  { Tpat_construct ("List", Tpat_tuple l)}
| s = IDENT
  { Tpat_id s }
;

const_list_pat:
| LB RB
  { [] }
| LB l = list_elmts_pat SEMI? RB
  { l }
;

list_elmts_pat:
| p = pattern
  { [p] }
| p = pattern SEMI l = list_elmts_pat
  { p::l }

pattern_list:
| ps = pattern_list COMMA p = pattern
  { (p::ps) }
| p1 = pattern COMMA p2 = pattern
  { [p2;p1] }
;

ident:
| x = IDENT { x }
;

%inline binop:
| PLUS  { Tbinop_add }
| MINUS { Tbinop_sub }
| TIMES { Tbinop_mul }
| DIV   { Tbinop_div }
| MOD   { Tbinop_mod }
| NEQ   { Tbinop_neq }
| PEQ   { Tbinop_peq }
| EQUAL { Tbinop_eq  }
| LT    { Tbinop_lt  }
| LEQ   { Tbinop_le  }
| GT    { Tbinop_gt  }
| GEQ   { Tbinop_ge  }
| BAND  { Tbinop_and }
| OR    { Tbinop_or  }
| CONCAT { Tbinop_concat }
;