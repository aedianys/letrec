
(* Analyseur lexical pour RecML *)

{
  open Lexing
  open Ast
  open Parser

  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      ["false", CST (Tconst_bool false);
       "true", CST (Tconst_bool true);
       "let", LET;
       "rec", REC;
       "in", IN;
       "function", FUNCTION;
       "fun", FUN;
       "match", MATCH;
       "with", WITH;
       "as", AS;
       "and", AND];
   fun s -> try Hashtbl.find h s with Not_found ->
   (let c = s.[0] in
    if Char.lowercase_ascii c = c then IDENT s else CONSTRUCT s)

  let string_buffer = Buffer.create 1024
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (alpha | '_') (alpha | digit | '_' | '\'')*
let integer = ('0' | ['1'-'9'] digit* )
let space = ' ' | '\t'

rule next_tokens = parse
  | '\n'    { new_line lexbuf; next_tokens lexbuf }
  | space+  { next_tokens lexbuf }
  | ident as id { [id_or_kwd id] }
  | '|'     { [PIPE]}
  | '+'     { [PLUS] }
  | '-'     { [MINUS] }
  | '*'     { [TIMES] }
  | '/'     { [DIV] }
  | '%'     { [MOD] }
  | "||"    { [OR] }
  | "&&"    { [BAND] }
  | '='     { [EQUAL] }
  | "=="    { [PEQ] }
  | "->"    { [ARROW] }
  | "<>"    { [NEQ] }
  | "<"     { [LT] }
  | "<="    { [LEQ] }
  | ">"     { [GT] }
  | ">="    { [GEQ] }
  | "^"     { [CONCAT] }
  | '('     { [LP] }
  | ')'     { [RP] }
  | '['     { [LB] }
  | ']'     { [RB] }
  | ','     { [COMMA] }
  | ";;"    { [DOUBLESEMI] }
  | ';'     { [SEMI] }
  | '_'     { [UNSC] }
  | '"'     { [CST (Tconst_string (str lexbuf))] }
  | "(*"    { comment lexbuf }
  | integer as n
            { try [CST (Tconst_int (int_of_string n))]
              with _ -> raise (Lexing_error ("constant too large: " ^ n)) }
  | eof     { [EOF] }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*)"    { next_tokens lexbuf }
  | '\n'    { new_line lexbuf; comment lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexing_error "unterminated comment") }

and str = parse
  | '"'
      { let s = Buffer.contents string_buffer in
	Buffer.reset string_buffer;
	s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
	str lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"';
	str lexbuf }
  | "\\\\"
      { Buffer.add_char string_buffer '\\';
	str lexbuf }
  | "\\"
      { raise (Lexing_error "illegal character in string: \\") }
  | _ as c
      { Buffer.add_char string_buffer c;
	str lexbuf }
  | eof
      { raise (Lexing_error "unterminated string") }

{

  let next_token =
    let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
    fun lb ->
      if Queue.is_empty tokens then begin
	let l = next_tokens lb in
	List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}