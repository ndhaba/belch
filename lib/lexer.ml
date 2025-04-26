open Error

type token =
  (* Symbols *)
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | SEMICOLON
  | QUESTION
  | COLON
  (* Keywords *)
  | IF
  | ELSE
  | WHILE
  | SWITCH
  | CASE
  | DEFAULT
  | GOTO
  | BREAK
  | RETURN
  | EXTRN
  | AUTO
  (* Variadic *)
  | OPERATOR of string
  | NAME of string
  | STRING of string
  | CHAR of char
  | WORD of int

type ptoken = code_position * token

(** [shift pos amt] is a code position based on [pos] shifted right by [amt] *)
let shift pos amt = { line = pos.line; col = pos.col + amt }

(** [next_line pos] is the code position on the next line of [pos] *)
let next_line pos = { line = pos.line + 1; col = 0 }

(** [is_octal c] is true if the character can be used in a base 8 number *)
let is_octal c = c >= '0' && c <= '7'

(** [is_digit c] is true if the character can be used in a base 10 number *)
let is_digit c = c >= '0' && c <= '9'

(** [is_variable_start c] is true if [c] can be the start of a variable name *)
let is_variable_start c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'

(** [is_variable_char c] is true if [c] can be part of a variable name *)
let is_variable_char c = is_variable_start c || is_digit c

(** [is_operator_char c] is true if [c] can be part of an operator *)
let is_operator_char = function
  | '+' -> true
  | '-' -> true
  | '*' -> true
  | '/' -> true
  | '<' -> true
  | '>' -> true
  | '&' -> true
  | '|' -> true
  | '!' -> true
  | '=' -> true
  | _ -> false

(* [is_operator s] is true if [s] is a valid operator *)
let is_operator =
  let operators = [
    "*"; "&"; "-"; "!"; "++"; "--"; "~"; "/"; "%"; "+";
    "-"; "<<"; ">>"; "<"; "<="; ">"; ">="; "=="; "!=";
    "|"; "=*"; "=/"; "=%"; "=+"; "=-"; "=<<"; "=>>";
    "=<"; "=<="; "=>"; "=>="; "==="; "=!="; "=&"; "=^";
    "=|"
  ]
  in fun token -> List.mem token operators
[@@ocamlformat "disable"]

let lex_operator str i pos errs : ptoken * code_position * int =
  let slen = String.length str in
  let rec operator_len i j =
    if j >= slen then j - i
    else if is_operator_char (String.unsafe_get str j) then
      operator_len i (j + 1)
    else j - i
  in
  let len = operator_len i i in
  let operator = String.sub str i len in
  if is_operator operator then ()
  else error errs (UnknownOperator (pos, operator));
  ((pos, OPERATOR operator), shift pos len, i + len)

