open Error

(** A lexical token for the B programming language. *)
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
  | CHAR of bytes
  | NUMBER of int64

(** A lexical token for the B programming language along with
    its position *)
type point_token = int * token

(** [lex source] lexes the given [source] code and returns
    the lexical tokens it consists of and any errors or
    warnings if [source] is malformed *)
val lex : string -> point_token list * error_state

(** [show_token token] is a stringified representation of
    [token], as defined in the type definition *)
val show_token : token -> string

(** [show_point_token token] is a stringified representation of
    [token], as defined in the type definition *)
val show_point_token : point_token -> string