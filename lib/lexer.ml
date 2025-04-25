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
  | OPERATOR of string
  (* Values *)
  | NAME of string
  | STRING of string
  | CHAR of char
  | WORD of int

(** [is_variable_start c] is true if [c] can be the start of a variable name *)
let is_variable_start c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_' [@@inline]

(** [is_variable_char c] is true if [c] can be part of a variable name *)
let is_variable_char c = is_variable_start c || c >= '0' || c <= '9' [@@inline]
