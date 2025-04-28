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
  | CHAR of bytes
  | NUMBER of int

type ptoken = int * token

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
  (* List of every single possible operator *)
  let operators = [
    "*"; "&"; "-"; "!"; "++"; "--"; "~"; "/"; "%"; "+";
    "-"; "<<"; ">>"; "<"; "<="; ">"; ">="; "=="; "!=";
    "|"; "=*"; "=/"; "=%"; "=+"; "=-"; "=<<"; "=>>";
    "=<"; "=<="; "=>"; "=>="; "==="; "=!="; "=&"; "=^";
    "=|"
  ]
  (* Check if [s] is in the list *)
  in fun s -> List.mem s operators

(** [is_octal_out_of_bounds num] is true if [num] cannot be stored in a 64-bit
    signed integer *)
let is_octal_out_of_bounds =
  let max = "377777777777777777777" in
  let maxlen = String.length max in
  fun num ->
    let len = String.length num in
    if len = maxlen then
      num > max
    else
      len > maxlen

(* [is_decimal_out_of_bounds num] is true if [num] cannot be stored in a 64-bit
   signed integer *)
let is_decimal_out_of_bounds =
  let max = string_of_int Int.max_int in
  let maxlen = String.length max in
  fun num ->
    let len = String.length num in
    if len = maxlen then
      num > max
    else
      len > maxlen

(** [int_of_octal str] is an integer value decoded from [str] *)
let int_of_octal str =
  let slen = String.length str in
  let zero = int_of_char '0' in
  let rec aux i acc =
    if i >= slen then
      acc
    else
      aux (i + 1) (Int.shift_left acc 3 + (int_of_char str.[i] - zero))
  in
  aux 0 0

(** [length_while f str i] is the length of the substring of [str] starting at
    [i] where [f] is true *)
let length_while f str i =
  let slen = String.length str in
  let rec aux i j =
    if j >= slen then
      j - i
    else if f (String.unsafe_get str j) then
      aux i (j + 1)
    else
      j - i
  in
  aux i i

(** [lex_operator str i errs] lexes the current position in [str] as an
    operator. If this operator is unknown, an error will be added to [errs] *)
let lex_operator str i errs : ptoken * int =
  (* Get the full operator *)
  let len = length_while is_operator_char str i in
  let operator = String.sub str i len in
  (* If the operator is unknown, add an error but continue execution *)
  if is_operator operator then
    ()
  else
    error errs (UnknownOperator (i, operator));
  (* Return the token *)
  ((i, OPERATOR operator), i + len)

(** [lex_string str i errs] lexes the current position in [str] as a string. If
    the string is not completed, an error will be added to [errs] *)
let lex_string str i errs : ptoken * int = Obj.magic ()

let lex_char str i errs : ptoken * int = Obj.magic ()

(** [lex_variable str i errs] lexes the current position in [str] as a name or
    number. *)
let lex_variable str i errs : ptoken * int =
  let len = length_while is_variable_char str i in
  let start = String.unsafe_get str i in
  (* Special case: 0 *)
  if start = '0' && len = 1 then
    ((i, NUMBER 0), i + 1)
  (* Everything else *)
  else
    let sub = String.sub str i len in
    (* If this is an octal number *)
    if start = '0' then
      (* Check that every character is valid *)
      if String.for_all is_octal sub then (
        (* Check that the octal number isn't out of bounds *)
        if is_octal_out_of_bounds sub then
          warn errs (OctalOutOfRange (i, sub))
        else
          ();
        (* Return the token *)
        ((i, NUMBER (int_of_octal sub)), i + len)
      )
      (* If there are normal decimal numbers (8, 9), tell the user its not
         octal form *)
      else if String.for_all is_digit sub then (
        error errs (InvalidOctal i);
        ((i, NUMBER 0), i + len)
      )
      (* If there are alphabet characters, tell the user you can't start a
         variable with numbers *)
      else (
        error errs (VarNumberStart i);
        ((i, NAME sub), i + len)
      )
    (* If this is a decimal number *)
    else if is_digit start then
      (* Check that all of the characters are valid *)
      if String.for_all is_digit sub then (
        (* Check that the decimal number isn't out of bounds *)
        if is_decimal_out_of_bounds sub then
          warn errs (DecimalOutOfRange (i, sub))
        else
          ();
        (* Return the token *)
        ((i, NUMBER (int_of_string sub)), i + len)
      )
      (* If there are alphabet characters, tell the user you can't start a
         variable with numbers *)
      else (
        error errs (VarNumberStart i);
        ((i, NAME sub), i + len)
      )
    (* If this is a name *)
    else
      let token =
        match sub with
        | "if" -> IF
        | "else" -> ELSE
        | "while" -> WHILE
        | "switch" -> SWITCH
        | "case" -> CASE
        | "default" -> DEFAULT
        | "goto" -> GOTO
        | "break" -> BREAK
        | "return" -> RETURN
        | "extrn" -> EXTRN
        | "auto" -> AUTO
        | name -> NAME name
      in
      ((i, token), i + len)

let lex str =
  let errs = new_error_state () in
  let slen = String.length str in
  let rec lex_raw i acc =
    if i >= slen then
      (List.rev acc, errs)
    else
      match String.unsafe_get str i with
      (* Whitespace *)
      | ' ' | '\r' | '\t' | '\n' -> lex_raw (i + 1) acc
      (* Symbols *)
      | '(' -> lex_raw (i + 1) ((i, LPAREN) :: acc)
      | ')' -> lex_raw (i + 1) ((i, RPAREN) :: acc)
      | '{' -> lex_raw (i + 1) ((i, LBRACE) :: acc)
      | '}' -> lex_raw (i + 1) ((i, RBRACE) :: acc)
      | '[' -> lex_raw (i + 1) ((i, LBRACKET) :: acc)
      | ']' -> lex_raw (i + 1) ((i, RBRACKET) :: acc)
      | ',' -> lex_raw (i + 1) ((i, COMMA) :: acc)
      | ';' -> lex_raw (i + 1) ((i, SEMICOLON) :: acc)
      | '?' -> lex_raw (i + 1) ((i, QUESTION) :: acc)
      | ':' -> lex_raw (i + 1) ((i, COLON) :: acc)
      (* String *)
      | '"' ->
          let token, i = lex_string str i errs in
          lex_raw i (token :: acc)
      (* Operator *)
      | c when is_operator_char c ->
          let token, i = lex_operator str i errs in
          lex_raw i (token :: acc)
      (* Names/numbers *)
      | c when is_variable_char c ->
          let token, i = lex_variable str i errs in
          lex_raw i (token :: acc)
      (* Everything else *)
      | _ -> Obj.magic ()
  in
  lex_raw 0 []
