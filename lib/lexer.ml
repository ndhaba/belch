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
[@@ocamlformat "disable"]

(** [is_octal_out_of_bounds num] is true if [num] cannot be stored in a 64-bit
    signed integer *)
let is_octal_out_of_bounds =
  let max = "377777777777777777777" in
  let maxlen = String.length max in
  fun num ->
    let len = String.length num in
    if len = maxlen then num > max else len > maxlen

(* [is_decimal_out_of_bounds num] is true if [num] cannot be stored in a 64-bit
   signed integer *)
let is_decimal_out_of_bounds =
  let max = string_of_int Int.max_int in
  let maxlen = String.length max in
  fun num ->
    let len = String.length num in
    if len = maxlen then num > max else len > maxlen

(** [int_of_octal str] is an integer value decoded from [str] *)
let int_of_octal str =
  let slen = String.length str in
  let rec aux i acc =
    if i >= slen then acc
    else aux (i + 1) (Int.shift_left acc 3 + int_of_char str.[i])
  in
  aux 0 0

(** [length_while f str i] is the length of the substring of [str] starting at
    [i] where [f] is true *)
let length_while f str i =
  let slen = String.length str in
  let rec aux i j =
    if j >= slen then j - i
    else if f (String.unsafe_get str j) then aux i (j + 1)
    else j - i
  in
  aux i i

(** [length_until f str i] is the length of the substring of [str] starting at
    [i] that is terminated by a character that satisfies [f] *)
let length_until f str i =
  let slen = String.length str in
  let rec aux i j =
    if j >= slen || f (String.unsafe_get str j) then j - i else aux i (j + 1)
  in
  aux i i

(** [lex_operator str i errs] lexes the current position in [str] as an
    operator. If this operator is unknown, an error will be added to [errs] *)
let lex_operator str i errs : ptoken * int =
  (* Get the full operator *)
  let len = length_while is_operator_char str i in
  let operator = String.sub str i len in
  (* If the operator is unknown, add an error but continue execution *)
  if is_operator operator then ()
  else error errs (UnknownOperator (i, operator));
  ((i, OPERATOR operator), i + len)

(** [lex_string str i errs] lexes the current position in [str] as a string. If
    the string is not completed, an error will be added to [errs] *)
let lex_string str i errs : ptoken * int =
  let slen = String.length str in
  (* Get the full string *)
  let len = length_until (fun c -> c = '"' || c = '\n') str (i + 1) in
  let string = String.sub str (i + 1) len in
  (* If the string terminated because there's no more input... *)
  if i + len + 1 >= slen then (
    error errs (StringNotClosed i);
    ((i, STRING string), i + len + 1))
  else
    (* Check the character right after the body of the string *)
    let c = String.unsafe_get str (i + len + 1) in
    (* If it's a quote, this ran successfully *)
    if c = '"' then ((i, STRING string), i + len + 2)
      (* If it's a line break, add an error. This string wasn't closed. *)
    else if c = '\n' then (
      error errs (StringNotClosed i);
      ((i, STRING string), i + len + 2))
    (* If it's not either of these, why did string_len terminate????? *)
      else failwith "lex_string: string_len failed" [@coverage off]

let lex_char str i errs : ptoken * int =
  Obj.magic ()

(** [lex_name str i] lexes the current position in [str] as a name or keyword.
*)
let lex_name str i : ptoken * int =
  (* If it's a known keyword, use the specific token for it *)
  (* If it's not, use NAME *)
  let len = length_while is_variable_char str i in
  let token =
    match String.sub str i len with
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

(** [lex_octal str i errs] lexes the current position in [str] as an octal
    number. *)
let lex_octal str i errs : ptoken * int =
  (* Get the full number *)
  let slen = String.length str in
  let len = length_while is_octal str i in
  let num = String.sub str i len in
  (* Handle errors *)
  if i + len < slen && is_digit str.[i + len] then error errs (InvalidOctal i)
  else if i + len < slen && is_variable_char str.[i + len] then
    error errs (VarNumberStart i)
  else if is_octal_out_of_bounds num then warn errs (OctalOutOfRange (i, num))
  else ();
  (* Return the token *)
  ((i, NUMBER (int_of_octal num)), i + len)

(** [lex_decimal str i errs] lexes the current position in [str] as a decimal
    number. *)
let lex_decimal str i errs : ptoken * int =
  (* Get the full number *)
  let len = length_while is_digit str i in
  let num = String.sub str i len in
  (* Handle errors *)
  if i + len < String.length str && is_variable_char str.[i + len] then
    error errs (VarNumberStart i)
  else if is_decimal_out_of_bounds num then
    warn errs (DecimalOutOfRange (i, num))
  else ();
  (* Return the token *)
  ((i, NUMBER (int_of_string num)), i + len)

(** [lex_number str i errs] lexes the current position in [str] as a number. If
    the number is out of bounds, a warning is added to [errs]. If the number is
    part of a name (due to the user starting a name with numbers), then an error
    is added to [errs] *)
let lex_number str i errs : ptoken * int =
  (* If the number does not start with a 0, it must be decimal*)
  if String.unsafe_get str i <> '0' then lex_decimal str i errs
    (* If it's at the end of input, then it's definitely a zero *)
  else if i + 1 >= String.length str then ((i, NUMBER 0), i + 1)
  (* If a number starts with a zero, it has to be octal *)
    else
    let c = String.unsafe_get str (i + 1) in
    (* Checking for a variable char, so lex_octal can give a nice error for
       that *)
    if is_variable_char c then lex_octal str i errs else ((i, NUMBER 0), i + 1)

let lex str =
  let errs = new_error_state () in
  let slen = String.length str in
  let rec lex_raw i acc =
    if i >= slen then (List.rev acc, errs)
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
      (* Name *)
      | c when is_variable_start c ->
          let token, i = lex_name str i in
          lex_raw i (token :: acc)
      (* Everything else *)
      | _ -> Obj.magic ()
  in
  lex_raw 0 []
