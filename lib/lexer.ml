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
  | NUMBER of int64
[@@deriving show]

type point_token = int * token
[@@deriving show]

(** [is_octal c] is true if the character can be used in a base 8 number *)
let is_octal c = c >= '0' && c <= '7'

(** [is_digit c] is true if the character can be used in a base 10 number *)
let is_digit c = c >= '0' && c <= '9'

(** [is_alphanumeric c] is true if [c] can be part of a variable name *)
let is_alphanumeric c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_' || is_digit c

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
  | '~' -> true
  | '%' -> true
  | '^' -> true
  | _ -> false

(* [is_operator s] is true if [s] is a valid operator *)
let is_operator =
  (* List of every single possible operator *)
  let operators = [
    "*"; "&"; "-"; "!"; "++"; "--"; "~"; "/"; "%"; "+";
    "<<"; ">>"; "<"; "<="; ">"; ">="; "=="; "!="; "^";
    "|"; "=*"; "=/"; "=%"; "=+"; "=-"; "=<<"; "=>>";
    "=<"; "=<="; "=>"; "=>="; "==="; "=!="; "=&"; "=^";
    "=|"
  ]
  (* Check if [s] is in the list *)
  in fun s -> List.mem s operators

let int_zero_char = int_of_char '0'

(** [int64_of_string_overflow radix s] parses a string as an [Int64] in base-[radix].
    Along with the final number, it returns a boolean that is [true] if the number
    overflows. Raises: [Failure] if an invalid character appears in [s] **)
let int64_of_string_overflow radix =
  let open Int64 in

  let max_div = div max_int radix in
  let max_mod = rem max_int radix in
  let min_div = div min_int radix in
  let min_mod = abs (rem min_int radix) in
  
  fun s ->
    let len = String.length s in
    let is_negative = len > 0 && s.[0] = '-' in
    let start_idx = if is_negative || (len > 0 && s.[0] = '+') then 1 else 0 in

    let rec loop i acc overflow =
      if i >= len then
        (acc, overflow)
      else
        let digit = of_int (Char.code s.[i] - int_zero_char) in
        if digit < 0L || digit >= radix then
          failwith "int64_of_string_overflow" [@coverage off]
        else if is_negative then
          let acc' = sub (mul acc radix) digit in
          let will_overflow = acc < min_div || (acc = min_div && digit > min_mod) in
          loop (i + 1) acc' (overflow || will_overflow)
        else
          let acc' = add (mul acc radix) digit in
          let will_overflow = acc > max_div || (acc = max_div && digit > max_mod) in
          loop (i + 1) acc' (overflow || will_overflow)
    in

    if start_idx >= len then (0L, true)  (* empty or sign-only string *)
    else loop start_idx 0L false

let int64_of_octal = int64_of_string_overflow 8L
let int64_of_decimal = int64_of_string_overflow 10L

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
let lex_operator str i errs : point_token * int =
  (* Get the full operator *)
  let len = length_while is_operator_char str i in
  let operator = String.sub str i len in
  (* If the operator is unknown, add an error but continue execution *)
  if is_operator operator then
    ()
  else
    error errs i (UnknownOperator operator);
  (* Return the token *)
  ((i, OPERATOR operator), i + len)

(* A type representing the state of [lex_text_data] *)
type text_state = Valid | NotClosed | Incomplete

(** [lex_text_data str i errs closure] lexes the current position in [str] as
    a string-like token, with the given [closure] character surrounding it *)
let lex_text_data str i errs closure =
  (* Initial loop variables *)
  let slen = String.length str in
  let buffer = Buffer.create 128 in
  let state = ref Incomplete in
  let escape = ref false in
  let index = ref (i + 1) in
  (* Start parsing the string *)
  while !state = Incomplete && !index < slen do
    match String.unsafe_get str !index with
    (* Escape characters *)
    | '*' when not !escape ->
      escape := true
    | '*' | '"' | '\'' as c when !escape ->
      Buffer.add_char buffer c;
      escape := false
    | '0' when !escape ->
      Buffer.add_char buffer '\x00';
      escape := false
    | '(' when !escape ->
      Buffer.add_char buffer '{';
      escape := false
    | ')' when !escape ->
      Buffer.add_char buffer '}';
      escape := false
    | 't' when !escape ->
      Buffer.add_char buffer '\t';
      escape := false
    | 'n' when !escape ->
      Buffer.add_char buffer '\n';
      escape := false
    (* Unknown escape character *)
    | c when !escape ->
      error errs (!index - 1) (UnknownEscape ("*" ^ String.make 1 c));
      Buffer.add_char buffer c;
      escape := false
    (* Terminating characters *)
    | c when c = closure ->
      state := Valid
    | '\n' ->
      state := NotClosed
    (* All other characters *)
    | c ->
      Buffer.add_char buffer c;
    (* Increment the index *)
    index := !index + 1
  done;
  (* Return buffer + state *)
  buffer, !state, !index

(** [lex_string str i errs] lexes the current position in [str] as a string. If
    the string is not completed, an error will be added to [errs] *)
let lex_string str i errs : point_token * int =
  let buffer, state, index = lex_text_data str i errs '"' in
  (* Add error if needed *)
  (match state with
  | Valid -> ()
  | _ -> error errs i StringNotClosed);
  (* Return the token *)
  (i, STRING (Buffer.contents buffer)), index

(** [lex_char str i errs] lexes the current position in [str] as a char. If
    the char is not completed, or if it's too big, an error will be added to
    [errs] *)
let lex_char str i errs : point_token * int =
  let buffer, state, index = lex_text_data str i errs '\'' in
  (* Add error if needed *)
  (match state with
  | Valid when Buffer.length buffer <= 8 -> ()
  | Valid -> error errs i CharTooBig
  | _ -> error errs i CharNotClosed);
  (* Return the token *)
  (i, CHAR (Buffer.to_bytes buffer)), index

(** [lex_number str i errs signed] lexes the current position in [str] as a
    number. If the number is malformed, or if it's out of bounds, an error
    will be added to [errs] *)
let lex_number str i errs : point_token * int =
  let len = length_while is_alphanumeric str i in
  let start = String.unsafe_get str i in
  (* Special case: 0 *)
  if start = '0' && len = 1 then
    ((i, NUMBER 0L), i + 1)
  (* Everything else *)
  else
    let sub = String.sub str i len in
    (* If this is an octal number *)
    if start = '0' then
      (* Check that every character is valid *)
      if String.for_all is_octal sub then (
        (* Parse the octal number *)
        let num, overflow = int64_of_octal sub in
        (* Check that the octal number isn't out of bounds *)
        if overflow then
          error errs i (OctalOutOfRange sub)
        else
          ();
        (* Return the token *)
        ((i, NUMBER num), i + len)
      )
      (* If there are normal decimal numbers (8, 9), tell the user its not
         octal form *)
      else if String.for_all is_digit sub then (
        error errs i InvalidOctal;
        ((i, NUMBER 0L), i + len)
      )
      (* If there are alphabet characters, tell the user you can't start a
         variable with numbers *)
      else (
        error errs i VarNumberStart;
        ((i, NAME sub), i + len)
      )
    (* If this is a decimal number *)
    else
      (* Check that all of the characters are valid *)
      if String.for_all is_digit sub then (
        (* Parse the decimal number *)
        let num, overflow = int64_of_decimal sub in
        (* Check that the decimal number isn't out of bounds *)
        if overflow then
          error errs i (DecimalOutOfRange sub)
        else
          ();
        (* Return the token *)
        ((i, NUMBER num), i + len)
      )
      (* If there are alphabet characters, tell the user you can't start a
         variable with numbers *)
      else (
        error errs i VarNumberStart;
        ((i, NAME sub), i + len)
      )

(** [lex_name str i] lexes the current position in [str] as a name or
    number. *)
let lex_name str i : point_token * int =
  let len = length_while is_alphanumeric str i in
  let sub = String.sub str i len in
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
      (* Char *)
      | '\'' ->
        let token, i = lex_char str i errs in
        lex_raw i (token :: acc)
      (* Numbers *)
      | c when is_digit c ->
        let token, i = lex_number str i errs in
        lex_raw i (token :: acc)
      (* Names *)
      | c when is_alphanumeric c ->
        let token, i = lex_name str i in
        lex_raw i (token :: acc)
      (* Operator *)
      | c when is_operator_char c ->
        let token, i = lex_operator str i errs in
        lex_raw i (token :: acc)
      (* Everything else *)
      | c ->
        error errs i (UnknownToken c);
        lex_raw (i + 1) acc
  in
  lex_raw 0 []