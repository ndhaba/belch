open Error
open Ast
open Lexer

let parse_constant tokens =
  match tokens with
    | (_, NUMBER n) :: t -> Some (n, t)
    | _ -> None

let rec parse_primary_value_inner tokens state =
  match tokens with
    (* Parentheses *)
    | (i, LPAREN) :: tokens -> (
      let inner, tokens = parse_value tokens state in
      match tokens with
        (* Closed as expected *)
        | (_, RPAREN) :: tokens -> inner, tokens
        (* Unclosed *)
        | tokens -> (
          error state i UnclosedParentheses;
          inner, tokens
        )
    )
    (* Other Constants *)
    | (_, NAME name) :: tokens -> Name name, tokens
    | (_, CHAR char) :: tokens -> Char char, tokens
    | (_, NUMBER num) :: tokens -> Constant num, tokens
    | (_, STRING str) :: tokens -> String str, tokens
    (* Unknown *)
    | (i, _) :: tokens -> (
      error state i ExpectedPrimaryValue;
      Name "", tokens
    )
    | [] -> (
      error state Int.max_int ExpectedPrimaryValue;
      Name "", tokens
    )

and parse_primary_value tokens state =
  (* Loop for collecting function call arguments *)
  let rec fn_arg_loop tokens acc =
    let arg, tokens = parse_value tokens state in
    match tokens with
      (* More arguments *)
      | (_, COMMA) :: tokens ->
        fn_arg_loop tokens (arg :: acc)
      (* No more *)
      | tokens -> (List.rev (arg :: acc)), tokens
  (* Main loop for parsing vector accesses and function calls *)
  in let rec main_loop (left, tokens) =
    match tokens with
      (* Vector access *)
      | (i, LBRACKET) :: tokens -> (
        let right, tokens = parse_value tokens state in
        match tokens with
          (* Closed as expected *)
          | (_, RBRACKET) :: tokens ->
            main_loop (VectorAccess (left, right), tokens)
          (* Not closed *)
          | tokens -> (
            error state i UnclosedBracket;
            main_loop (VectorAccess (left, right), tokens)
          )
      )
      (* Function call *)
      | (_, LPAREN) :: (_, RPAREN) :: tokens ->
        main_loop (FunctionCall (left, []), tokens)
      | (i, LPAREN) :: tokens -> (
        let args, tokens = fn_arg_loop tokens [] in
        match tokens with
          (* Closed as expected *)
          (_, RPAREN) :: tokens ->
            main_loop (FunctionCall (left, args), tokens)
          (* Not closed *)
          | tokens -> (
            error state i UnclosedParentheses;
            main_loop (FunctionCall (left, args), tokens)
          )
      )
      (* Anything else *)
      | tokens -> left, tokens
  (* Do the thing *)
  in main_loop (parse_primary_value_inner tokens state)

and parse_unary_expr tokens state = 
  match tokens with
    (* Indirection *)
    | (_, OPERATOR "*") :: tokens ->
      let inner, tokens = parse_unary_expr tokens state in
      Indirection (inner), tokens
    (* Address *)
    | (_, OPERATOR "&") :: tokens ->
      let inner, tokens = parse_unary_expr tokens state in
      Address (inner), tokens
    (* Negate *)
    | (_, OPERATOR "-") :: tokens ->
      let inner, tokens = parse_unary_expr tokens state in
      Negate (inner), tokens
    (* Logical NOT *)
    | (_, OPERATOR "!") :: tokens ->
      let inner, tokens = parse_unary_expr tokens state in
      LogicalNot (inner), tokens
    (* Increment *)
    | (_, OPERATOR "++") :: tokens ->
      let inner, tokens = parse_unary_expr tokens state in
      Increment (inner, Prefix), tokens
    (* Decrement *)
    | (_, OPERATOR "--") :: tokens ->
      let inner, tokens = parse_unary_expr tokens state in
      Decrement (inner, Prefix), tokens
    (* Bitwise NOT *)
    | (_, OPERATOR "~") :: tokens ->
      let inner, tokens = parse_unary_expr tokens state in
      BitwiseNot (inner), tokens
    (* Postfixes *)
    | tokens ->
      let inner, tokens = parse_primary_value tokens state in
      match tokens with
        (* Increment *)
        | (_, OPERATOR "++") :: tokens ->
          Increment (inner, Postfix), tokens
        (* Decrement *)
        | (_, OPERATOR "--") :: tokens ->
          Decrement (inner, Postfix), tokens
        (* Anything else *)
        | tokens -> inner, tokens

and parse_binary_expr_mult tokens state =
  let rec loop left tokens =
    match tokens with
      (* Multiply *)
      | (_, OPERATOR "*") :: tokens ->
        let right, tokens = parse_unary_expr tokens state in
        loop (BinaryOperation (left, Multiply, right)) tokens
      (* Divide *)
      | (_, OPERATOR "/") :: tokens ->
        let right, tokens = parse_unary_expr tokens state in
        loop (BinaryOperation (left, Divide, right)) tokens
      (* Modulo *)
      | (_, OPERATOR "%") :: tokens ->
        let right, tokens = parse_unary_expr tokens state in
        loop (BinaryOperation (left, Modulo, right)) tokens
      (* Anything else*)
      | tokens -> left, tokens in
  (* Start loop *)
  let left, tokens = parse_unary_expr tokens state in
  loop left tokens

and parse_binary_expr_add tokens state =
  let rec loop left tokens =
    match tokens with
      (* Add *)
      | (_, OPERATOR "+") :: tokens ->
        let right, tokens = parse_binary_expr_mult tokens state in
        loop (BinaryOperation (left, Add, right)) tokens
      (* Subtract *)
      | (_, OPERATOR "-") :: tokens ->
        let right, tokens = parse_binary_expr_mult tokens state in
        loop (BinaryOperation (left, Subtract, right)) tokens
      (* Anything else*)
      | tokens -> left, tokens in
  (* Start loop *)
  let left, tokens = parse_binary_expr_mult tokens state in
  loop left tokens

and parse_binary_expr_shift tokens state =
  let rec loop left tokens =
    match tokens with
      (* Left Shift *)
      | (_, OPERATOR "<<") :: tokens ->
        let right, tokens = parse_binary_expr_add tokens state in
        loop (BinaryOperation (left, LeftShift, right)) tokens
      (* Right Shift *)
      | (_, OPERATOR ">>") :: tokens ->
        let right, tokens = parse_binary_expr_add tokens state in
        loop (BinaryOperation (left, RightShift, right)) tokens
      (* Anything else*)
      | tokens -> left, tokens in
  (* Start loop *)
  let left, tokens = parse_binary_expr_add tokens state in
  loop left tokens

and parse_binary_expr_rel tokens state =
  let rec loop left tokens =
    match tokens with
      (* Less Than *)
      | (_, OPERATOR "<") :: tokens ->
        let right, tokens = parse_binary_expr_shift tokens state in
        loop (BinaryOperation (left, LessThan, right)) tokens
      (* Less Than or Equal *)
      | (_, OPERATOR "<=") :: tokens ->
        let right, tokens = parse_binary_expr_shift tokens state in
        loop (BinaryOperation (left, LessEqual, right)) tokens
      (* Greater Than *)
      | (_, OPERATOR ">") :: tokens ->
        let right, tokens = parse_binary_expr_shift tokens state in
        loop (BinaryOperation (left, GreaterThan, right)) tokens
      (* Greater Than or Equal *)
      | (_, OPERATOR ">=") :: tokens ->
        let right, tokens = parse_binary_expr_shift tokens state in
        loop (BinaryOperation (left, GreaterEqual, right)) tokens
      (* Anything else*)
      | tokens -> left, tokens in
  (* Start loop *)
  let left, tokens = parse_binary_expr_shift tokens state in
  loop left tokens

and parse_binary_expr_eq tokens state =
  let rec loop left tokens =
    match tokens with
      (* Equal *)
      | (_, OPERATOR "==") :: tokens ->
        let right, tokens = parse_binary_expr_rel tokens state in
        loop (BinaryOperation (left, Equal, right)) tokens
      (* Not Equal *)
      | (_, OPERATOR "!=") :: tokens ->
        let right, tokens = parse_binary_expr_rel tokens state in
        loop (BinaryOperation (left, NotEqual, right)) tokens
      (* Anything else*)
      | tokens -> left, tokens in
  (* Start loop *)
  let left, tokens = parse_binary_expr_rel tokens state in
  loop left tokens

and parse_binary_expr_and tokens state =
  let rec loop left tokens =
    match tokens with
      (* AND *)
      | (_, OPERATOR "&") :: tokens ->
        let right, tokens = parse_binary_expr_eq tokens state in
        loop (BinaryOperation (left, BitwiseAND, right)) tokens
      (* Anything else*)
      | tokens -> left, tokens in
  (* Start loop *)
  let left, tokens = parse_binary_expr_eq tokens state in
  loop left tokens

and parse_binary_expr_xor tokens state =
  let rec loop left tokens =
    match tokens with
      (* XOR *)
      | (_, OPERATOR "^") :: tokens ->
        let right, tokens = parse_binary_expr_and tokens state in
        loop (BinaryOperation (left, BitwiseXOR, right)) tokens
      (* Anything else*)
      | tokens -> left, tokens in
  (* Start loop *)
  let left, tokens = parse_binary_expr_and tokens state in
  loop left tokens

and parse_binary_expr tokens state =
  let rec loop left tokens =
    match tokens with
      (* OR *)
      | (_, OPERATOR "|") :: tokens ->
        let right, tokens = parse_binary_expr_xor tokens state in
        loop (BinaryOperation (left, BitwiseOR, right)) tokens
      (* Anything else*)
      | tokens -> left, tokens in
  (* Start loop *)
  let left, tokens = parse_binary_expr_xor tokens state in
  loop left tokens

and parse_ternary_expr tokens state =
  let left, tokens = parse_binary_expr tokens state in
  match tokens with
    (* The most important symbol is the question mark *)
    | (_, QUESTION) :: tokens -> (
      (* Parse the first result *)
      let middle, tokens = parse_binary_expr tokens state in
      let tokens = match tokens with
        (* We expect a colon to be there *)
        | (_, COLON) :: tokens -> tokens
        (* If the next token isn't a colon *)
        | (i, _) :: tokens -> (
          error state i MissingColon;
          tokens
        )
        (* If there is no next colon *)
        | [] -> (
          error state Int.max_int MissingColon;
          tokens
        )
      (* Parse the second result *)
      in let right, tokens = parse_ternary_expr tokens state in
      Ternary (left, middle, right), tokens
    )
    (* For any other symbol, it's not a ternary *)
    | tokens -> left, tokens

and parse_value tokens state =
  (* Parse the left side *)
  let left, tokens = parse_ternary_expr tokens state in
  (* Try to find the operation being represented *)
  let operator, tokens = match tokens with
    | (_, OPERATOR "=") :: tokens -> Some (None), tokens
    | (_, OPERATOR "=+") :: tokens -> Some (Some Add), tokens
    | (_, OPERATOR "=-") :: tokens -> Some (Some Subtract), tokens
    | (_, OPERATOR "=*") :: tokens -> Some (Some Multiply), tokens
    | (_, OPERATOR "=/") :: tokens -> Some (Some Divide), tokens
    | (_, OPERATOR "=%") :: tokens -> Some (Some Modulo), tokens
    | (_, OPERATOR "=<<") :: tokens -> Some (Some LeftShift), tokens
    | (_, OPERATOR "=>>") :: tokens -> Some (Some RightShift), tokens
    | (_, OPERATOR "=<") :: tokens -> Some (Some LessThan), tokens
    | (_, OPERATOR "=<=") :: tokens -> Some (Some LessEqual), tokens
    | (_, OPERATOR "=>") :: tokens -> Some (Some GreaterThan), tokens
    | (_, OPERATOR "=>=") :: tokens -> Some (Some GreaterEqual), tokens
    | (_, OPERATOR "===") :: tokens -> Some (Some Equal), tokens
    | (_, OPERATOR "=!=") :: tokens -> Some (Some NotEqual), tokens
    | (_, OPERATOR "=&") :: tokens -> Some (Some BitwiseAND), tokens
    | (_, OPERATOR "=^") :: tokens -> Some (Some BitwiseXOR), tokens
    | (_, OPERATOR "=|") :: tokens -> Some (Some BitwiseOR), tokens
    | tokens -> None, tokens in
  (* Handle the result *)
  match operator with
    | None -> left, tokens
    | Some op ->
      let right, tokens = parse_value tokens state in
      AssignmentOperation (left, op, right), tokens
