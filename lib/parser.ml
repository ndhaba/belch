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

and parse_unary_expression tokens state = 
  match tokens with
    (* Indirection *)
    | (_, OPERATOR "*") :: tokens ->
      let inner, tokens = parse_unary_expression tokens state in
      Indirection (inner), tokens
    (* Address *)
    | (_, OPERATOR "&") :: tokens ->
      let inner, tokens = parse_unary_expression tokens state in
      Address (inner), tokens
    (* Negate *)
    | (_, OPERATOR "-") :: tokens ->
      let inner, tokens = parse_unary_expression tokens state in
      Negate (inner), tokens
    (* Logical NOT *)
    | (_, OPERATOR "!") :: tokens ->
      let inner, tokens = parse_unary_expression tokens state in
      LogicalNot (inner), tokens
    (* Increment *)
    | (_, OPERATOR "++") :: tokens ->
      let inner, tokens = parse_unary_expression tokens state in
      Increment (inner, Prefix), tokens
    (* Decrement *)
    | (_, OPERATOR "--") :: tokens ->
      let inner, tokens = parse_unary_expression tokens state in
      Decrement (inner, Prefix), tokens
    (* Bitwise NOT *)
    | (_, OPERATOR "~") :: tokens ->
      let inner, tokens = parse_unary_expression tokens state in
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

and parse_value tokens state = Obj.magic ()