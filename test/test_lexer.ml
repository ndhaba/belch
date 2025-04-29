open Belch
open Lexer
open OUnit2

let string_of_list f l = "[" ^ (String.concat "; " (List.map f l)) ^ "]"

let clean_lex expected actual =
  "Lex output of \"" ^ actual ^ "\"" >:: fun _ ->
    let output, errors = lex actual in
    if Error.has_error errors then
      assert_failure "Warning/error was raised"
    else
      assert_equal expected output ~printer:(string_of_list show_point_token)

let tests = "Lexer" >::: [
  (* Symbols and whitespace *)
  clean_lex [(0, LBRACE); (2, RBRACE)] "{ }";
  clean_lex [(0, LBRACKET); (2, RBRACKET)] "[\t]";
  clean_lex [(0, LPAREN); (3, RPAREN)] "(\r\n)";
  clean_lex [(1, COLON); (2, SEMICOLON)] " :;";
  clean_lex [(0, COMMA); (4, QUESTION)] ",   ?";
  (* Keywords *)
  clean_lex [(0, IF); (3, ELSE)] "if else";
  clean_lex [(0, WHILE); (6, GOTO); (11, BREAK)] "while goto break";
  clean_lex [(0, SWITCH); (7, CASE); (12, DEFAULT)] "switch case default";
  clean_lex [(0, GOTO); (5, RETURN)] "goto return";
  clean_lex [(0, EXTRN); (6, AUTO)] "extrn auto";
  (* Names *)
  clean_lex [(0, GOTO); (5, NAME "madethisup")] "goto madethisup";
  clean_lex [(0, NAME "ifelse")] "ifelse";
  clean_lex [(0, NAME "extrn_auto")] "extrn_auto"
]