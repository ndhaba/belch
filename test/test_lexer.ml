open Belch
open Lexer
open OUnit2

let string_of_list f l = "[" ^ (String.concat "; " (List.map f l)) ^ "]"

let clean_lex expected actual =
  "Lex output of \"" ^ actual ^ "\"" >:: fun _ ->
    let output, errors = lex actual in
    if Error.has_error errors || Error.has_warning errors then
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
]