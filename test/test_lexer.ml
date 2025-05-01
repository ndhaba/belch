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

let lex_error expected actual =
  "Lex error output of \"" ^ actual ^ "\"" >:: fun _ ->
    let output, errors = lex actual in
    if Error.has_error errors then
      assert_equal expected (Error.errors errors) ~printer:(string_of_list Error.show_error)
    else
      assert_failure "No errors, lexed successfully"

let tests = "Lexer" >::: let open Error in [
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
  clean_lex [(2, NAME "extrn_auto")] "  extrn_auto";
  lex_error [(6, VarNumberStart)] "      400atbeginning";
  lex_error [(4, VarNumberStart)] "    0777777a";
  (* Numbers *)
  clean_lex [(0, NUMBER 0L)] "0";
  clean_lex [(0, NUMBER 143682759L)] "143682759 ";
  clean_lex [(3, NUMBER (-1234L))] "   -1234";
  clean_lex [(0, NUMBER 24L)] "+24";
  clean_lex [(0, NUMBER (Int64.max_int))] "9223372036854775807";
  clean_lex [(0, NUMBER (Int64.min_int))] "-9223372036854775808";
  lex_error [(0, DecimalOutOfRange "9223372036854775808")] "9223372036854775808";
  lex_error [(0, DecimalOutOfRange "-9223372036854775809")] "-9223372036854775809";
  clean_lex [(0, NUMBER 15L)] "017";
  clean_lex [(0, NUMBER 342391L)] "01234567";
  clean_lex [(0, NUMBER 17L)] "00000000000000000000000000000000021";
]