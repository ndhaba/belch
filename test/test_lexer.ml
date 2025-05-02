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

let string_lex expected actual =
  "String lex output of \"" ^ actual ^ "\"" >:: fun _ ->
    let o1, e1 = lex ("\"" ^ actual ^ "\"") in
    let o2, e2 = lex ("'" ^ actual ^ "'") in
    if Error.has_error e1 || (Error.has_error e2 && Error.errors e2 <> [(0, CharTooBig)]) then
      assert_failure "Lex failed"
    else
      assert_equal [(0, STRING expected)] o1 ~printer:(string_of_list show_point_token);
      assert_equal [(0, CHAR (Bytes.of_string expected))] o2 ~printer:(string_of_list show_point_token)

let op l n = (l, Lexer.OPERATOR n)

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
  clean_lex [(0, NUMBER 24L)] "24";
  clean_lex [(0, NUMBER (Int64.max_int))] "9223372036854775807";
  lex_error [(0, DecimalOutOfRange "9223372036854775808")] "9223372036854775808";
  clean_lex [(0, NUMBER 15L)] "017";
  clean_lex [(0, NUMBER 342391L)] "01234567";
  clean_lex [(0, NUMBER 17L)] "00000000000000000000000000000000021";
  clean_lex [(0, NUMBER (Int64.max_int))] "0777777777777777777777";
  lex_error [(0, OctalOutOfRange "01000000000000000000000")] "01000000000000000000000";
  lex_error [(0, InvalidOctal)] "018";
  lex_error [(0, InvalidOctal)] "0139";
  (* Operators *)
  clean_lex [op 0 "+"; op 2 "-"; op 4 "|"] "+ - |";
  clean_lex [op 0 "++"; op 3 "--"] "++ --";
  clean_lex [op 0 "*"; op 2 "/"; op 4 "^"] "* / ^";
  clean_lex [op 0 "&"; op 2 "!"; op 4 "~"] "& ! ~";
  clean_lex [op 0 "%"; op 2 "<"; op 4 ">"] "% < >";
  clean_lex [op 0 "<="; op 3 ">="] "<= >=";
  clean_lex [op 0 "<<"; op 3 ">>"; op 6 "=<<"; op 10 "=>>"] "<< >> =<< =>>";
  clean_lex [op 0 "=="; op 3 "!="; op 6 "=<="] "== != =<=";
  clean_lex [op 0 "=+"; op 3 "=-"; op 6 "=>="] "=+ =- =>=";
  clean_lex [op 0 "=*"; op 3 "=/"; op 6 "=!="] "=* =/ =!=";
  clean_lex [op 0 "=%"; op 3 "=|"; op 6 "==="] "=% =| ===";
  clean_lex [op 0 "=^"; op 3 "=<"; op 6 "=>"] "=^ =< =>";
  lex_error [(2, UnknownOperator "+--")] "x +-- 1";
  (* String/char syntax *)
  clean_lex [(0, STRING "Heyo")] "\"Heyo\"";
  clean_lex [(0, STRING "Single 'quotes'")] "\"Single 'quotes'\"";
  lex_error [(1, StringNotClosed)] " \"I NEED MORE";
  lex_error [(0, StringNotClosed); (10, StringNotClosed)] "\"FAIL\nFAIL\"";
  clean_lex [(0, CHAR (Bytes.of_string "\""))] "'\"'";
  lex_error [(2, CharTooBig)] "  'aaaaaaaaa'  ";
  lex_error [(3, CharNotClosed)] "   'I NEED MORE";
  lex_error [(1, CharNotClosed); (12, CharNotClosed)] " 'Line\nBreak'";
  (* String/char escaping *)
  string_lex "A\x00B" "A*0B";
  string_lex "{Brace}" "*(Brace*)";
  string_lex "One\tTwo" "One*tTwo";
  string_lex "Star*Power" "Star**Power";
  string_lex "It's Fine" "It*'s Fine";
  string_lex "Say \"Hello\"" "Say *\"Hello*\"";
  string_lex "Line 1\nLine 2" "Line 1*nLine 2";
  string_lex "End here -> \x04" "End here -> *e";
  lex_error [(5, UnknownEscape "*l")] "'Esc *l'";
  (* Unknown characters *)
  lex_error [(2, UnknownToken '`')] "++`";
  (* Comments *)
  clean_lex [] "/** Dude \n COMMENTS HAVE NO TOKENS */";
  clean_lex [(0, NUMBER 1L); (10, NUMBER 3L)] "1 /* 2 */ 3";
  lex_error [(8, CommentNotClosed)] "auto x; /** Unclosed :("
]