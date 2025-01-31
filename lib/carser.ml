(* Types *)

type 'a parse_result = ('a, string) Result.t
type 'a parser = { parse : string -> ('a * string) parse_result }
type 'a t = 'a parser

let pp_parse_result pp_ok oc = function
  | Ok (x, rest) -> Printf.fprintf oc "Ok(%a, \"%s\")" pp_ok x rest
  | Error message -> Printf.fprintf oc "Error(\"%s\")" message
;;

(* Primitive parsers *)

let pchar expected =
  let parse_fn input =
    let length = String.length input in
    if length == 0
    then Error "no more input"
    else if String.get input 0 == expected
    then Ok (String.get input 0, String.sub input 1 (length - 1))
    else Error (Printf.sprintf "expected %c, got %c" expected (String.get input 0))
  in
  { parse = parse_fn }
;;

(* Parser combinators *)

let ( >> ) parser1 parser2 =
  let parse_fn input =
    let ( let* ) = Result.bind in
    let* value1, rest1 = parser1.parse input in
    let* value2, rest2 = parser2.parse rest1 in
    Ok ((value1, value2), rest2)
  in
  { parse = parse_fn }
;;

let ( <|> ) parser1 parser2 =
  let parse_fn input =
    match parser1.parse input with
    | Ok x -> Ok x
    | Error _ -> parser2.parse input
  in
  { parse = parse_fn }
;;

(* Tests *)

let pp_pair pp_left pp_right oc (x, y) = Printf.fprintf oc "(%a, %a)" pp_left x pp_right y
let pp_char oc c = Printf.fprintf oc "'%c'" c

let%expect_test "a | success" =
  let input = "abc" in
  let parse_a = pchar 'a' in
  let result = parse_a.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', "bc") |}]
;;

let%expect_test "a | fail" =
  let input = "zbc" in
  let parse_a = pchar 'a' in
  let result = parse_a.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("expected a, got z") |}]
;;

let%expect_test "a | empty" =
  let input = "" in
  let parse_a = pchar 'a' in
  let result = parse_a.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("no more input") |}]
;;

let%expect_test "a then b | success" =
  let input = "abc" in
  let parser = pchar 'a' >> pchar 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'b'), "c") |}]
;;

let%expect_test "a then b | fail a" =
  let input = "zbc" in
  let parser = pchar 'a' >> pchar 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("expected a, got z") |}]
;;

let%expect_test "a then b | fail b" =
  let input = "azc" in
  let parser = pchar 'a' >> pchar 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("expected b, got z") |}]
;;

let%expect_test "a then b | empty" =
  let input = "" in
  let parser = pchar 'a' >> pchar 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("no more input") |}]
;;

let%expect_test "a or b | success a" =
  let input = "azz" in
  let parser = pchar 'a' <|> pchar 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', "zz") |}]
;;

let%expect_test "a or b | success b" =
  let input = "bzz" in
  let parser = pchar 'a' <|> pchar 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('b', "zz") |}]
;;

let%expect_test "a or b | fail" =
  let input = "zzz" in
  let parser = pchar 'a' <|> pchar 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("expected b, got z") |}]
;;

let%expect_test "a or b | empty" =
  let input = "" in
  let parser = pchar 'a' <|> pchar 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("no more input") |}]
;;
