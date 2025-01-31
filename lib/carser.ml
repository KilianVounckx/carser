(* Types *)

type 'a parse_result = ('a, string) Result.t
type 'a parser = { parse : string -> ('a * string) parse_result }
type 'a t = 'a parser

let pp_parse_result pp_ok oc = function
  | Ok (x, rest) -> Printf.fprintf oc "Ok(%a, \"%s\")" pp_ok x rest
  | Error message -> Printf.fprintf oc "Error(\"%s\")" message
;;

(* Primitive parsers *)

let fail message =
  let parse_fn _ = Error message in
  { parse = parse_fn }
;;

let const x =
  let parse_fn input = Ok (x, input) in
  { parse = parse_fn }
;;

let char expected =
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

let map f parser =
  let parse_fn input =
    let ( let+ ) = Fun.flip Result.map in
    let+ value, rest = parser.parse input in
    f value, rest
  in
  { parse = parse_fn }
;;

let ( let+ ) x f = map f x

let ( and+ ) parser1 parser2 =
  let parse_fn input =
    let ( let* ) = Result.bind in
    let* x, rest1 = parser1.parse input in
    let* y, rest2 = parser2.parse rest1 in
    Ok ((x, y), rest2)
  in
  { parse = parse_fn }
;;

let ( >> ) parser1 parser2 =
  let parse_fn input =
    let ( let* ) = Result.bind in
    let* value1, rest1 = parser1.parse input in
    let* value2, rest2 = parser2.parse rest1 in
    Ok ((value1, value2), rest2)
  in
  { parse = parse_fn }
;;

let rec sequence parsers =
  match parsers with
  | [] -> const []
  | phead :: ptail ->
    let+ head = phead
    and+ tail = sequence ptail in
    head :: tail
;;

let ( <|> ) parser1 parser2 =
  let parse_fn input =
    match parser1.parse input with
    | Ok x -> Ok x
    | Error _ -> parser2.parse input
  in
  { parse = parse_fn }
;;

let choice parsers = List.fold_left ( <|> ) (fail "no choice") parsers
let any_of chars = chars |> List.map char |> choice

let string string =
  let+ chars = string |> String.to_seq |> Seq.map char |> List.of_seq |> sequence in
  chars |> List.to_seq |> String.of_seq
;;

(* Tests *)

let pp_list pp_elem oc list =
  Printf.fprintf oc "[";
  List.iteri
    (fun index elem ->
       if index != 0 then Printf.fprintf oc ", ";
       Printf.fprintf oc "%a" pp_elem elem)
    list;
  Printf.fprintf oc "]"
;;

let pp_pair pp_left pp_right oc (x, y) = Printf.fprintf oc "(%a, %a)" pp_left x pp_right y
let pp_string oc s = Printf.fprintf oc "\"%s\"" s
let pp_char oc c = Printf.fprintf oc "'%c'" c

let%expect_test "a | success" =
  let input = "abc" in
  let parse_a = char 'a' in
  let result = parse_a.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', "bc") |}]
;;

let%expect_test "a | fail" =
  let input = "zbc" in
  let parse_a = char 'a' in
  let result = parse_a.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("expected a, got z") |}]
;;

let%expect_test "a | empty" =
  let input = "" in
  let parse_a = char 'a' in
  let result = parse_a.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("no more input") |}]
;;

let%expect_test "a then b | success" =
  let input = "abc" in
  let parser = char 'a' >> char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'b'), "c") |}]
;;

let%expect_test "a then b | fail a" =
  let input = "zbc" in
  let parser = char 'a' >> char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("expected a, got z") |}]
;;

let%expect_test "a then b | fail b" =
  let input = "azc" in
  let parser = char 'a' >> char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("expected b, got z") |}]
;;

let%expect_test "a then b | empty" =
  let input = "" in
  let parser = char 'a' >> char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("no more input") |}]
;;

let%expect_test "a or b | success a" =
  let input = "azz" in
  let parser = char 'a' <|> char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', "zz") |}]
;;

let%expect_test "a or b | success b" =
  let input = "bzz" in
  let parser = char 'a' <|> char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('b', "zz") |}]
;;

let%expect_test "a or b | fail" =
  let input = "zzz" in
  let parser = char 'a' <|> char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("expected b, got z") |}]
;;

let%expect_test "a or b | empty" =
  let input = "" in
  let parser = char 'a' <|> char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("no more input") |}]
;;

let%expect_test "a and then (b or c) | success b" =
  let input = "abz" in
  let parser = char 'a' >> (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'b'), "z") |}]
;;

let%expect_test "a and then (b or c) | success c" =
  let input = "acz" in
  let parser = char 'a' >> (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'c'), "z") |}]
;;

let%expect_test "a and then (b or c) | fail a" =
  let input = "zcz" in
  let parser = char 'a' >> (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("expected a, got z") |}]
;;

let%expect_test "a and then (b or c) | fail bc" =
  let input = "azz" in
  let parser = char 'a' >> (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("expected c, got z") |}]
;;

let%expect_test "a and then (b or c) | empty" =
  let input = "" in
  let parser = char 'a' >> (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("no more input") |}]
;;

let%expect_test "lowercase | success" =
  let input = "aBC" in
  let parser = any_of (List.of_seq (String.to_seq "abcdefghijklmnopqrstuvwxyz")) in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', "BC") |}]
;;

let%expect_test "lowercase | fail" =
  let input = "ABC" in
  let parser = any_of (List.of_seq (String.to_seq "abcdefghijklmnopqrstuvwxyz")) in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("expected z, got A") |}]
;;

let%expect_test "sequence | success" =
  let input = "abcz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'b', 'c'], "z") |}]
;;

let%expect_test "sequence | fail a" =
  let input = "zzzz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Error("expected a, got z") |}]
;;

let%expect_test "sequence | fail b" =
  let input = "azzz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Error("expected b, got z") |}]
;;

let%expect_test "sequence | fail c" =
  let input = "abzz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Error("expected c, got z") |}]
;;

let%expect_test "string | success" =
  let input = "abcz" in
  let parser = string "abc" in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_string) result;
  [%expect {| Ok("abc", "z") |}]
;;
