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

let ( *>>* ) parser1 parser2 =
  let parse_fn input =
    let ( let* ) = Result.bind in
    let* value1, rest1 = parser1.parse input in
    let* value2, rest2 = parser2.parse rest1 in
    Ok ((value1, value2), rest2)
  in
  { parse = parse_fn }
;;

let ( and+ ) = ( *>>* )

let ( *>> ) parser1 parser2 =
  let+ value = parser1
  and+ _ = parser2 in
  value
;;

let ( >>* ) parser1 parser2 =
  let+ _ = parser1
  and+ value = parser2 in
  value
;;

let between left right parser = left >>* parser *>> right

let bind parser_x fn =
  let parse_fn input =
    let ( let* ) = Result.bind in
    let* x, rest1 = parser_x.parse input in
    (fn x).parse rest1
  in
  { parse = parse_fn }
;;

let ( >>= ) = bind
let ( let* ) = bind

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

let many parser =
  let rec helper parser input =
    match parser.parse input with
    | Error _ -> [], input
    | Ok (x, rest1) ->
      let xs, rest2 = helper parser rest1 in
      x :: xs, rest2
  in
  let parse_fn input = Ok (helper parser input) in
  { parse = parse_fn }
;;

let some parser =
  let+ first = parser
  and+ rest = many parser in
  first :: rest
;;

let opt parser =
  let some = map Option.some parser in
  let none = const Option.none in
  some <|> none
;;

let sep_by_1 separator parser =
  let sep_then_p = separator >>* parser in
  let+ head = parser
  and+ tail = many sep_then_p in
  head :: tail
;;

let sep_by separator parser = sep_by_1 separator parser <|> const []

(* Combined parsers *)

let string string =
  let+ chars = string |> String.to_seq |> Seq.map char |> List.of_seq |> sequence in
  chars |> List.to_seq |> String.of_seq
;;

let uint =
  let digit = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] |> any_of in
  let+ digits = some digit in
  digits |> List.to_seq |> String.of_seq |> int_of_string
;;

let int =
  let+ sign =
    map
      (Option.value ~default:1)
      (opt (map (Fun.const (-1)) (char '-') <|> map (Fun.const 1) (char '+')))
  and+ abs = uint in
  sign * abs
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

let pp_option pp_elem oc = function
  | Some x -> Printf.fprintf oc "Some(%a)" pp_elem x
  | None -> Printf.fprintf oc "None"
;;

let pp_pair pp_left pp_right oc (x, y) = Printf.fprintf oc "(%a, %a)" pp_left x pp_right y
let pp_string oc s = Printf.fprintf oc "\"%s\"" s
let pp_int oc n = Printf.fprintf oc "%d" n
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
  let parser = char 'a' *>>* char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'b'), "c") |}]
;;

let%expect_test "a then b | fail a" =
  let input = "zbc" in
  let parser = char 'a' *>>* char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("expected a, got z") |}]
;;

let%expect_test "a then b | fail b" =
  let input = "azc" in
  let parser = char 'a' *>>* char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("expected b, got z") |}]
;;

let%expect_test "a then b | empty" =
  let input = "" in
  let parser = char 'a' *>>* char 'b' in
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
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'b'), "z") |}]
;;

let%expect_test "a and then (b or c) | success c" =
  let input = "acz" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'c'), "z") |}]
;;

let%expect_test "a and then (b or c) | fail a" =
  let input = "zcz" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("expected a, got z") |}]
;;

let%expect_test "a and then (b or c) | fail bc" =
  let input = "azz" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("expected c, got z") |}]
;;

let%expect_test "a and then (b or c) | empty" =
  let input = "" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
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

let%expect_test "many a | success 3" =
  let input = "aaaz" in
  let parser = many (char 'a') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a', 'a'], "z") |}]
;;

let%expect_test "many a | success 2" =
  let input = "aazz" in
  let parser = many (char 'a') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a'], "zz") |}]
;;

let%expect_test "many a | success 1" =
  let input = "azzz" in
  let parser = many (char 'a') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a'], "zzz") |}]
;;

let%expect_test "many a | success 0" =
  let input = "zzzz" in
  let parser = many (char 'a') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok([], "zzzz") |}]
;;

let%expect_test "some a | success 3" =
  let input = "aaaz" in
  let parser = some (char 'a') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a', 'a'], "z") |}]
;;

let%expect_test "some a | success 2" =
  let input = "aazz" in
  let parser = some (char 'a') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a'], "zz") |}]
;;

let%expect_test "some a | success 1" =
  let input = "azzz" in
  let parser = some (char 'a') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a'], "zzz") |}]
;;

let%expect_test "some a | fail" =
  let input = "zzzz" in
  let parser = some (char 'a') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Error("expected a, got z") |}]
;;

let%expect_test "int | success 1" =
  let input = "1abc" in
  let parser = uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(1, "abc") |}]
;;

let%expect_test "int | success 12" =
  let input = "12bc" in
  let parser = uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(12, "bc") |}]
;;

let%expect_test "int | success 123" =
  let input = "123c" in
  let parser = uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, "c") |}]
;;

let%expect_test "int | success 1234" =
  let input = "1234" in
  let parser = uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(1234, "") |}]
;;

let%expect_test "int | fail" =
  let input = "abc" in
  let parser = uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Error("expected 9, got a") |}]
;;

let%expect_test "opt | success some" =
  let input = "123;" in
  let parser = uint *>>* opt (char ';') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_int (pp_option pp_char))) result;
  [%expect {| Ok((123, Some(';')), "") |}]
;;

let%expect_test "opt | success none" =
  let input = "123" in
  let parser = uint *>>* opt (char ';') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_int (pp_option pp_char))) result;
  [%expect {| Ok((123, None), "") |}]
;;

let%expect_test "int | success positive" =
  let input = "123z" in
  let parser = int in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, "z") |}]
;;

let%expect_test "int | success negative" =
  let input = "-123z" in
  let parser = int in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(-123, "z") |}]
;;

let%expect_test "int | fail positive" =
  let input = "abcz" in
  let parser = int in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Error("expected 9, got a") |}]
;;

let%expect_test "int | fail negative" =
  let input = "-abcz" in
  let parser = int in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Error("expected 9, got a") |}]
;;

let%expect_test "ignore_right | success" =
  let input = "123;" in
  let parser = int *>> char ';' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, "") |}]
;;

let%expect_test "ignore_left | success" =
  let input = ";123" in
  let parser = char ';' >>* int in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, "") |}]
;;

let%expect_test "between | success" =
  let input = "(123)" in
  let parser = between (char '(') (char ')') uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, "") |}]
;;

let%expect_test "sep_by_1 | success 3" =
  let input = "1,2,3;" in
  let parser = sep_by_1 (char ',') uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2, 3], ";") |}]
;;

let%expect_test "sep_by_1 | success 2" =
  let input = "1,2;" in
  let parser = sep_by_1 (char ',') uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2], ";") |}]
;;

let%expect_test "sep_by_1 | success 1" =
  let input = "1,;" in
  let parser = sep_by_1 (char ',') uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1], ",;") |}]
;;

let%expect_test "sep_by_1 | fail" =
  let input = ";" in
  let parser = sep_by_1 (char ',') uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Error("expected 9, got ;") |}]
;;

let%expect_test "sep_by | success 3" =
  let input = "1,2,3;" in
  let parser = sep_by (char ',') uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2, 3], ";") |}]
;;

let%expect_test "sep_by | success 2" =
  let input = "1,2;" in
  let parser = sep_by (char ',') uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2], ";") |}]
;;

let%expect_test "sep_by | success 1" =
  let input = "1,;" in
  let parser = sep_by (char ',') uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1], ",;") |}]
;;

let%expect_test "sep_by | success 0" =
  let input = ";" in
  let parser = sep_by (char ',') uint in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([], ";") |}]
;;

let%expect_test "bind" =
  let input = "{name:frodo,age:50}" in
  let parser =
    let p_name =
      let+ chars =
        "abcdefghijklmnopqrstuvwxyz" |> String.to_seq |> List.of_seq |> any_of |> some
      in
      chars |> List.to_seq |> String.of_seq
    in
    let* _ = string "{name:" in
    let* name = p_name in
    let* _ = string ",age:" in
    let* age = uint in
    let* _ = char '}' in
    const (name, age)
  in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_string pp_int)) result;
  [%expect {| Ok(("frodo", 50), "") |}]
;;
