open Parser

module Stream : StreamType with type t = string and type token = char = struct
  type t = string
  type token = char

  let pp_token () = Printf.sprintf "%c"

  let next_token state =
    if state.index >= Stdlib.String.length state.input
    then None
    else (
      let char = Stdlib.String.get state.input state.index in
      let index = state.index + 1 in
      let position =
        if char == '\n' then inc_line state.position else inc_column state.position
      in
      Some (char, { state with index; position }))
  ;;
end

include Make (Stream)
open Operators
open Monad_syntax

let char expected = satisfy (fun c -> c == expected) (Printf.sprintf "'%c'" expected)
let any_of chars = chars |> List.map char |> choice

let string string =
  (let+ chars =
     string |> Stdlib.String.to_seq |> Seq.map char |> List.of_seq |> sequence
   in
   chars |> List.to_seq |> Stdlib.String.of_seq)
  <?> Printf.sprintf "\"%s\"" string
;;

let uint =
  let digit = satisfy (Stdlib.String.contains "0123456789") "digit" in
  let+ digits = some digit in
  digits |> List.to_seq |> Stdlib.String.of_seq |> int_of_string
;;

let int =
  (let+ sign =
     map
       (Option.value ~default:1)
       (opt (map (Fun.const (-1)) (char '-') <|> map (Fun.const 1) (char '+')))
   and+ abs = uint in
   sign * abs)
  <?> "int"
;;

(* Tests *)

let pp_parse_result pp_ok oc result =
  match result with
  | Ok (x, rest) -> Printf.fprintf oc "Ok(%a, %a)" pp_ok x pp_position rest.position
  | Error error -> pp_error oc error
;;

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
  let result = run parse_a input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', { line = 1; column = 2 }) |}]
;;

let%expect_test "a | fail" =
  let input = "zbc" in
  let parse_a = char 'a' in
  let result = run parse_a input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing 'a'
    unexpected 'z'
    |}]
;;

let%expect_test "a | empty" =
  let input = "" in
  let parse_a = char 'a' in
  let result = run parse_a input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing 'a'
    no more input
    |}]
;;

let%expect_test "a then b | success" =
  let input = "abc" in
  let parser = char 'a' *>>* char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'b'), { line = 1; column = 3 }) |}]
;;

let%expect_test "a then b | fail a" =
  let input = "zbc" in
  let parser = char 'a' *>>* char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing 'a' and then 'b'
    unexpected 'z'
    |}]
;;

let%expect_test "a then b | fail b" =
  let input = "azc" in
  let parser = char 'a' *>>* char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect
    {|
    Line:1 Col:2 | Error parsing 'a' and then 'b'
    unexpected 'z'
    |}]
;;

let%expect_test "a then b | empty" =
  let input = "" in
  let parser = char 'a' *>>* char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing 'a' and then 'b'
    no more input
    |}]
;;

let%expect_test "a or b | success a" =
  let input = "azz" in
  let parser = char 'a' <|> char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', { line = 1; column = 2 }) |}]
;;

let%expect_test "a or b | success b" =
  let input = "bzz" in
  let parser = char 'a' <|> char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('b', { line = 1; column = 2 }) |}]
;;

let%expect_test "a or b | fail" =
  let input = "zzz" in
  let parser = char 'a' <|> char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing 'a' or 'b'
    unexpected 'z'
    |}]
;;

let%expect_test "a or b | empty" =
  let input = "" in
  let parser = char 'a' <|> char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing 'a' or 'b'
    no more input
    |}]
;;

let%expect_test "a and then (b or c) | success b" =
  let input = "abz" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'b'), { line = 1; column = 3 }) |}]
;;

let%expect_test "a and then (b or c) | success c" =
  let input = "acz" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'c'), { line = 1; column = 3 }) |}]
;;

let%expect_test "a and then (b or c) | fail a" =
  let input = "zcz" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing 'a' and then 'b' or 'c'
    unexpected 'z'
    |}]
;;

let%expect_test "a and then (b or c) | fail bc" =
  let input = "azz" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect
    {|
    Line:1 Col:2 | Error parsing 'a' and then 'b' or 'c'
    unexpected 'z'
    |}]
;;

let%expect_test "a and then (b or c) | empty" =
  let input = "" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing 'a' and then 'b' or 'c'
    no more input
    |}]
;;

let%expect_test "lowercase | success" =
  let input = "aBC" in
  let parser =
    any_of (List.of_seq (Stdlib.String.to_seq "abcdefghijklmnopqrstuvwxyz"))
    <?> "lowercase"
  in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', { line = 1; column = 2 }) |}]
;;

let%expect_test "lowercase | fail" =
  let input = "ABC" in
  let parser =
    any_of (List.of_seq (Stdlib.String.to_seq "abcdefghijklmnopqrstuvwxyz"))
    <?> "lowercase"
  in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing lowercase
    unexpected 'A'
    |}]
;;

let%expect_test "sequence | success" =
  let input = "abcz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'b', 'c'], { line = 1; column = 4 }) |}]
;;

let%expect_test "sequence | fail a" =
  let input = "zzzz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing 'a' and then 'b' and then 'c'
    unexpected 'z'
    |}]
;;

let%expect_test "sequence | fail b" =
  let input = "azzz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect
    {|
    Line:1 Col:2 | Error parsing 'a' and then 'b' and then 'c'
    unexpected 'z'
    |}]
;;

let%expect_test "sequence | fail c" =
  let input = "abzz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect
    {|
    Line:1 Col:3 | Error parsing 'a' and then 'b' and then 'c'
    unexpected 'z'
    |}]
;;

let%expect_test "string | success" =
  let input = "abcz" in
  let parser = string "abc" in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_string) result;
  [%expect {| Ok("abc", { line = 1; column = 4 }) |}]
;;

let%expect_test "many a | success 3" =
  let input = "aaaz" in
  let parser = many (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a', 'a'], { line = 1; column = 4 }) |}]
;;

let%expect_test "many a | success 2" =
  let input = "aazz" in
  let parser = many (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a'], { line = 1; column = 3 }) |}]
;;

let%expect_test "many a | success 1" =
  let input = "azzz" in
  let parser = many (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a'], { line = 1; column = 2 }) |}]
;;

let%expect_test "many a | success 0" =
  let input = "zzzz" in
  let parser = many (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok([], { line = 1; column = 1 }) |}]
;;

let%expect_test "some a | success 3" =
  let input = "aaaz" in
  let parser = some (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a', 'a'], { line = 1; column = 4 }) |}]
;;

let%expect_test "some a | success 2" =
  let input = "aazz" in
  let parser = some (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a'], { line = 1; column = 3 }) |}]
;;

let%expect_test "some a | success 1" =
  let input = "azzz" in
  let parser = some (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a'], { line = 1; column = 2 }) |}]
;;

let%expect_test "some a | fail" =
  let input = "zzzz" in
  let parser = some (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing some 'a'
    unexpected 'z'
    |}]
;;

let%expect_test "int | success 1" =
  let input = "1abc" in
  let parser = uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(1, { line = 1; column = 2 }) |}]
;;

let%expect_test "int | success 12" =
  let input = "12bc" in
  let parser = uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(12, { line = 1; column = 3 }) |}]
;;

let%expect_test "int | success 123" =
  let input = "123c" in
  let parser = uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, { line = 1; column = 4 }) |}]
;;

let%expect_test "int | success 1234" =
  let input = "1234" in
  let parser = uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(1234, { line = 1; column = 5 }) |}]
;;

let%expect_test "int | fail" =
  let input = "abc" in
  let parser = uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing some digit
    unexpected 'a'
    |}]
;;

let%expect_test "opt | success some" =
  let input = "123;" in
  let parser = uint *>>* opt (char ';') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_int (pp_option pp_char))) result;
  [%expect {| Ok((123, Some(';')), { line = 1; column = 5 }) |}]
;;

let%expect_test "opt | success none" =
  let input = "123" in
  let parser = uint *>>* opt (char ';') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_int (pp_option pp_char))) result;
  [%expect {| Ok((123, None), { line = 1; column = 4 }) |}]
;;

let%expect_test "int | success positive" =
  let input = "123z" in
  let parser = int in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, { line = 1; column = 4 }) |}]
;;

let%expect_test "int | success negative" =
  let input = "-123z" in
  let parser = int in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(-123, { line = 1; column = 5 }) |}]
;;

let%expect_test "int | fail positive" =
  let input = "abcz" in
  let parser = int in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing int
    unexpected 'a'
    |}]
;;

let%expect_test "int | fail negative" =
  let input = "-abcz" in
  let parser = int in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect
    {|
    Line:1 Col:2 | Error parsing int
    unexpected 'a'
    |}]
;;

let%expect_test "ignore_right | success" =
  let input = "123;" in
  let parser = int *>> char ';' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, { line = 1; column = 5 }) |}]
;;

let%expect_test "ignore_left | success" =
  let input = ";123" in
  let parser = char ';' >>* int in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, { line = 1; column = 5 }) |}]
;;

let%expect_test "between | success" =
  let input = "(123)" in
  let parser = between (char '(') (char ')') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, { line = 1; column = 6 }) |}]
;;

let%expect_test "sep_by_1 | success 3" =
  let input = "1,2,3;" in
  let parser = sep_by_1 (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2, 3], { line = 1; column = 6 }) |}]
;;

let%expect_test "sep_by_1 | success 2" =
  let input = "1,2;" in
  let parser = sep_by_1 (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2], { line = 1; column = 4 }) |}]
;;

let%expect_test "sep_by_1 | success 1" =
  let input = "1,;" in
  let parser = sep_by_1 (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1], { line = 1; column = 2 }) |}]
;;

let%expect_test "sep_by_1 | fail" =
  let input = ";" in
  let parser = sep_by_1 (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect
    {|
    Line:1 Col:1 | Error parsing unknown and then many ',' and then unknown
    unexpected ';'
    |}]
;;

let%expect_test "sep_by | success 3" =
  let input = "1,2,3;" in
  let parser = sep_by (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2, 3], { line = 1; column = 6 }) |}]
;;

let%expect_test "sep_by | success 2" =
  let input = "1,2;" in
  let parser = sep_by (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2], { line = 1; column = 4 }) |}]
;;

let%expect_test "sep_by | success 1" =
  let input = "1,;" in
  let parser = sep_by (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1], { line = 1; column = 2 }) |}]
;;

let%expect_test "sep_by | success 0" =
  let input = ";" in
  let parser = sep_by (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([], { line = 1; column = 1 }) |}]
;;

let%expect_test "bind" =
  let input = "{name:frodo,age:50}" in
  let parser =
    let p_name =
      let+ chars =
        "abcdefghijklmnopqrstuvwxyz"
        |> Stdlib.String.to_seq
        |> List.of_seq
        |> any_of
        |> some
      in
      chars |> List.to_seq |> Stdlib.String.of_seq
    in
    let* _ = string "{name:" in
    let* name = p_name in
    let* _ = string ",age:" in
    let* age = uint in
    let* _ = char '}' in
    const (name, age)
  in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_string pp_int)) result;
  [%expect {| Ok(("frodo", 50), { line = 1; column = 20 }) |}]
;;
