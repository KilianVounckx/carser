(* Types *)

type parser_label = string
type parser_error = string
type 'a parse_result = ('a, parser_label * parser_error) Result.t

type 'a parser =
  { parse : string -> ('a * string) parse_result
  ; label : parser_label
  }

type 'a t = 'a parser

let pp_parse_result pp_ok oc = function
  | Ok (x, rest) -> Printf.fprintf oc "Ok(%a, \"%s\")" pp_ok x rest
  | Error (label, message) -> Printf.fprintf oc "Error(\"%s\", \"%s\")" label message
;;

let run parser = parser.parse

(* Primitive parsers *)

let fail message =
  let parse_fn _ = Error (message, message) in
  { parse = parse_fn; label = message }
;;

let const x =
  let parse_fn input = Ok (x, input) in
  { parse = parse_fn; label = "<const>" }
;;

let satisfy predicate label =
  let parse_fn input =
    let length = String.length input in
    if length == 0
    then Error (label, "no more input")
    else if predicate (String.get input 0)
    then Ok (String.get input 0, String.sub input 1 (length - 1))
    else Error (label, Printf.sprintf "unexpected '%c'" (String.get input 0))
  in
  { parse = parse_fn; label }
;;

(* Parser combinators *)

let with_label label parser =
  let parse_fn input =
    match parser.parse input with
    | Ok x -> Ok x
    | Error (_, message) -> Error (label, message)
  in
  { parse = parse_fn; label }
;;

let ( <?> ) parser label = with_label label parser

let bind parser_x fn =
  let label = "unknown" in
  let parse_fn input =
    let ( let* ) = Result.bind in
    let* x, rest1 = parser_x.parse input in
    (fn x).parse rest1
  in
  { parse = parse_fn; label }
;;

let ( >>= ) = bind
let ( let* ) = bind

let map f parser =
  let* x = parser in
  const (f x)
;;

let ( let+ ) x f = map f x

let ( *>>* ) parser1 parser2 =
  let label = Printf.sprintf "%s and then %s" parser1.label parser2.label in
  (let* x1 = parser1 in
   let* x2 = parser2 in
   const (x1, x2))
  <?> label
;;

let ( and+ ) = ( *>>* )

let ( *>> ) parser1 parser2 =
  let+ value, _ = parser1 *>>* parser2 in
  value
;;

let ( >>* ) parser1 parser2 =
  let+ _, value = parser1 *>>* parser2 in
  value
;;

let between left right parser = left >>* parser *>> right

let rec sequence parsers =
  match parsers with
  | [] -> const [] <?> "nothing"
  | phead :: ptail ->
    let label =
      parsers |> List.map (fun parser -> parser.label) |> String.concat " and then "
    in
    (let+ head = phead
     and+ tail = sequence ptail <?> "foo" in
     head :: tail)
    <?> label
;;

let ( <|> ) parser1 parser2 =
  let label = Printf.sprintf "%s or %s" parser1.label parser2.label in
  let parse_fn input =
    match parser1.parse input with
    | Ok x -> Ok x
    | Error _ ->
      (match parser2.parse input with
       | Ok x -> Ok x
       | Error (_, message) -> Error (label, message))
  in
  { parse = parse_fn; label }
;;

let choice = function
  | [] -> raise (Invalid_argument "choice: list")
  | first :: rest -> List.fold_left ( <|> ) first rest
;;

let many parser =
  let label = Printf.sprintf "many %s" parser.label in
  let rec helper parser input =
    match parser.parse input with
    | Error _ -> [], input
    | Ok (x, rest1) ->
      let xs, rest2 = helper parser rest1 in
      x :: xs, rest2
  in
  let parse_fn input = Ok (helper parser input) in
  { parse = parse_fn; label }
;;

let some parser =
  (let+ first = parser
   and+ rest = many parser in
   first :: rest)
  <?> Printf.sprintf "some %s" parser.label
;;

let opt parser =
  let some = map Option.some parser in
  let none = const Option.none in
  some <|> none
;;

let sep_by_1 separator parser =
  let sep_then_p =
    separator >>* parser <?> Printf.sprintf "%s and then %s" separator.label parser.label
  in
  let+ head = parser
  and+ tail = many sep_then_p in
  head :: tail
;;

let sep_by separator parser = sep_by_1 separator parser <|> const []

(* Combined parsers *)

let char expected = satisfy (fun c -> c == expected) (Printf.sprintf "'%c'" expected)
let any_of chars = chars |> List.map char |> choice

let string string =
  (let+ chars = string |> String.to_seq |> Seq.map char |> List.of_seq |> sequence in
   chars |> List.to_seq |> String.of_seq)
  <?> Printf.sprintf "\"%s\"" string
;;

let uint =
  let digit = satisfy (String.contains "0123456789") "digit" in
  let+ digits = some digit in
  digits |> List.to_seq |> String.of_seq |> int_of_string
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
  [%expect {| Error("'a'", "unexpected 'z'") |}]
;;

let%expect_test "a | empty" =
  let input = "" in
  let parse_a = char 'a' in
  let result = parse_a.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("'a'", "no more input") |}]
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
  [%expect {| Error("'a' and then 'b'", "unexpected 'z'") |}]
;;

let%expect_test "a then b | fail b" =
  let input = "azc" in
  let parser = char 'a' *>>* char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("'a' and then 'b'", "unexpected 'z'") |}]
;;

let%expect_test "a then b | empty" =
  let input = "" in
  let parser = char 'a' *>>* char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("'a' and then 'b'", "no more input") |}]
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
  [%expect {| Error("'a' or 'b'", "unexpected 'z'") |}]
;;

let%expect_test "a or b | empty" =
  let input = "" in
  let parser = char 'a' <|> char 'b' in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("'a' or 'b'", "no more input") |}]
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
  [%expect {| Error("'a' and then 'b' or 'c'", "unexpected 'z'") |}]
;;

let%expect_test "a and then (b or c) | fail bc" =
  let input = "azz" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("'a' and then 'b' or 'c'", "unexpected 'z'") |}]
;;

let%expect_test "a and then (b or c) | empty" =
  let input = "" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Error("'a' and then 'b' or 'c'", "no more input") |}]
;;

let%expect_test "lowercase | success" =
  let input = "aBC" in
  let parser =
    any_of (List.of_seq (String.to_seq "abcdefghijklmnopqrstuvwxyz")) <?> "lowercase"
  in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', "BC") |}]
;;

let%expect_test "lowercase | fail" =
  let input = "ABC" in
  let parser =
    any_of (List.of_seq (String.to_seq "abcdefghijklmnopqrstuvwxyz")) <?> "lowercase"
  in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("lowercase", "unexpected 'A'") |}]
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
  [%expect {| Error("'a' and then 'b' and then 'c'", "unexpected 'z'") |}]
;;

let%expect_test "sequence | fail b" =
  let input = "azzz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Error("'a' and then 'b' and then 'c'", "unexpected 'z'") |}]
;;

let%expect_test "sequence | fail c" =
  let input = "abzz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Error("'a' and then 'b' and then 'c'", "unexpected 'z'") |}]
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
  [%expect {| Error("some 'a'", "unexpected 'z'") |}]
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
  [%expect {| Error("some digit", "unexpected 'a'") |}]
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
  [%expect {| Error("int", "unexpected 'a'") |}]
;;

let%expect_test "int | fail negative" =
  let input = "-abcz" in
  let parser = int in
  let result = parser.parse input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Error("int", "unexpected 'a'") |}]
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
  [%expect {| Error("unknown and then many ',' and then unknown", "unexpected ';'") |}]
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
