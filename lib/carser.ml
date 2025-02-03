(* Types *)

type position =
  { line : int
  ; column : int
  }

let initial_position = { line = 1; column = 1 }
let inc_line pos = { line = pos.line + 1; column = 1 }
let inc_column pos = { pos with column = pos.column + 1 }

let pp_position oc { line; column } =
  Printf.fprintf oc "{ line = %d; column = %d }" line column
;;

type 's state =
  { input : 's
  ; index : int
  ; position : position
  }

let state_from_string input = { input; index = 0; position = initial_position }

module type StreamType = sig
  type t
  type token

  val next_token : t state -> (token * t state) option
  val pp_token : unit -> token -> string
end

module StringStream : StreamType with type t = string and type token = char = struct
  type t = string
  type token = char

  let pp_token () = Printf.sprintf "%c"

  let next_token state =
    if state.index >= String.length state.input
    then None
    else (
      let char = String.get state.input state.index in
      let index = state.index + 1 in
      let position =
        if char == '\n' then inc_line state.position else inc_column state.position
      in
      Some (char, { state with index; position }))
  ;;
end

module type S = sig
  type 'a t
  type stream
  type token
  type parser_label = string
  type parser_error = string
  type 'a parse_result = ('a, parser_label * parser_error * position) Result.t

  val pp_error : out_channel -> parser_label * parser_error * position -> unit
  val run : 'a t -> stream -> ('a * stream state) parse_result
  val run_eof : 'a t -> stream -> 'a parse_result
  val fail : string -> 'a t
  val const : 'a -> 'a t
  val satisfy : (token -> bool) -> parser_label -> token t
  val eof : unit t
  val with_label : parser_label -> 'a t -> 'a t
  val ( <?> ) : 'a t -> parser_label -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( *>>* ) : 'a t -> 'b t -> ('a * 'b) t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( *>> ) : 'a t -> 'b t -> 'a t
  val ( >>* ) : 'a t -> 'b t -> 'b t
  val between : 'a t -> 'b t -> 'c t -> 'c t
  val sequence : 'a t list -> 'a list t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val choice : 'a t list -> 'a t
  val many : 'a t -> 'a list t
  val some : 'a t -> 'a list t
  val opt : 'a t -> 'a option t
  val sep_by_1 : 'b t -> 'a t -> 'a list t
  val sep_by : 'b t -> 'a t -> 'a list t
end

module Make (Stream : StreamType) :
  S with type stream = Stream.t with type token = Stream.token = struct
  type stream = Stream.t
  type token = Stream.token
  type parser_label = string
  type parser_error = string
  type 'a parse_result = ('a, parser_label * parser_error * position) Result.t

  type 'a t =
    { parse : stream state -> ('a * stream state) parse_result
    ; label : parser_label
    }

  let pp_error oc (label, message, { line; column }) =
    Printf.fprintf oc "Line:%d Col:%d | Error parsing %s\n%s\n" line column label message
  ;;

  let run parser input = parser.parse (state_from_string input)

  (* Primitive parsers *)

  let fail message =
    let parse_fn input = Error (message, message, input.position) in
    { parse = parse_fn; label = message }
  ;;

  let const x =
    let parse_fn input = Ok (x, input) in
    { parse = parse_fn; label = "<const>" }
  ;;

  let satisfy predicate label =
    let parse_fn state =
      match Stream.next_token state with
      | None -> Error (label, "no more input", state.position)
      | Some (token, next) ->
        if predicate token
        then Ok (token, next)
        else
          Error
            (label, Printf.sprintf "unexpected '%a'" Stream.pp_token token, state.position)
    in
    { parse = parse_fn; label }
  ;;

  let eof =
    let label = "eof" in
    let parse_fn state =
      match Stream.next_token state with
      | None -> Ok ((), state)
      | Some (token, _) ->
        Error
          ( label
          , Printf.sprintf "expected eof, got '%a'" Stream.pp_token token
          , state.position )
    in
    { parse = parse_fn; label }
  ;;

  (* Parser combinators *)

  let with_label label parser =
    let parse_fn input =
      match parser.parse input with
      | Ok x -> Ok x
      | Error (_, message, pos) -> Error (label, message, pos)
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
         | Error (_, message, pos) -> Error (label, message, pos))
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
      separator
      >>* parser
      <?> Printf.sprintf "%s and then %s" separator.label parser.label
    in
    let+ head = parser
    and+ tail = many sep_then_p in
    head :: tail
  ;;

  let sep_by separator parser = sep_by_1 separator parser <|> const []

  let run_eof parser input =
    let ( let+ ) = Fun.flip Result.map in
    let+ value, _ = run (parser *>> eof) input in
    value
  ;;
end

(* Combined parsers *)
module String = struct
  include Make (StringStream)

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
end

(* Tests *)

let pp_parse_result pp_ok oc result =
  match result with
  | Ok (x, rest) -> Printf.fprintf oc "Ok(%a, %a)" pp_ok x pp_position rest.position
  | Error error -> String.pp_error oc error
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
  let open String in
  let input = "abc" in
  let parse_a = char 'a' in
  let result = run parse_a input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', { line = 1; column = 2 }) |}]
;;

let%expect_test "a | fail" =
  let open String in
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
  let open String in
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
  let open String in
  let input = "abc" in
  let parser = char 'a' *>>* char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'b'), { line = 1; column = 3 }) |}]
;;

let%expect_test "a then b | fail a" =
  let open String in
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
  let open String in
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
  let open String in
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
  let open String in
  let input = "azz" in
  let parser = char 'a' <|> char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('a', { line = 1; column = 2 }) |}]
;;

let%expect_test "a or b | success b" =
  let open String in
  let input = "bzz" in
  let parser = char 'a' <|> char 'b' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok('b', { line = 1; column = 2 }) |}]
;;

let%expect_test "a or b | fail" =
  let open String in
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
  let open String in
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
  let open String in
  let input = "abz" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'b'), { line = 1; column = 3 }) |}]
;;

let%expect_test "a and then (b or c) | success c" =
  let open String in
  let input = "acz" in
  let parser = char 'a' *>>* (char 'b' <|> char 'c') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_char pp_char)) result;
  [%expect {| Ok(('a', 'c'), { line = 1; column = 3 }) |}]
;;

let%expect_test "a and then (b or c) | fail a" =
  let open String in
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
  let open String in
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
  let open String in
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
  let open String in
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
  let open String in
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
  let open String in
  let input = "abcz" in
  let parser = sequence [ char 'a'; char 'b'; char 'c' ] in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'b', 'c'], { line = 1; column = 4 }) |}]
;;

let%expect_test "sequence | fail a" =
  let open String in
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
  let open String in
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
  let open String in
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
  let open String in
  let input = "abcz" in
  let parser = string "abc" in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_string) result;
  [%expect {| Ok("abc", { line = 1; column = 4 }) |}]
;;

let%expect_test "many a | success 3" =
  let open String in
  let input = "aaaz" in
  let parser = many (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a', 'a'], { line = 1; column = 4 }) |}]
;;

let%expect_test "many a | success 2" =
  let open String in
  let input = "aazz" in
  let parser = many (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a'], { line = 1; column = 3 }) |}]
;;

let%expect_test "many a | success 1" =
  let open String in
  let input = "azzz" in
  let parser = many (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a'], { line = 1; column = 2 }) |}]
;;

let%expect_test "many a | success 0" =
  let open String in
  let input = "zzzz" in
  let parser = many (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok([], { line = 1; column = 1 }) |}]
;;

let%expect_test "some a | success 3" =
  let open String in
  let input = "aaaz" in
  let parser = some (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a', 'a'], { line = 1; column = 4 }) |}]
;;

let%expect_test "some a | success 2" =
  let open String in
  let input = "aazz" in
  let parser = some (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a', 'a'], { line = 1; column = 3 }) |}]
;;

let%expect_test "some a | success 1" =
  let open String in
  let input = "azzz" in
  let parser = some (char 'a') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_char)) result;
  [%expect {| Ok(['a'], { line = 1; column = 2 }) |}]
;;

let%expect_test "some a | fail" =
  let open String in
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
  let open String in
  let input = "1abc" in
  let parser = uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(1, { line = 1; column = 2 }) |}]
;;

let%expect_test "int | success 12" =
  let open String in
  let input = "12bc" in
  let parser = uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(12, { line = 1; column = 3 }) |}]
;;

let%expect_test "int | success 123" =
  let open String in
  let input = "123c" in
  let parser = uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, { line = 1; column = 4 }) |}]
;;

let%expect_test "int | success 1234" =
  let open String in
  let input = "1234" in
  let parser = uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(1234, { line = 1; column = 5 }) |}]
;;

let%expect_test "int | fail" =
  let open String in
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
  let open String in
  let input = "123;" in
  let parser = uint *>>* opt (char ';') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_int (pp_option pp_char))) result;
  [%expect {| Ok((123, Some(';')), { line = 1; column = 5 }) |}]
;;

let%expect_test "opt | success none" =
  let open String in
  let input = "123" in
  let parser = uint *>>* opt (char ';') in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_pair pp_int (pp_option pp_char))) result;
  [%expect {| Ok((123, None), { line = 1; column = 4 }) |}]
;;

let%expect_test "int | success positive" =
  let open String in
  let input = "123z" in
  let parser = int in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, { line = 1; column = 4 }) |}]
;;

let%expect_test "int | success negative" =
  let open String in
  let input = "-123z" in
  let parser = int in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(-123, { line = 1; column = 5 }) |}]
;;

let%expect_test "int | fail positive" =
  let open String in
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
  let open String in
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
  let open String in
  let input = "123;" in
  let parser = int *>> char ';' in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, { line = 1; column = 5 }) |}]
;;

let%expect_test "ignore_left | success" =
  let open String in
  let input = ";123" in
  let parser = char ';' >>* int in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, { line = 1; column = 5 }) |}]
;;

let%expect_test "between | success" =
  let open String in
  let input = "(123)" in
  let parser = between (char '(') (char ')') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result pp_int) result;
  [%expect {| Ok(123, { line = 1; column = 6 }) |}]
;;

let%expect_test "sep_by_1 | success 3" =
  let open String in
  let input = "1,2,3;" in
  let parser = sep_by_1 (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2, 3], { line = 1; column = 6 }) |}]
;;

let%expect_test "sep_by_1 | success 2" =
  let open String in
  let input = "1,2;" in
  let parser = sep_by_1 (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2], { line = 1; column = 4 }) |}]
;;

let%expect_test "sep_by_1 | success 1" =
  let open String in
  let input = "1,;" in
  let parser = sep_by_1 (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1], { line = 1; column = 2 }) |}]
;;

let%expect_test "sep_by_1 | fail" =
  let open String in
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
  let open String in
  let input = "1,2,3;" in
  let parser = sep_by (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2, 3], { line = 1; column = 6 }) |}]
;;

let%expect_test "sep_by | success 2" =
  let open String in
  let input = "1,2;" in
  let parser = sep_by (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1, 2], { line = 1; column = 4 }) |}]
;;

let%expect_test "sep_by | success 1" =
  let open String in
  let input = "1,;" in
  let parser = sep_by (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([1], { line = 1; column = 2 }) |}]
;;

let%expect_test "sep_by | success 0" =
  let open String in
  let input = ";" in
  let parser = sep_by (char ',') uint in
  let result = run parser input in
  Printf.printf "%a" (pp_parse_result (pp_list pp_int)) result;
  [%expect {| Ok([], { line = 1; column = 1 }) |}]
;;

let%expect_test "bind" =
  let open String in
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
