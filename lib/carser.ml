type 'a parse_result = ('a, string) Result.t

let pp_parse_result pp_ok oc = function
  | Ok (x, rest) -> Printf.fprintf oc "Ok(%a, \"%s\")" pp_ok x rest
  | Error message -> Printf.fprintf oc "Error(\"%s\")" message
;;

let pchar expected input =
  let length = String.length input in
  if length == 0
  then Error "no more input"
  else if String.get input 0 == expected
  then Ok (String.get input 0, String.sub input 1 (length - 1))
  else Error (Printf.sprintf "expected %c, got %c" expected (String.get input 0))
;;

let pp_char oc c = Printf.fprintf oc "%c" c

let%expect_test "abc" =
  let input = "abc" in
  let parse_a = pchar 'a' in
  let result = parse_a input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Ok(a, "bc") |}]
;;

let%expect_test "zbc" =
  let input = "zbc" in
  let parse_a = pchar 'a' in
  let result = parse_a input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("expected a, got z") |}]
;;

let%expect_test "" =
  let input = "" in
  let parse_a = pchar 'a' in
  let result = parse_a input in
  Printf.printf "%a" (pp_parse_result pp_char) result;
  [%expect {| Error("no more input") |}]
;;
