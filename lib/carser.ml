let pchar expected input =
  let length = String.length input in
  if length == 0
  then "no more input", ""
  else if String.get input 0 == expected
  then Printf.sprintf "%c" (String.get input 0), String.sub input 1 (length - 1)
  else Printf.sprintf "expected %c, got %c" expected (String.get input 0), input
;;

let%expect_test "abc" =
  let input = "abc" in
  let parse_a = pchar 'a' in
  let b, s = parse_a input in
  Printf.printf "(%s, %s)" b s;
  [%expect {| (a, bc) |}]
;;

let%expect_test "zbc" =
  let input = "zbc" in
  let parse_a = pchar 'a' in
  let b, s = parse_a input in
  Printf.printf "(%s, %s)" b s;
  [%expect {| (expected a, got z, zbc) |}]
;;

let%expect_test "" =
  let input = "" in
  let parse_a = pchar 'a' in
  let b, s = parse_a input in
  Printf.printf "(%s, %s)" b s;
  [%expect {| (no more input, ) |}]
;;
