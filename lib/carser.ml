let parse_a input =
  let length = String.length input in
  if length == 0
  then false, ""
  else if String.get input 0 == 'a'
  then true, String.sub input 1 (length - 1)
  else false, input
;;

let%expect_test "abc" =
  let input = "abc" in
  let b, s = parse_a input in
  Printf.printf "(%b, %s)" b s;
  [%expect {| (true, bc) |}]
;;

let%expect_test "zbc" =
  let input = "zbc" in
  let b, s = parse_a input in
  Printf.printf "(%b, %s)" b s;
  [%expect {| (false, zbc) |}]
;;

let%expect_test "" =
  let input = "" in
  let b, s = parse_a input in
  Printf.printf "(%b, %s)" b s;
  [%expect {| (false, ) |}]
;;
