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

let state_from_stream input = { input; index = 0; position = initial_position }

module type StreamType = sig
  type t
  type token

  val next_token : t state -> (token * t state) option
  val pp_token : unit -> token -> string
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
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val and_then : 'a t -> 'b t -> ('a * 'b) t
  val keep_left : 'a t -> 'b t -> 'a t
  val keep_right : 'a t -> 'b t -> 'b t
  val between : 'a t -> 'b t -> 'c t -> 'c t
  val sequence : 'a t list -> 'a list t
  val or_else : 'a t -> 'a t -> 'a t
  val choice : 'a t list -> 'a t
  val many : 'a t -> 'a list t
  val some : 'a t -> 'a list t
  val opt : 'a t -> 'a option t
  val sep_by_1 : 'b t -> 'a t -> 'a list t
  val sep_by : 'b t -> 'a t -> 'a list t

  module Operators : sig
    val ( <?> ) : 'a t -> parser_label -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( *>>* ) : 'a t -> 'b t -> ('a * 'b) t
    val ( *>> ) : 'a t -> 'b t -> 'a t
    val ( >>* ) : 'a t -> 'b t -> 'b t
    val ( <|> ) : 'a t -> 'a t -> 'a t
  end

  module Applicative_syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

  module Monad_syntax : sig
    include module type of struct
      include Applicative_syntax
    end

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  end
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

  let run parser input = parser.parse (state_from_stream input)

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

  let ( let* ) = bind

  let map f parser =
    let* x = parser in
    const (f x)
  ;;

  let ( let+ ) x f = map f x

  let and_then parser1 parser2 =
    let label = Printf.sprintf "%s and then %s" parser1.label parser2.label in
    (let* x1 = parser1 in
     let* x2 = parser2 in
     const (x1, x2))
    <?> label
  ;;

  let ( *>>* ) = and_then
  let ( and+ ) = ( *>>* )

  let keep_left parser1 parser2 =
    let+ value, _ = parser1 *>>* parser2 in
    value
  ;;

  let ( *>> ) = keep_left

  let keep_right parser1 parser2 =
    let+ _, value = parser1 *>>* parser2 in
    value
  ;;

  let ( >>* ) = keep_right
  let between left right parser = left >>* parser *>> right

  let rec sequence parsers =
    match parsers with
    | [] -> const [] <?> "nothing"
    | phead :: ptail ->
      let label =
        parsers
        |> List.map (fun parser -> parser.label)
        |> Stdlib.String.concat " and then "
      in
      (let+ head = phead
       and+ tail = sequence ptail <?> "foo" in
       head :: tail)
      <?> label
  ;;

  let or_else parser1 parser2 =
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

  let ( <|> ) = or_else

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

  module Operators = struct
    let ( <?> ) label parser = with_label parser label
    let ( >>= ) = bind
    let ( *>>* ) = and_then
    let ( *>> ) = keep_left
    let ( >>* ) = keep_right
    let ( <|> ) = or_else
  end

  module Applicative_syntax = struct
    let ( let+ ) = ( let+ )
    let ( and+ ) = ( and+ )
  end

  module Monad_syntax = struct
    include Applicative_syntax

    let ( let* ) = ( let* )
    let ( and* ) = ( and+ )
  end
end
