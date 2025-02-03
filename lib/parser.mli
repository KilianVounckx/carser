type position =
  { line : int
  ; column : int
  }

val initial_position : position
val inc_line : position -> position
val inc_column : position -> position
val pp_position : out_channel -> position -> unit

type 's state =
  { input : 's
  ; index : int
  ; position : position
  }

val state_from_stream : 's -> 's state

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
  S with type stream := Stream.t and type token := Stream.token
