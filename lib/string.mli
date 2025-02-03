(** Module String contains a parser instance, where the stream type is string and the
    tokens are chars *)

open Parser

(** A string stream with char tokens, which conforms to the
    {!Parser.StreamType} signature. *)
module Stream : StreamType with type t := string and type token := char

(** Includes the main parser functions for convenience.

    @closed *)
include S with type stream := string and type token := char

(** [char c] returns a parser which succeeds if the next character in the stream
    is [c]. *)
val char : char -> char t

(** [any_of characters] returns a parser which succeeds if the next character in
    the stream is any of [characters]. *)
val any_of : char list -> char t

(** [string s] returns a parser which succeeds if the next characters in the
    stream form [s]. *)
val string : string -> string t

(** [uint] returns a parser which succceeds if the next characters in the stream
    form an unsigned integer. *)
val uint : int t

(** [int] returns a parser which succceeds if the next characters in the stream
    form an signed integer. *)
val int : int t
