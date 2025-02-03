open Parser
module Stream : StreamType with type t := string and type token := char
include S with type stream := string and type token := char

val char : char -> char t
val any_of : char list -> char t
val string : string -> string t
val uint : int t
val int : int t
