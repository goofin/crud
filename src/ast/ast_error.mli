type t =
  | Lexing of string * Lexing.position
  | Parsing of string option * Lexing.position * Lexing.position

val print : t -> unit