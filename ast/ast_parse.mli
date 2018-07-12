open Base

type parser
val dbx : parser

type t = (Ast_syntax.definition Ast_annotate.t list, Ast_error.t) Result.t

val parse : parser -> Lexing.lexbuf -> t
val parse_string : parser -> string -> t
val parse_file : parser -> string -> t
