open Core;

type parse_error;
let print_error: parse_error => unit;
type t = Result.t(list(Syntax.definition), parse_error);

type parser;
let dbx: parser;

let parse: (parser, Lexing.lexbuf) => t;
let parse_string: (parser, string) => t;
let parse_file: (parser, string) => t;
