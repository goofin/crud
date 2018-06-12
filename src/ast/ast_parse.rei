open Core;

type parser;
let dbx: parser;

type t = Result.t(list(Ast_annotate.t(Ast_syntax.definition)), Ast_error.t);

let parse: (parser, Lexing.lexbuf) => t;
let parse_string: (parser, string) => t;
let parse_file: (parser, string) => t;
