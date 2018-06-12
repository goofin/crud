open Core;

type parser =
  Lexing.position =>
  Ast_parser.MenhirInterpreter.checkpoint(list(Ast_annotate.t(Ast_syntax.definition)));

let dbx: parser = Ast_parser.Incremental.dbx;

type t = Result.t(list(Ast_annotate.t(Ast_syntax.definition)), Ast_error.t);

let parse = (parse_fun, lexbuf) => {
  module Interp = Ast_parser.MenhirInterpreter;
  let input = Interp.lexer_lexbuf_to_supplier(Ast_lexer.token, lexbuf);
  let success = dbx => Ok(dbx);
  let failure = error_state =>
    switch (error_state) {
    | Interp.HandlingError(env) =>
      let (start_pos, end_pos) = Interp.positions(env);
      let num = Interp.current_state_number(env);
      let message =
        try (Some(Ast_parser_messages.message(num))) {
        | _ => None
        };
      Core.Error(Ast_error.Parsing(message, start_pos, end_pos));
    | _ => assert(false)
    };

  try (Interp.loop_handle(success, failure, input, parse_fun(lexbuf.Lexing.lex_curr_p))) {
  | Ast_lexer.Error(input, pos) => Core.Error(Lexing(input, pos))
  };
};

let parse_string = (parse_fun, str) => {
  let lexbuf = Lexing.from_string(str);
  parse(parse_fun, lexbuf);
};

let parse_file = (parse_fun, path) => {
  open In_channel;
  let handle = chan => {
    let lexbuf = {
      open Lexing;
      let lexbuf = from_channel(chan);
      lexbuf.lex_start_p = {pos_fname: path, pos_lnum: 1, pos_bol: 0, pos_cnum: 0};
      lexbuf.lex_curr_p = lexbuf.lex_start_p;
      lexbuf;
    };
    parse(parse_fun, lexbuf);
  };

  with_file(path, ~f=handle);
};
