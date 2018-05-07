open Core;

type parse_error =
  | Lexing(string, Lexing.position)
  | Parsing(option(message), Lexing.position, Lexing.position)
and message = string;

exception Error(parse_error);

let position = ({Lexing.pos_fname, pos_lnum, pos_cnum, pos_bol}) => {
  let file = pos_fname;
  let line = pos_lnum;
  let character = pos_cnum - pos_bol;
  (file, line, character);
};

let nth_line = (file, line) => {
  let read_lines = input => {
    for (i in 1 to line - 1) {
      ignore(In_channel.input_line(input));
    };
    In_channel.input_line(input);
  };
  try (In_channel.with_file(file, ~f=read_lines)) {
  | _ => None
  };
};

let print_error =
  fun
  | Parsing(message, start_pos, end_pos) => {
      let (file, start_line, start_character) = position(start_pos);
      let (_, curr_line, curr_character) = position(end_pos);
      let lines =
        if (curr_line == start_line) {
          sprintf("line %d", curr_line);
        } else {
          sprintf("lines %d-%d", start_line, curr_line);
        };
      let characters =
        if (curr_line == start_line) {
          sprintf("characters %d-%d", start_character, curr_character);
        } else {
          sprintf("character %d", start_character);
        };
      eprintf("File %s, %s, %s, parsing error:\n%!", file, lines, characters);
      switch (nth_line(file, curr_line)) {
      | None => ()
      | Some(line) => eprintf("> %s\n", line)
      };
      switch (message) {
      | None => ()
      | Some(error_message) => prerr_endline(error_message)
      };
    }
  | Lexing(invalid_input, err_pos) => {
      let (file, line, character) = position(err_pos);
      eprintf(
        "File %s, line %d, character %d, lexing error:\n",
        file,
        line,
        character,
      );
      switch (nth_line(file, line)) {
      | None => ()
      | Some(line) => eprintf("> %s\n", line)
      };
      eprintf("Invalid input %s\n%!", invalid_input);
    };

let prog = Parser.Incremental.prog;

let parse_exn = (parse_fun, lexbuf) => {
  open MenhirLib.General;
  module Interp = Parser.MenhirInterpreter;
  let input = Interp.lexer_lexbuf_to_supplier(Lexer.token, lexbuf);
  let success = prog => prog;
  let failure = error_state => {
    let env =
      switch (error_state) {
      | Interp.HandlingError(env) => env
      | _ => assert(false)
      };
    switch (Interp.stack(env)) {
    | lazy Nil => assert(false)
    | lazy (Cons(Interp.Element(state, _, start_pos, end_pos), _)) =>
      let message =
        try (Some(Parser_messages.message(Interp.number(state)))) {
        | _ => None
        };

      raise(Error(Parsing(message, start_pos, end_pos)));
    };
  };

  try (
    Interp.loop_handle(
      success,
      failure,
      input,
      parse_fun(lexbuf.Lexing.lex_curr_p),
    )
  ) {
  | Lexer.Error(input, pos) => raise(Error(Lexing(input, pos)))
  };
};

let parse = (parse_fun, lexbuf) =>
  try (Ok(parse_exn(parse_fun, lexbuf))) {
  | err => Error(err)
  };

let parse_string_exn = (parse_fun, str) => {
  let lexbuf = Lexing.from_string(str);
  parse_exn(parse_fun, lexbuf);
};

let parse_string = (parse_fun, str) =>
  try (Ok(parse_string_exn(parse_fun, str))) {
  | err => Error(err)
  };

let parse_file_exn = (parse_fun, path) => {
  open In_channel;
  let handle = chan => {
    let lexbuf = {
      open Lexing;
      let lexbuf = from_channel(chan);
      lexbuf.lex_start_p = {
        pos_fname: path,
        pos_lnum: 1,
        pos_bol: 0,
        pos_cnum: 0,
      };
      lexbuf.lex_curr_p = lexbuf.lex_start_p;
      lexbuf;
    };
    parse_exn(parse_fun, lexbuf);
  };
  with_file(path, ~f=handle);
};

let parse_file = (parse_fun, path) =>
  try (Ok(parse_file_exn(parse_fun, path))) {
  | err => Error(err)
  };
