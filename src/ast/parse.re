open Core;

type parse_error =
  | Lexing(string, Lexing.position)
  | Parsing(option(message), Lexing.position, Lexing.position)
and message = string;

let position = ({Lexing.pos_fname, pos_lnum, pos_cnum, pos_bol}) => {
  let file = pos_fname;
  let line = pos_lnum;
  let character = pos_cnum - pos_bol;
  (file, line, character);
};

let nth_line = (file, line) =>
  if (line <= 0) {
    None;
  } else {
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

let print_error = error => {
  /* we make sure we render tabs as 4 spaces so that we can highlight
   */
  let fixtabs = line =>
    String.concat_map(line, ch =>
      if (ch == '\t') {
        "    ";
      } else {
        String.make(1, ch);
      }
    );

  /* this gets around tabs by replacing everything except tabs with a space
   * in order to make a prefix, and then we fixtabs, slice, and print the
   * right number of highlight characters.
   */
  let print_highlight = (line, start_character, end_character) => {
    let prefix =
      if (start_character == 0) {
        "";
      } else {
        let replacer =
          fun
          | '\t' => '\t'
          | _ => ' ';
        let replaced = String.map(line, replacer);
        fixtabs(String.slice(replaced, 0, start_character));
      };
    let underlines = String.make(end_character - start_character, '^');
    eprintf("      %s%s\n", prefix, underlines);
  };

  /* attempst to print the nth line of a file and returns the line if it
   * was available.
   */
  let print_line = (file, line_num) =>
    switch (nth_line(file, line_num)) {
    | None => None
    | Some(line) =>
      eprintf("%4d: %s\n", line_num, fixtabs(line));
      Some(line);
    };

  switch (error) {
  | Parsing(message, start_pos, end_pos) =>
    let (file, start_line, start_character) = position(start_pos);
    let (_, curr_line, curr_character) = position(end_pos);
    let lines =
      if (curr_line == start_line) {
        sprintf("line %d", curr_line);
      } else {
        sprintf("lines %d-%d", start_line, curr_line);
      };
    eprintf("File %s, %s, parsing error:\n%!", file, lines);
    let _ = print_line(file, curr_line - 2);
    let _ = print_line(file, curr_line - 1);
    switch (print_line(file, curr_line)) {
    | None => ()
    | Some(line) =>
      if (curr_line == start_line) {
        print_highlight(line, start_character, curr_character);
      }
    };
    switch (message) {
    | None => ()
    | Some(error_message) => prerr_endline(error_message)
    };

  | Lexing(invalid_input, err_pos) =>
    let (file, line, character) = position(err_pos);
    eprintf("File %s, line %d, lexing error:\n", file, line);
    let _ = print_line(file, line - 2);
    let _ = print_line(file, line - 1);
    switch (print_line(file, line)) {
    | None => ()
    | Some(line) => print_highlight(line, character - 1, character)
    };
  };
};

let dbx = Parser.Incremental.dbx;

let parse = (parse_fun, lexbuf) => {
  module Interp = Parser.MenhirInterpreter;
  let input = Interp.lexer_lexbuf_to_supplier(Lexer.token, lexbuf);
  let success = dbx => Ok(dbx);
  let failure = error_state =>
    switch (error_state) {
    | Interp.HandlingError(env) =>
      let (start_pos, end_pos) = Interp.positions(env);
      let num = Interp.current_state_number(env);
      let message =
        try (Some(Parser_messages.message(num))) {
        | _ => None
        };

      Core.Error(Parsing(message, start_pos, end_pos));
    | _ => assert(false)
    };

  try (Interp.loop_handle(success, failure, input, parse_fun(lexbuf.Lexing.lex_curr_p))) {
  | Lexer.Error(input, pos) => Core.Error(Lexing(input, pos))
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