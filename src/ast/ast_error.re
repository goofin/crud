open Core;

type t =
  | Lexing(string, Lexing.position)
  | Parsing(option(string), Lexing.position, Lexing.position);

let location =
    (
      {Lexing.pos_fname: file, pos_lnum: start_line, pos_cnum: start_cnum, pos_bol: start_bol},
      {Lexing.pos_lnum: end_line, pos_cnum: end_cnum, pos_bol: end_bol},
    ) => {
  Ast_annotate.file,
  start_line,
  start_pos: start_cnum - start_bol,
  end_line,
  end_pos: end_cnum - end_bol,
};

let print = error =>
  switch (error) {
  | Parsing(message, start_pos, end_pos) =>
    let loc = location(start_pos, end_pos);
    let lines =
      if (loc.start_line == loc.end_line) {
        sprintf("line %d", loc.start_line);
      } else {
        sprintf("lines %d-%d", loc.start_line, loc.end_line);
      };
    printf("File %s, %s, parsing error:\n%!", loc.file, lines);
    Ast_annotate.print_location(~context=2, ~highlight=true, loc);
    switch (message) {
    | None => ()
    | Some(error_message) => print_endline(error_message)
    };

  | Lexing(invalid_input, err_pos) =>
    let loc = location(err_pos, err_pos);
    let loc = {...loc, start_pos: loc.start_pos - 1};
    printf("File %s, line %d, lexing error:\n", loc.file, loc.start_line);
    Ast_annotate.print_location(~context=2, ~highlight=true, loc);
  };
