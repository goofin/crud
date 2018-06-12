open Core;

[@deriving (sexp, hash, compare)]
type location = {
  file: string,
  start_line: int,
  start_pos: int,
  end_line: int,
  end_pos: int,
};

type t('a) = {
  node: 'a,
  loc: location,
};

module Utils = {
  /* read_file reads the file into an array of lines */
  let read_file = file => {
    let rec read_lines = (accum, input) =>
      switch (In_channel.input_line(input)) {
      | Some(line) => read_lines([line, ...accum], input)
      | None => List.rev(accum)
      };
    let lines =
      try (In_channel.with_file(file, ~f=read_lines([]))) {
      | _ => []
      };
    Array.of_list(lines);
  };

  module M = Map.Make(String);
  let files = ref(M.empty);

  /* returns the nth line of a file and caches the contents of the
   * file for further use.
   */
  let nth_line = (file, line) => {
    let lines =
      switch (M.find(files^, file)) {
      | Some(lines) => lines
      | None =>
        let lines = read_file(file);
        files := M.set(files^, file, lines);
        lines;
      };
    try (Some(lines[line - 1])) {
    | _ => None
    };
  };

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
  let print_highlight = (line, start_pos, end_pos) => {
    /* add some non-whitespace to the end in case we want to highlight one character past the
     * end of the string
     */
    let line = line ++ "_";
    let rec count_chars = (~accum=0, ch) =>
      fun
      | "" => accum
      | line when line.[0] == ch => count_chars(~accum=accum + 1, ch, String.slice(line, 1, 0))
      | line => count_chars(~accum, ch, String.slice(line, 1, 0));
    let prefix_tabs =
      if (start_pos == 0) {
        0;
      } else {
        count_chars('\t', String.slice(line, 0, start_pos));
      };
    let inside_tabs = count_chars('\t', String.slice(line, start_pos, end_pos));
    let line = fixtabs(line);
    let prefix_length = start_pos + 3 * prefix_tabs;
    let rec loop = accum =>
      fun
      | "" => accum
      | line when line.[0] == ' ' => loop(accum + 1, String.slice(line, 1, 0))
      | _ => accum;
    let whitespace = loop(0, String.slice(line, prefix_length, 0));
    let prefix = String.make(prefix_length + whitespace, ' ');
    let highlight = String.make(end_pos - start_pos + 3 * inside_tabs - whitespace, '^');
    printf("      %s%s\n", prefix, highlight);
  };

  /* attempst to print the nth line of a file and returns the line if it
   * was available.
   */
  let print_line = (file, line_num) =>
    switch (nth_line(file, line_num)) {
    | None => None
    | Some(line) =>
      printf("%4d: %s\n", line_num, fixtabs(line));
      Some(line);
    };
};

let print_location = (~depth=0, ~highlight=false, ~context=0, loc) => {
  let prefix = String.make(depth, '\t');
  for (i in 0 to context - 1) {
    let _ = Utils.print_line(loc.file, loc.start_line - (context - i));
    ();
  };
  let loc =
    if (loc.end_line == loc.start_line + 1 && loc.end_pos == 0) {
      {...loc, end_line: loc.start_line, end_pos: loc.start_pos + 1};
    } else {
      loc;
    };
  for (line_num in loc.start_line to loc.end_line) {
    let line =
      switch (Utils.nth_line(loc.file, line_num)) {
      | Some(line) => line
      | _ => assert(false)
      };
    printf("%s%4d: ", prefix, line_num);
    if (highlight) {
      print_endline(Utils.fixtabs(line));
      if (loc.start_line == loc.end_line) {
        Utils.print_highlight(line, loc.start_pos, loc.end_pos);
      } else if (line_num == loc.start_line) {
        Utils.print_highlight(line, loc.start_pos, String.length(line));
      } else if (line_num == loc.end_line) {
        Utils.print_highlight(line, 0, loc.end_pos);
      } else {
        Utils.print_highlight(line, 0, String.length(line));
      };
    } else if (loc.start_line == loc.end_line) {
      print_endline(String.slice(line, loc.start_pos, loc.end_pos));
    } else if (line_num == loc.start_line) {
      print_endline(String.slice(line, loc.start_pos, 0));
    } else if (line_num == loc.end_line) {
      print_endline(String.slice(line, 0, loc.end_pos));
    } else {
      print_endline(line);
    };
  };
};
