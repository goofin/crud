open Core

type location =
  { file: string
  ; start_line: int
  ; start_pos: int
  ; end_line: int
  ; end_pos: int
  }
[@@deriving (sexp, hash, compare)]

type 'a t =
  { node: 'a
  ; loc: location
  }

module Utils = struct
  let read_file file =
    let rec read_lines accum input =
      match In_channel.input_line input with
      | Some line -> read_lines (line :: accum) input
      | None -> List.rev accum
    in

    let lines =
      try In_channel.with_file file ~f:(read_lines [])
      with | _ -> []
    in
    Array.of_list lines

  module M = (Map.Make)(String)
  let files = ref M.empty

  let nth_line file line =
    let lines =
      match M.find !files file with
      | Some lines -> lines
      | None ->
        let lines = read_file file in
        files := M.set !files file lines;
        lines
    in

    try Some lines.(line - 1) with
    | _ -> None

  let fixtabs line =
    String.concat_map line (fun ch ->
        if ch = '\t' then
          "    "
        else
          String.make 1 ch
      )

  let print_highlight line start_pos end_pos =
    let line = line ^ "_" in

    let rec count_chars ?(accum= 0) ch =
      function
      | "" -> accum
      | line when (line.[0]) = ch ->
        count_chars ~accum:(accum + 1) ch (String.slice line 1 0)
      | line -> count_chars ~accum ch (String.slice line 1 0)
    in

    let prefix_tabs =
      if start_pos = 0 then
        0
      else
        count_chars '\t' (String.slice line 0 start_pos)
    in

    let inside_tabs = count_chars '\t' (String.slice line start_pos end_pos) in
    let line = fixtabs line in
    let prefix_length = start_pos + (3 * prefix_tabs) in

    let rec loop accum =
      function
      | "" -> accum
      | line when (line.[0]) = ' ' -> loop (accum + 1) (String.slice line 1 0)
      | _ -> accum
    in

    let whitespace = loop 0 (String.slice line prefix_length 0) in
    let prefix = String.make (prefix_length + whitespace) ' ' in
    let highlight = String.make (end_pos - start_pos + (3 * inside_tabs) - whitespace) '^' in
    printf "      %s%s\n" prefix highlight

  let print_line file line_num =
    match nth_line file line_num with
    | None -> None
    | Some line ->
      printf "%4d: %s\n" line_num (fixtabs line);
      Some line
end

let print_location ?(depth=0) ?(highlight=false) ?(context=0) loc =
  let prefix = String.make depth '\t' in

  for i = 0 to context - 1 do
    ignore @@ Utils.print_line loc.file (loc.start_line - context - i)
  done;

  let loc =
    if loc.end_line = loc.start_line + 1 && loc.end_pos = 0 then
      { loc with end_line = loc.start_line
               ; end_pos = loc.start_pos + 1
      }
    else
      loc
  in

  for line_num = loc.start_line to loc.end_line do
    let line =
      match Utils.nth_line loc.file line_num with
      | Some line -> line
      | _ -> assert false
    in

    printf "%s%4d: " prefix line_num;

    if highlight then begin
      print_endline (Utils.fixtabs line);
      if loc.start_line = loc.end_line then
        Utils.print_highlight line loc.start_pos loc.end_pos
      else if line_num = loc.start_line then
        Utils.print_highlight line loc.start_pos (String.length line)
      else if line_num = loc.end_line then
        Utils.print_highlight line 0 loc.end_pos
      else
        Utils.print_highlight line 0 (String.length line)
    end

    else if loc.start_line = loc.end_line then
      print_endline (String.slice line loc.start_pos loc.end_pos)
    else if line_num = loc.start_line then
      print_endline (String.slice line loc.start_pos 0)
    else if line_num = loc.end_line then
      print_endline (String.slice line 0 loc.end_pos)
    else
      print_endline line

  done
