open Core

type parse_error =
  | Lexing of string * Lexing.position
  | Parsing of message option * Lexing.position * Lexing.position
and message = string

exception Error of parse_error

let position {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} =
  let file = pos_fname in
  let line = pos_lnum in
  let character = pos_cnum - pos_bol in
  (file, line, character)

let nth_line file line =
  let open In_channel in
  try
    with_file file ~f:(fun input ->
        for i = 1 to line - 1 do
          ignore (input_line input)
        done;
        input_line input
      )
  with _ -> None

let report_error lexbuf = function
  | Parsing (message, start_pos, end_pos) ->
    let file, start_line, start_character = position start_pos in
    let _, curr_line, curr_character = position end_pos in
    let lines =
      if curr_line = start_line
      then sprintf "line %d" curr_line
      else sprintf "lines %d-%d" start_line curr_line in
    let characters =
      if curr_line = start_line
      then sprintf "characters %d-%d" start_character curr_character
      else sprintf "character %d" start_character in
    eprintf "File %s, %s, %s, parsing error:\n%!"
      file lines characters;
    begin match nth_line file curr_line with
      | None -> ()
      | Some line -> eprintf "> %s\n" line;
    end;
    begin match message with
      | None -> ()
      | Some error_message -> prerr_endline error_message
    end
  | Lexing (invalid_input, err_pos) ->
    let file, line, character = position err_pos in
    eprintf "File %s, line %d, character %d, lexing error:\n"
      file line character;
    begin match nth_line file line with
      | None -> ()
      | Some line -> eprintf "> %s\n" line;
    end;
    eprintf "Invalid input %s\n%!" invalid_input

let prog = Parser.Incremental.prog

let parse parse_fun lexbuf =
  let open MenhirLib.General in
  let module Interp = Parser.MenhirInterpreter in
  let input = Interp.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let success prog = prog in
  let failure error_state =
    let env = match[@warning "-4"] error_state with
      | Interp.HandlingError env -> env
      | _ -> assert false in
    match Interp.stack env with
    | lazy Nil -> assert false
    | lazy (Cons (Interp.Element (state, _, start_pos, end_pos), _)) ->
      let message =
        try Some (Parser_messages.message (Interp.number state)) with
        | Not_found -> None
      in
      raise (Error (Parsing (message, start_pos, end_pos)))
  in
  try
    Interp.loop_handle success failure input
      (parse_fun lexbuf.Lexing.lex_curr_p)
  with Lexer.Error (input, pos) -> raise (Error (Lexing (input, pos)))

let parse_string parse_fun str =
  let lexbuf = Lexing.from_string str in
  parse parse_fun lexbuf

let parse_file parse_fun path =
  let open In_channel in
  with_file path ~f:(fun chan ->
      let lexbuf =
        let open Lexing in
        let lexbuf = from_channel chan in
        lexbuf.lex_start_p <- {
          pos_fname = path;
          pos_lnum = 1;
          pos_bol = 0;
          pos_cnum = 0;
        };
        lexbuf.lex_curr_p <- lexbuf.lex_start_p;
        lexbuf
      in
      try parse parse_fun lexbuf
      with (Error err) as exn ->
        report_error lexbuf err;
        raise exn
    )
