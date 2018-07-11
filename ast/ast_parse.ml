open Core

type parser = Lexing.position ->
  Ast_syntax.definition Ast_annotate.t list Ast_parser.MenhirInterpreter.checkpoint

let dbx : parser = Ast_parser.Incremental.dbx

type t = (Ast_syntax.definition Ast_annotate.t list, Ast_error.t) Result.t

let parse parse_fun lexbuf =
  let module Interp = Ast_parser.MenhirInterpreter in
  let input = Interp.lexer_lexbuf_to_supplier Ast_lexer.token lexbuf in
  let success dbx = Ok dbx in
  let failure = function
    | Interp.HandlingError env ->
      let (start_pos, end_pos) = Interp.positions env in
      (* remove this until we actually care about messages
         let num = Interp.current_state_number env in
         let message =
         try Some (Ast_parser_messages.message num)
         with | _ -> None
         in
      *)
      Core.Error (Ast_error.Parsing (None, start_pos, end_pos))
    | _ -> assert false
  in
  try Interp.loop_handle success failure input (parse_fun lexbuf.Lexing.lex_curr_p)
  with | Ast_lexer.Error (input, pos) -> Core.Error (Lexing (input, pos))

let parse_string parse_fun str =
  parse parse_fun @@ Lexing.from_string str

let parse_file parse_fun path =
  let open In_channel in
  with_file path ~f:(fun chan ->
      let open Lexing in
      let lexbuf = from_channel chan in
      lexbuf.lex_start_p <- { pos_fname = path
                            ; pos_lnum = 1
                            ; pos_bol = 0
                            ; pos_cnum = 0
                            };
      lexbuf.lex_curr_p <- lexbuf.lex_start_p;
      parse parse_fun lexbuf
    )
