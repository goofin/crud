open Core

type t =
  | Lexing of string * Lexing.position
  | Parsing of string option * Lexing.position * Lexing.position

let location
    { Lexing.pos_fname = file
    ; pos_lnum = start_line
    ; pos_cnum = start_cnum
    ; pos_bol = start_bol
    }
    { Lexing.pos_lnum = end_line
    ; pos_cnum = end_cnum
    ; pos_bol = end_bol
    ; _
    }
  =
  { Ast_annotate.file = file
  ; start_line
  ; start_pos = (start_cnum - start_bol)
  ; end_line
  ; end_pos = (end_cnum - end_bol)
  }

let print  = function
  | Parsing (message, start_pos, end_pos) ->
    let loc = location start_pos end_pos in
    let lines =
      if loc.start_line = loc.end_line then
        sprintf "line %d" loc.start_line
      else
        sprintf "lines %d-%d" loc.start_line loc.end_line
    in
    printf "File %s, %s, parsing error:\n%!" loc.file lines;
    Ast_annotate.print_location ~context:2 ~highlight:true loc;
    begin match message with
      | None -> ()
      | Some message -> print_endline message
    end

  | Lexing (_, err_pos) ->
    let loc = location err_pos err_pos in
    let loc = { loc with start_pos = loc.start_pos - 1 } in
    printf "File %s, line %d, lexing error:\n" loc.file loc.start_line;
    Ast_annotate.print_location ~context:2 ~highlight:true loc
