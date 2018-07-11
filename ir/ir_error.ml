open Core
open Crud_ast

type t =
  | Duplicate of string * Annotate.location * Annotate.location
  | Invalid of Annotate.location
  | Undefined of string * Annotate.location

exception Exn of t

let print =
  function
  | Duplicate (kind, first, second) ->
    let first_same = first.start_line = first.end_line in
    let second_same = second.start_line = second.end_line in
    let all_same = first_same && second_same && first.start_line = second.start_line in
    printf "File %s, duplicate: %s\n" first.file kind;
    print_endline "  first occurance:";
    Annotate.print_location ~context:2 ~highlight:true first;
    print_endline "";
    print_endline "  second occurance:";
    let context = if all_same then 0 else 2 in
    Annotate.print_location ~context ~highlight:true second

  | Invalid loc ->
    printf "File %s, invalid:\n" loc.file;
    Annotate.print_location ~context:2 ~highlight:true loc

  | Undefined (entity, loc) ->
    printf "File %s, undefined %s:\n" loc.file entity;
    Annotate.print_location ~context:2 ~highlight:true loc
