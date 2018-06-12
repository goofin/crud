open Core;
open Crud_ast;

type t =
  | Duplicate(string, Annotate.location, Annotate.location)
  | Invalid(Annotate.location)
  | Undefined(string, Annotate.location);

exception Exn(t);

let print =
  fun
  | Duplicate(kind, first, second) => {
      let first_same = first.start_line == first.end_line;
      let second_same = second.start_line == second.end_line;
      let all_same = first_same && second_same && first.start_line == second.start_line;
      printf("File %s, duplicate %s:\n", first.file, kind);
      print_endline("  first occurance:");
      Annotate.print_location(~context=2, ~highlight=true, first);
      print_endline("");
      print_endline("  second occurance:");
      let context = if (all_same) {0} else {2};
      Annotate.print_location(~context, ~highlight=true, second);
    }
  | Invalid(loc) => {
      printf("File %s, invalid:\n", loc.file);
      Annotate.print_location(~context=2, ~highlight=true, loc);
    }
  | Undefined(entity, loc) => {
      printf("File %s, undefined %s:\n", loc.file, entity);
      Annotate.print_location(~context=2, ~highlight=true, loc);
    };
