open Core

let loop filename () =
  List.iter (Parse.parse_file Parse.prog filename)
    ~f:(fun def -> printf "%s\n" (Syntax.show_definition def))

let () =
  Command.basic_spec ~summary:"Test"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run
