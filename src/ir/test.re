open Core;

let loop = (filename, ()) =>
  Crud_ast.(
    List.iter(Parse.parse_file_exn(Parse.prog, filename), ~f=def =>
      printf("%s\n", Syntax.show_definition(def))
    )
  );

let () =
  Command.run(
    Command.basic_spec(
      ~summary="Test",
      Command.Spec.(empty +> anon("filename" %: file)),
      loop,
    ),
  );
