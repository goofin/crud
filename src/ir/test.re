open Core;

let loop = (filename, ()) =>
  Crud_ast.(
    List.iter(Parse.parse_file_exn(Parse.dbx, filename), ~f=def =>
      printf("%s\n", def |> Syntax.sexp_of_definition |> Sexp.to_string_hum)
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