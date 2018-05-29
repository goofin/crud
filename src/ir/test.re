open Core;

let loop = (filename, ()) =>
  Crud_ast.(
    switch (Parse.parse_file(Parse.dbx, filename)) {
    | Ok(defs) =>
      List.iter(defs, ~f=def =>
        printf("%s\n", def |> Syntax.sexp_of_definition |> Sexp.to_string_hum)
      )
    | Error(err) => Parse.print_error(err)
    }
  );

let () =
  Command.run(
    Command.basic_spec(
      ~summary="Test",
      Command.Spec.(empty +> anon("filename" %: file)),
      loop,
    ),
  );