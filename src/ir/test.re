open Core;

let loop = (filename, ()) =>
  Crud_ast.(
    switch (Parse.parse_file(Parse.dbx, filename)) {
    | Ok(defs) => List.iter(defs, def => Crud_ast.Print.definition(def))
    | Error(err) => Parse.print_error(err)
    }
  );

let () = Command.run(Command.basic_spec(~summary="Test", Command.Spec.(empty +> anon("filename" %: file)), loop));