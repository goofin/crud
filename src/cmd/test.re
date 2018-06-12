open Core;

let loop = (filename, ()) =>
  switch (Crud_ast.parse_file(filename)) {
  | Error(err) => Crud_ast.Error.print(err)
  | Ok(defs) =>
    /* List.iter(defs, Crud_ast.print); */
    switch (Crud_ir.transform_defs(defs)) {
    | Error(err) => Crud_ir.Error.print(err)
    | Ok(defs) => Crud_ir.print(defs)
    }
  };

{
  open Command;

  let args = Spec.(empty +> anon("filename" %: file));
  let spec = basic_spec(~summary="Test", args, loop);

  run(spec);
};
