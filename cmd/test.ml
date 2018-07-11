open Core

let loop filename () =
  match Crud_ast.parse_file filename with
  | Error err -> Crud_ast.Error.print err
  | Ok defs ->
    List.iter defs ~f:Crud_ast.print;
    match Crud_ir.transform_defs defs with
    | Error err -> Crud_ir.Error.print err
    | Ok defs -> Crud_ir.print defs

let command =
  Command.basic
    ~summary:"Does dbx stuff"
    Command.Let_syntax.(
      let%map_open
        filename = anon ("filename" %: string)
      in
      loop filename
    )

let () =
  Command.run command
