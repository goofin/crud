open Core;
open Crud_ast.Syntax;

let nth_line = (file, line) =>
  if (line <= 0) {
    None;
  } else {
    let read_lines = input => {
      for (i in 1 to line - 1) {
        ignore(In_channel.input_line(input));
      };
      In_channel.input_line(input);
    };
    try (In_channel.with_file(file, ~f=read_lines)) {
    | _ => None
    };
  };

let print_annotated = (depth, header, annotated) => {
  open Crud_ast.Syntax.Annotate;
  let start_line = annotated.loc.start_line;
  let end_line = annotated.loc.end_line;
  let prefix = String.make(depth, '\t');
  printf("%s%s\n", prefix, header);
  for (line_num in start_line to end_line) {
    let line =
      switch (nth_line(annotated.loc.file, line_num)) {
      | Some(line) => line
      | _ => assert(false)
      };
    printf("%s%4d: ", prefix, line_num);
    if (start_line == end_line) {
      print_endline(String.slice(line, annotated.loc.start_pos, annotated.loc.end_pos));
    } else if (line_num == start_line) {
      print_endline(String.slice(line, annotated.loc.start_pos, 0));
    } else if (line_num == end_line) {
      print_endline(String.slice(line, 0, annotated.loc.end_pos));
    } else {
      print_endline(line);
    };
  };
  printf("\n");
};

let rec walk_definition =
  fun
  | Model(model) => walk_model(model)
  | Crud(crud) => walk_crud(crud)
and walk_model = annotated => {
  print_annotated(0, "model", annotated);
  let (name, entries) = annotated.Annotate.node;
  walk_model_name(name);
  walk_model_entries(entries);
}
and walk_model_name = annotated => print_annotated(1, "name", annotated)
and walk_model_entries = entries => List.iter(entries, walk_model_entry)
and walk_model_entry =
  fun
  | Model.Table(name) => walk_model_entry_table(name)
  | Model.Key(names) => walk_model_entry_key(names)
  | Model.Unique(names) => walk_model_entry_unique(names)
  | Model.Index(index) => walk_model_entry_index(index)
  | Model.Field(field) => walk_model_entry_field(field)
  | Model.Rel(rel) => walk_model_entry_rel(rel)
and walk_model_entry_table = annotated => {
  print_annotated(1, "table", annotated);
  print_annotated(2, "name", annotated.Annotate.node);
}
and walk_model_entry_key = annotated => {
  print_annotated(1, "key", annotated);
  List.iter(annotated.Annotate.node, print_annotated(2, "field name"));
}
and walk_model_entry_unique = annotated => {
  print_annotated(1, "unique", annotated);
  List.iter(annotated.Annotate.node, print_annotated(2, "field name"));
}
and walk_model_entry_index = annotated => print_annotated(1, "index", annotated)
and walk_model_entry_field = annotated => print_annotated(1, "field", annotated)
and walk_model_entry_rel = annotated => print_annotated(1, "rel", annotated)
and walk_crud = annotated => print_annotated(0, "crud", annotated);

let loop = (filename, ()) =>
  Crud_ast.(
    switch (Parse.parse_file(Parse.dbx, filename)) {
    | Ok(defs) => List.iter(defs, ~f=def => walk_definition(def))
    | Error(err) => Parse.print_error(err)
    }
  );

let () = Command.run(Command.basic_spec(~summary="Test", Command.Spec.(empty +> anon("filename" %: file)), loop));