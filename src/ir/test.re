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
  printf("%s\n", annotated.loc |> Annotate.sexp_of_location |> Sexplib.Sexp.to_string_hum);
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
  | Model(model) => walk_model(0, model)
  | Crud(crud) => walk_crud(0, crud)
and walk_model = (depth, annotated) => {
  print_annotated(depth, "model", annotated);
  let (name, entries) = annotated.Annotate.node;
  print_annotated(depth + 1, "name", name);
  List.iter(entries, walk_model_entry(depth + 1));
}
and walk_model_entry = depth =>
  fun
  | Model.Table(annotated) => walk_model_entry_table(depth, annotated)
  | Model.Key(annotated) => walk_model_entry_key(depth, annotated)
  | Model.Unique(annotated) => walk_model_entry_unique(depth, annotated)
  | Model.Index(annotated) => walk_model_entry_index(depth, annotated)
  | Model.Field(annotated) => walk_model_entry_field(depth, annotated)
  | Model.Rel(annotated) => walk_model_entry_rel(depth, annotated)
and walk_model_entry_table = (depth, annotated) => {
  print_annotated(depth, "table", annotated);
  print_annotated(depth + 1, "name", annotated.Annotate.node);
}
and walk_model_entry_key = (depth, annotated) => {
  print_annotated(depth, "key", annotated);
  List.iter(annotated.Annotate.node, print_annotated(depth + 1, "field name"));
}
and walk_model_entry_unique = (depth, annotated) => {
  print_annotated(depth, "unique", annotated);
  List.iter(annotated.Annotate.node, print_annotated(depth + 1, "field name"));
}
and walk_model_entry_index = (depth, annotated) => {
  print_annotated(depth, "index", annotated);
  List.iter(annotated.Annotate.node, walk_model_entry_index_entry(depth + 1));
}
and walk_model_entry_index_entry = depth =>
  fun
  | Model.Index.Name(annotated) => walk_model_entry_index_entry_name(depth, annotated)
  | Model.Index.Fields(annotated) => walk_model_entry_index_entry_fields(depth, annotated)
  | Model.Index.Unique(annotated) => walk_model_entry_index_entry_unique(depth, annotated)
and walk_model_entry_index_entry_name = (depth, annotated) => {
  print_annotated(depth, "entry", annotated);
  print_annotated(depth + 1, "name", annotated.Annotate.node);
}
and walk_model_entry_index_entry_fields = (depth, annotated) => {
  print_annotated(depth, "entry", annotated);
  List.iter(annotated.Annotate.node, print_annotated(depth + 1, "field"));
}
and walk_model_entry_index_entry_unique = (depth, annotated) => print_annotated(depth, "entry", annotated)
and walk_model_entry_field = (depth, annotated) => {
  print_annotated(depth, "field", annotated);
  let (name, type_, attrs) = annotated.Annotate.node;
  print_annotated(depth + 1, "name", name);
  walk_model_entry_field_type(depth + 1, type_);
  switch (attrs) {
  | Some(attrs) => List.iter(attrs, walk_model_entry_field_attr(depth + 1))
  | None => ()
  };
}
and walk_model_entry_field_type = depth =>
  fun
  | Model.Field.Serial(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Serial64(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Int(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Int64(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Uint(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Uint64(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Bool(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Text(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Date(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Timestamp(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Utimestamp(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Float(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Float64(annotated) => print_annotated(depth, "type", annotated)
  | Model.Field.Blob(annotated) => print_annotated(depth, "type", annotated)
and walk_model_entry_field_attr = depth =>
  fun
  | Model.Field.Column(annotated) => {
      print_annotated(depth, "attr", annotated);
      print_annotated(depth + 1, "name", annotated.Annotate.node);
    }
  | Model.Field.Length(annotated) => {
      print_annotated(depth, "attr", annotated);
      print_annotated(depth + 1, "length", annotated.Annotate.node);
    }
  | Model.Field.Nullable(annotated) => print_annotated(depth, "attr", annotated)
  | Model.Field.Updatable(annotated) => print_annotated(depth, "attr", annotated)
  | Model.Field.Autoinsert(annotated) => print_annotated(depth, "attr", annotated)
  | Model.Field.Autoupdate(annotated) => print_annotated(depth, "attr", annotated)
and walk_model_entry_rel = (depth, annotated) => {
  print_annotated(depth, "rel", annotated);
  let (name, model, field, kind, attrs) = annotated.Annotate.node;
  print_annotated(depth + 1, "name", name);
  print_annotated(depth + 1, "model", model);
  print_annotated(depth + 1, "field", field);
  walk_model_entry_rel_kind(depth + 1, kind);
  switch (attrs) {
  | Some(attrs) => List.iter(attrs, walk_model_entry_rel_attr(depth + 1))
  | None => ()
  };
}
and walk_model_entry_rel_kind = depth =>
  fun
  | Model.Rel.Setnull(annotated) => print_annotated(depth, "kind", annotated)
  | Model.Rel.Cascade(annotated) => print_annotated(depth, "kind", annotated)
  | Model.Rel.Restrict(annotated) => print_annotated(depth, "kind", annotated)
and walk_model_entry_rel_attr = depth =>
  fun
  | Model.Rel.Column(annotated) => {
      print_annotated(depth, "attr", annotated);
      print_annotated(depth + 1, "name", annotated.Annotate.node);
    }
  | Model.Rel.Nullable(annotated) => print_annotated(depth, "attr", annotated)
  | Model.Rel.Updatable(annotated) => print_annotated(depth, "attr", annotated)
and walk_crud = (depth, annotated) => {
  print_annotated(depth, "crud", annotated);
  let (model, entries) = annotated.Annotate.node;
  print_annotated(depth + 1, "model", model);
  List.iter(entries, walk_crud_entry(depth + 1));
}
and walk_crud_entry = depth =>
  fun
  | Crud.Create(annotated) => walk_crud_entry_create(depth, annotated)
  | Crud.Read(annotated) => walk_crud_entry_read(depth, annotated)
  | Crud.Update(annotated) => walk_crud_entry_update(depth, annotated)
  | Crud.Delete(annotated) => walk_crud_entry_delete(depth, annotated)
and walk_crud_query = depth =>
  fun
  | Crud.Query.Term(annotated) => walk_crud_query_term(depth, annotated)
  | Crud.Query.And(annotated) => walk_crud_query_and(depth, annotated)
  | Crud.Query.Or(annotated) => walk_crud_query_or(depth, annotated)
and walk_crud_query_term = (depth, annotated) => {
  print_annotated(depth, "term", annotated);
  let (left, op, right) = annotated.Annotate.node;
  walk_crud_query_value(depth + 1, left);
  walk_crud_query_op(depth + 1, op);
  walk_crud_query_value(depth + 1, right);
}
and walk_crud_query_value = depth =>
  fun
  | Crud.Query.Placeholder(annotated) => print_annotated(depth, "value (placeholder)", annotated)
  | Crud.Query.Field(annotated) => {
      print_annotated(depth, "value (field)", annotated);
      print_annotated(depth + 1, "name", annotated.Annotate.node);
    }
  | Crud.Query.Literal(annotated) => {
      print_annotated(depth, "value (literal)", annotated);
      print_annotated(depth + 1, "value", annotated.Annotate.node);
    }
  | Crud.Query.Call(annotated) => {
      print_annotated(depth, "value (call)", annotated);
      let (name, value) = annotated.Annotate.node;
      print_annotated(depth + 1, "name", name);
      walk_crud_query_value(depth + 1, value);
    }
  | Crud.Query.Join(annotated) => {
      print_annotated(depth, "value (join)", annotated);
      let (model, query, field) = annotated.Annotate.node;
      print_annotated(depth + 1, "model", model);
      print_annotated(depth + 1, "field", field);
      walk_crud_query(depth + 1, query);
    }
and walk_crud_query_op = depth =>
  fun
  | Crud.Query.NotEqual(annotated) => print_annotated(depth, "op", annotated)
  | Crud.Query.LessThan(annotated) => print_annotated(depth, "op", annotated)
  | Crud.Query.LessThanOrEqual(annotated) => print_annotated(depth, "op", annotated)
  | Crud.Query.GreaterThan(annotated) => print_annotated(depth, "op", annotated)
  | Crud.Query.GreaterThanOrEqual(annotated) => print_annotated(depth, "op", annotated)
  | Crud.Query.Equal(annotated) => print_annotated(depth, "op", annotated)
  | Crud.Query.In(annotated) => print_annotated(depth, "op", annotated)
and walk_crud_query_and = (depth, annotated) => {
  print_annotated(depth, "and", annotated);
  let (left, right) = annotated.Annotate.node;
  walk_crud_query(depth + 1, left);
  walk_crud_query(depth + 1, right);
}
and walk_crud_query_or = (depth, annotated) => {
  print_annotated(depth, "or", annotated);
  let (left, right) = annotated.Annotate.node;
  walk_crud_query(depth + 1, left);
  walk_crud_query(depth + 1, right);
}
and walk_crud_entry_create = (depth, annotated) => {
  print_annotated(depth, "create", annotated);
  List.iter(annotated.Annotate.node, walk_crud_entry_create_attr(depth + 1));
}
and walk_crud_entry_create_attr = depth =>
  fun
  | Crud.Create.Raw(annotated) => print_annotated(depth, "attr", annotated)
  | Crud.Create.Suffix(annotated) => {
      print_annotated(depth, "attr", annotated);
      print_annotated(depth + 1, "suffix", annotated.Annotate.node);
    }
and walk_crud_entry_read = (depth, annotated) => {
  print_annotated(depth, "read", annotated);
  let (kind, query, attrs) = annotated.Annotate.node;
  walk_crud_entry_read_kind(depth + 1, kind);
  switch (query) {
  | Some(query) => walk_crud_query(depth + 1, query)
  | None => ()
  };
  switch (attrs) {
  | Some(attrs) => List.iter(attrs, walk_crud_entry_read_attr(depth + 1))
  | None => ()
  };
}
and walk_crud_entry_read_kind = depth =>
  fun
  | Crud.Read.Has(annotated) => print_annotated(depth, "kind", annotated)
  | Crud.Read.First(annotated) => print_annotated(depth, "kind", annotated)
  | Crud.Read.One(annotated) => print_annotated(depth, "kind", annotated)
  | Crud.Read.All(annotated) => print_annotated(depth, "kind", annotated)
  | Crud.Read.Find(annotated) => print_annotated(depth, "kind", annotated)
  | Crud.Read.Limited(annotated) => print_annotated(depth, "kind", annotated)
  | Crud.Read.Paged(annotated) => print_annotated(depth, "kind", annotated)
and walk_crud_entry_read_attr = depth =>
  fun
  | Crud.Read.Suffix(annotated) => {
      print_annotated(depth, "attr", annotated);
      print_annotated(depth + 1, "suffix", annotated.Annotate.node);
    }
  | Crud.Read.OrderBy(annotated) => {
      print_annotated(depth, "attr", annotated);
      walk_crud_entry_read_attr_direction(depth + 1, annotated.Annotate.node);
    }
and walk_crud_entry_read_attr_direction = depth =>
  fun
  | Crud.Read.Ascending(annotated) => print_annotated(depth, "direction", annotated)
  | Crud.Read.Descending(annotated) => print_annotated(depth, "direction", annotated)
and walk_crud_entry_update = (depth, annotated) => {
  print_annotated(depth, "update", annotated);
  let (query, attrs) = annotated.Annotate.node;
  walk_crud_query(depth + 1, query);
  switch (attrs) {
  | Some(attrs) => List.iter(attrs, walk_crud_entry_update_attr(depth + 1))
  | None => ()
  };
}
and walk_crud_entry_update_attr = depth =>
  fun
  | Crud.Update.Suffix(annotated) => {
      print_annotated(depth, "attr", annotated);
      print_annotated(depth + 1, "name", annotated.Annotate.node);
    }
and walk_crud_entry_delete = (depth, annotated) => {
  print_annotated(depth, "delete", annotated);
  let (query, attrs) = annotated.Annotate.node;
  walk_crud_query(depth + 1, query);
  switch (attrs) {
  | Some(attrs) => List.iter(attrs, walk_crud_entry_delete_attr(depth + 1))
  | None => ()
  };
}
and walk_crud_entry_delete_attr = depth =>
  fun
  | Crud.Delete.Suffix(annotated) => {
      print_annotated(depth, "attr", annotated);
      print_annotated(depth + 1, "name", annotated.Annotate.node);
    };

let loop = (filename, ()) =>
  Crud_ast.(
    switch (Parse.parse_file(Parse.dbx, filename)) {
    | Ok(defs) => List.iter(defs, ~f=def => walk_definition(def))
    | Error(err) => Parse.print_error(err)
    }
  );

let () = Command.run(Command.basic_spec(~summary="Test", Command.Spec.(empty +> anon("filename" %: file)), loop));
