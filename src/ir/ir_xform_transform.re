open Core;
open Crud_ast;
open Ir_xform_hashes;

include Ir_xform_transform_types;

let rec find_and_xform_field = (defs, t, model, field) =>
  switch (Ir_xform_defs.find_field(defs, model^.Model.name, field)) {
  | Field(field) => xform_field(defs, t, model, field)
  | Rel(rel) => xform_rel(defs, t, model, rel)
  }
and find_and_xform_model = (defs, t, model) =>
  xform_model(defs, t, Ir_xform_defs.find_model(defs, model))
and xform_model = (defs, t, model) => {
  let {Syntax.Model.name: {node: name}, entries} = model;

  let do_xform_model = () => {
    let entries_set = Ir_xform_dupes.create("model entry");
    let fields_set = Ir_xform_dupes.create("field");

    let model =
      ref({Model.name, fields: StringHash.create(), table: None, key: [], unique: [], index: []});
    StringHash.set(t.models, name, model);

    List.iter(entries, ~f=({Annotate.loc, node}) =>
      switch (node) {
      | Syntax.Model.Table(table) =>
        Ir_xform_dupes.check(entries_set, "table", loc);
        model := {...model^, table: Some(table.node)};
      | Key(key) =>
        Ir_xform_dupes.check(entries_set, "key", loc);
        let fields = List.map(key, find_and_xform_field(defs, t, model));
        model := {...model^, key: fields};
      | Unique(unique) =>
        let fields = List.map(unique, find_and_xform_field(defs, t, model));
        model := {...model^, unique: [fields, ...model^.unique]};
      | Index(index) =>
        let fields = List.map(index, find_and_xform_field(defs, t, model));
        model := {...model^, index: [fields, ...model^.index]};
      | Field({name: {node: field_name}} as field) =>
        Ir_xform_dupes.check(fields_set, field_name, loc);
        let field = xform_field(defs, t, model, field);
        StringHash.set(model^.fields, field_name, field);
      | Rel({name: {node: rel_name}} as rel) =>
        Ir_xform_dupes.check(fields_set, rel_name, loc);
        let rel = xform_rel(defs, t, model, rel);
        StringHash.set(model^.fields, rel_name, rel);
      }
    );

    model;
  };

  switch (StringHash.find(t.models, name)) {
  | Some(model) => model
  | None => do_xform_model()
  };
}
and xform_field = (defs, t, parent, field) => {
  let {Syntax.Field.name: {node: name}, type_: {node: type_}, attrs} = field;

  let do_xform_field = () => {
    let entries_set = Ir_xform_dupes.create("field attribute");

    let field_ =
      ref({
        Field.parent,
        name,
        type_,
        column: None,
        nullable: false,
        updatable: false,
        autoinsert: false,
        autoupdate: false,
        length: None,
      });
    let field = ref(Model.Field(field_^));
    FieldHash.set(t.fields, (parent^.name, name), field);

    List.iter(
      attrs,
      ~f=({Annotate.loc, node}) => {
        switch (node) {
        | Syntax.Field.Column({node: col}) =>
          Ir_xform_dupes.check(entries_set, "column", loc);
          field_ := {...field_^, column: Some(col)};
        | Nullable =>
          Ir_xform_dupes.check(entries_set, "nullable", loc);
          field_ := {...field_^, nullable: true};
        | Updatable =>
          Ir_xform_dupes.check(entries_set, "updatable", loc);
          field_ := {...field_^, updatable: true};
        | Autoinsert =>
          Ir_xform_dupes.check(entries_set, "autoinsert", loc);
          field_ := {...field_^, autoinsert: true};
        | Autoupdate =>
          Ir_xform_dupes.check(entries_set, "autoupdate", loc);
          field_ := {...field_^, autoupdate: true};
        | Length({node: length, loc}) =>
          Ir_xform_dupes.check(entries_set, "length", loc);
          switch (Int.of_string(length)) {
          | length => field_ := {...field_^, length: Some(length)}
          | exception _ => raise(Ir_error.Exn(Invalid(loc)))
          };
        };
        field := Field(field_^);
      },
    );

    field;
  };

  switch (FieldHash.find(t.fields, (parent^.name, name))) {
  | Some(field) => field
  | None => do_xform_field()
  };
}
and xform_rel = (defs, t, parent, rel) => {
  let {Syntax.Rel.name: {node: name}, model, field, kind: {node: kind}, attrs} = rel;

  let do_xform_rel = () => {
    let entries_set = Ir_xform_dupes.create("relation attribute");

    let model = find_and_xform_model(defs, t, model);
    let field = find_and_xform_field(defs, t, model, field);

    let rel_ =
      ref({
        Rel.parent,
        name,
        model,
        field,
        kind,
        column: None,
        nullable: false,
        updatable: false,
      });
    let rel = ref(Model.Rel(rel_^));
    FieldHash.set(t.fields, (parent^.name, name), rel);

    List.iter(
      attrs,
      ~f=({Annotate.loc, node}) => {
        switch (node) {
        | Syntax.Rel.Column({node: col}) =>
          Ir_xform_dupes.check(entries_set, "column", loc);
          rel_ := {...rel_^, column: Some(col)};
        | Nullable =>
          Ir_xform_dupes.check(entries_set, "nullable", loc);
          rel_ := {...rel_^, nullable: true};
        | Updatable =>
          Ir_xform_dupes.check(entries_set, "updatable", loc);
          rel_ := {...rel_^, updatable: true};
        };
        rel := Rel(rel_^);
      },
    );

    rel;
  };

  switch (FieldHash.find(t.fields, (parent^.name, name))) {
  | Some(rel) => rel
  | _ => do_xform_rel()
  };
}
and xform_crud = (defs, t, crud) => ();

let xform_def = (defs, t, {Annotate.node: def}) =>
  switch (def) {
  | Syntax.Model(model) => ignore(xform_model(defs, t, model))
  | Syntax.Crud(crud) => ignore(xform_crud(defs, t, crud))
  };

let xform_defs = defs => {
  let t = {models: StringHash.create(), fields: FieldHash.create()};
  switch (List.iter(defs, ~f=xform_def(Ir_xform_defs.create(defs), t))) {
  | () => Ok(t)
  | exception (Ir_error.Exn(err)) => Error(err)
  };
};
