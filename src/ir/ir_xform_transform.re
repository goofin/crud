open Core;
open Crud_ast;
open Ir_xform_hashes;

include Ir_xform_transform_types;

let rec find_and_xform_field = (defs, t, model, field) =>
  switch (Ir_xform_defs.find_field(defs, model^.m_name, field)) {
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
      ref({
        m_name: name,
        m_fields: StringHash.create(),
        m_table: None,
        m_key: [],
        m_unique: [],
        m_index: [],
      });
    StringHash.set(t.models, name, model);

    List.iter(entries, ~f=({Annotate.loc, node}) =>
      switch (node) {
      | Syntax.Model.Table(table) =>
        Ir_xform_dupes.check(entries_set, "table", loc);
        model := {...model^, m_table: Some(table.node)};
      | Key(key) =>
        Ir_xform_dupes.check(entries_set, "key", loc);
        let fields = List.map(key, find_and_xform_field(defs, t, model));
        model := {...model^, m_key: fields};
      | Unique(unique) =>
        let fields = List.map(unique, find_and_xform_field(defs, t, model));
        model := {...model^, m_unique: [fields, ...model^.m_unique]}
      | Index(index) =>
        let fields = List.map(index, find_and_xform_field(defs, t, model));
        model := {...model^, m_index: [fields, ...model^.m_index]}
      | Field({name: {node: field_name}} as field) =>
        Ir_xform_dupes.check(fields_set, field_name, loc);
        let field = xform_field(defs, t, model, field);
        StringHash.set(model^.m_fields, field_name, field);
      | Rel({name: {node: rel_name}} as rel) =>
        Ir_xform_dupes.check(fields_set, rel_name, loc);
        let rel = xform_rel(defs, t, model, rel);
        StringHash.set(model^.m_fields, rel_name, rel);
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
  let {Syntax.Model.Field.name: {node: name}, type_: {node: type_}, attrs} = field;

  let do_xform_field = () => {
    let entries_set = Ir_xform_dupes.create("field attribute");

    let field_ =
      ref({
        f_parent: parent,
        f_name: name,
        f_type: type_,
        f_column: None,
        f_nullable: false,
        f_updatable: false,
        f_autoinsert: false,
        f_autoupdate: false,
        f_length: None,
      });
    let field = ref(Field(field_^));
    FieldHash.set(t.fields, (parent^.m_name, name), field);

    List.iter(attrs, ~f=({Annotate.loc, node}) =>
      switch (node) {
      | Syntax.Model.Field.Column({node: col}) =>
        Ir_xform_dupes.check(entries_set, "column", loc)
      | Nullable => Ir_xform_dupes.check(entries_set, "nullable", loc)
      | Updatable => Ir_xform_dupes.check(entries_set, "updatable", loc)
      | Autoinsert => Ir_xform_dupes.check(entries_set, "autoinsert", loc)
      | Autoupdate => Ir_xform_dupes.check(entries_set, "autoupdate", loc)
      | Length({node: length}) => Ir_xform_dupes.check(entries_set, "length", loc)
      }
    );

    field;
  };

  switch (FieldHash.find(t.fields, (parent^.m_name, name))) {
  | Some(field) => field
  | None => do_xform_field()
  };
}
and xform_rel = (defs, t, parent, rel) => {
  let {Syntax.Model.Rel.name: {node: name}, model, field, kind: {node: kind}, attrs} = rel;

  let do_xform_rel = () => {
    let entries_set = Ir_xform_dupes.create("relation attribute");

    let model = find_and_xform_model(defs, t, model);
    let field = find_and_xform_field(defs, t, model, field);

    let rel_ =
      ref({
        r_parent: parent,
        r_name: name,
        r_model: model,
        r_field: field,
        r_kind: kind,
        r_column: None,
        r_nullable: false,
        r_updatable: false,
      });
    let rel = ref(Rel(rel_^));
    FieldHash.set(t.fields, (parent^.m_name, name), rel);

    List.iter(attrs, ~f=({Annotate.loc, node}) =>
      switch (node) {
      | Syntax.Model.Rel.Column({node: col}) =>
        Ir_xform_dupes.check(entries_set, "column", loc);
        rel := Rel({...rel_^, r_column: Some(col)});
      | Nullable =>
        Ir_xform_dupes.check(entries_set, "nullable", loc);
        rel := Rel({...rel_^, r_nullable: true});
      | Updatable =>
        Ir_xform_dupes.check(entries_set, "updatable", loc);
        rel := Rel({...rel_^, r_updatable: true});
      }
    );

    rel;
  };

  switch (FieldHash.find(t.fields, (parent^.m_name, name))) {
  | Some(rel) => rel
  | _ => do_xform_rel()
  };
};

let xform_def = (defs, t, {Annotate.node: def}) =>
  switch (def) {
  | Syntax.Model(model) =>
    let _ = xform_model(defs, t, model);
    ();
  | _ => ()
  };

let xform_defs = defs => {
  let t = {models: StringHash.create(), fields: FieldHash.create()};
  let x = Ir_xform_defs.create(defs);
  try (
    {
      List.iter(defs, ~f=xform_def(x, t));
      Ok(t);
    }
  ) {
  | Ir_error.Exn(err) => Error(err)
  };
};
