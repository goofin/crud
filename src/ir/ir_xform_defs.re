open Core;
open Crud_ast;
open Ir_xform_hashes;

type field =
  | Field(Syntax.Model.Field.t)
  | Rel(Syntax.Model.Rel.t);

type t = {
  models: StringHash.t(Syntax.Model.t),
  fields: FieldHash.t(field),
};

let create = defs => {
  let out = {models: StringHash.create(), fields: FieldHash.create()};

  let walk_model = ({Syntax.Model.name: {node: model}, entries} as node) => {
    StringHash.set(out.models, model, node);
    List.iter(entries, ~f=({Annotate.loc, node}) =>
      switch (node) {
      | Field({name: {node: field}} as node) =>
        FieldHash.set(out.fields, (model, field), Field(node))
      | Rel({name: {node: rel}} as node) =>
        FieldHash.set(out.fields, (model, rel), Rel(node))

      | Table(_)
      | Key(_)
      | Unique(_)
      | Index(_) => ()
      }
    );
  };

  List.iter(defs, ~f=({Annotate.loc, node}) =>
    switch (node) {
    | Syntax.Model(model) => walk_model(model)
    | Syntax.Crud(crud) => ()
    }
  );

  out;
};

let find_model = (t, {Annotate.node: model, loc}) =>
  switch (StringHash.find(t.models, model)) {
  | None => raise(Ir_error.Exn(Undefined("model", loc)))
  | Some(model) => model
  };

let find_field = (t, model, {Annotate.node: field, loc}) =>
  switch (FieldHash.find(t.fields, (model, field))) {
  | None => raise(Ir_error.Exn(Undefined("field", loc)))
  | Some(field) => field
  };
