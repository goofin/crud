open Core;
open Crud_ast;
open Ir_xform_hashes;

module rec Field: {
  type t = {
    parent: ref(Model.t),
    name: string,
    type_: Syntax.Field.type_,
    column: option(string),
    nullable: bool,
    updatable: bool,
    autoinsert: bool,
    autoupdate: bool,
    length: option(int),
  }
} = Field
and Rel: {
  type t = {
    parent: ref(Model.t),
    name: string,
    model: ref(Model.t),
    field: ref(Model.field),
    kind: Syntax.Rel.kind,
    column: option(string),
    nullable: bool,
    updatable: bool,
  }
} = Rel
and Model: {
  type field =
    | Field(Field.t)
    | Rel(Rel.t);

  type t = {
    name: string,
    fields: StringHash.t(ref(field)),
    table: option(string),
    key: list(ref(field)),
    unique: list(list(ref(field))),
    index: list(list(ref(field))),
  }
} = Model;

type t = {
  models: StringHash.t(ref(Model.t)),
  fields: FieldHash.t(ref(Model.field)),
};
