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
  };
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
  };
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
    cruds: list(ref(Crud.entry)),
  };
} = Model
and Query: {
  type term = {
    t_left: ref(value),
    t_op: Syntax.Query.op,
    t_right: ref(value),
  }
  and binop = {
    b_left: ref(t),
    b_right: ref(t),
  }
  and t =
    | Term(term)
    | And(binop)
    | Or(binop)
  and value =
    | Placeholder
    | Field(ref(Model.field))
    | Literal(string)
    | Call(string, ref(value))
    | Join(ref(Model.t), ref(t), ref(Model.field));
} = Query
and Create: {
  type t = {
    parent: ref(Crud.t),
    raw: bool,
    suffix: option(string),
  };
} = Create
and Read: {
  type t = {
    parent: ref(Crud.t),
    kind: Syntax.Read.kind,
    query: option(ref(Query.t)),
    suffix: option(string),
    order_by: option(Syntax.Read.direction),
  };
} = Read
and Update: {
  type t = {
    parent: ref(Crud.t),
    query: ref(Query.t),
    suffix: option(string),
  };
} = Update
and Delete: {
  type t = {
    parent: ref(Crud.t),
    query: ref(Query.t),
    suffix: option(string),
  };
} = Delete
and Crud: {
  type entry =
    | Create(Create.t)
    | Read(Read.t)
    | Update(Update.t)
    | Delete(Delete.t);

  type t = {
    model: ref(Model.t),
    entries: list(ref(entry)),
  };
} = Crud;

type t = {
  models: StringHash.t(ref(Model.t)),
  fields: FieldHash.t(ref(Model.field)),
};
