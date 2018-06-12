open Core;

module Field = {
  type type_ =
    | Serial
    | Serial64
    | Int
    | Int64
    | Uint
    | Uint64
    | Bool
    | Text
    | Date
    | Timestamp
    | Utimestamp
    | Float
    | Float64
    | Blob;

  type attr =
    | Column(Ast_annotate.t(string))
    | Nullable
    | Updatable
    | Autoinsert
    | Autoupdate
    | Length(Ast_annotate.t(string));

  type t = {
    name: Ast_annotate.t(string),
    type_: Ast_annotate.t(type_),
    attrs: list(Ast_annotate.t(attr)),
  };
};

module Rel = {
  type kind =
    | Setnull
    | Cascade
    | Restrict;

  type attr =
    | Column(Ast_annotate.t(string))
    | Nullable
    | Updatable;

  type t = {
    name: Ast_annotate.t(string),
    model: Ast_annotate.t(string),
    field: Ast_annotate.t(string),
    kind: Ast_annotate.t(kind),
    attrs: list(Ast_annotate.t(attr)),
  };
};

module Model = {
  type entry =
    | Table(Ast_annotate.t(string))
    | Key(list(Ast_annotate.t(string)))
    | Unique(list(Ast_annotate.t(string)))
    | Index(list(Ast_annotate.t(string)))
    | Field(Field.t)
    | Rel(Rel.t);

  type t = {
    name: Ast_annotate.t(string),
    entries: list(Ast_annotate.t(entry)),
  };
};

module Query = {
  type op =
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equal
    | In;

  type term = {
    left_val: Ast_annotate.t(value),
    op: Ast_annotate.t(op),
    right_val: Ast_annotate.t(value),
  }
  and binop = {
    left_query: Ast_annotate.t(t),
    right_query: Ast_annotate.t(t),
  }
  and t =
    | Term(term)
    | And(binop)
    | Or(binop)
  and value =
    | Placeholder
    | Field(Ast_annotate.t(string))
    | Literal(Ast_annotate.t(string))
    | Call(Ast_annotate.t(string), Ast_annotate.t(value))
    | Join(Ast_annotate.t(string), Ast_annotate.t(t), Ast_annotate.t(string));
};

module Create = {
  type attr =
    | Raw
    | Suffix(Ast_annotate.t(string));

  type t = {attrs: list(Ast_annotate.t(attr))};
};

module Read = {
  type kind =
    | Has
    | First
    | One
    | All
    | Find
    | Limited
    | Paged;

  type direction =
    | Ascending
    | Descending;

  type attr =
    | Suffix(Ast_annotate.t(string))
    | OrderBy(direction);

  type t = {
    kind: Ast_annotate.t(kind),
    query: option(Ast_annotate.t(Query.t)),
    attrs: list(Ast_annotate.t(attr)),
  };
};

module Update = {
  type attr =
    | Suffix(Ast_annotate.t(string));

  type t = {
    query: Ast_annotate.t(Query.t),
    attrs: list(Ast_annotate.t(attr)),
  };
};

module Delete = {
  type attr =
    | Suffix(Ast_annotate.t(string));

  type t = {
    query: Ast_annotate.t(Query.t),
    attrs: list(Ast_annotate.t(attr)),
  };
};

module Crud = {
  type entry =
    | Create(Create.t)
    | Read(Read.t)
    | Update(Update.t)
    | Delete(Delete.t);

  type t = {
    model: Ast_annotate.t(string),
    entries: list(Ast_annotate.t(entry)),
  };
};

type definition =
  | Model(Model.t)
  | Crud(Crud.t);
