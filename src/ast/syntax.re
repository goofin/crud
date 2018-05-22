open Core;

module Model = {
  module Index = {
    [@deriving sexp]
    type entry =
      | Name(string)
      | Fields(list(string))
      | Unique;

    [@deriving sexp]
    type t = list(entry);
  };

  module Field = {
    [@deriving sexp]
    type name = string;

    [@deriving sexp]
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

    [@deriving sexp]
    type attr =
      | Column(string)
      | Nullable
      | Updatable
      | Autoinsert
      | Autoupdate
      | Length(string);

    [@deriving sexp]
    type t = (name, type_, option(list(attr)));
  };

  module Rel = {
    [@deriving sexp]
    type name = string;

    [@deriving sexp]
    type model = string;

    [@deriving sexp]
    type field = string;

    [@deriving sexp]
    type kind =
      | Setnull
      | Cascade
      | Restrict;

    [@deriving sexp]
    type attr =
      | Column(string)
      | Nullable
      | Updatable;

    [@deriving sexp]
    type t = (name, model, field, kind, option(list(attr)));
  };

  [@deriving sexp]
  type name = string;

  [@deriving sexp]
  type entry =
    | Table(string)
    | Key(list(string))
    | Unique(list(string))
    | Index(Index.t)
    | Field(Field.t)
    | Rel(Rel.t);

  [@deriving sexp]
  type t = (name, list(entry));
};

module Crud = {
  module Query = {
    [@deriving sexp]
    type op =
      | NotEqual
      | LessThan
      | LessThanOrEqual
      | GreaterThan
      | GreaterThanOrEqual
      | Equal
      | In;

    [@deriving sexp]
    type value =
      | Placeholder
      | Field(string)
      | Literal(string)
      | Call(string, value)
      | Join(string, t, string)

    [@deriving sexp]
    and t =
      | Term(value, op, value)
      | And(t, t)
      | Or(t, t);
  };

  module Create = {
    [@deriving sexp]
    type entry =
      | Raw
      | Suffix(string);

    [@deriving sexp]
    type t = list(entry);
  };

  module Read = {
    [@deriving sexp]
    type kind =
      | Has
      | First
      | One
      | All
      | Find
      | Limited
      | Paged;

    [@deriving sexp]
    type attr =
      | Suffix(string);

    [@deriving sexp]
    type t = (kind, Query.t, option(list(attr)));
  };

  module Update = {
    [@deriving sexp]
    type attr =
      | Suffix(string);

    [@deriving sexp]
    type t = (Query.t, option(list(attr)));
  };

  module Delete = {
    [@deriving sexp]
    type attr =
      | Suffix(string);

    [@deriving sexp]
    type t = (Query.t, option(list(attr)));
  };

  [@deriving sexp]
  type model = string;

  [@deriving sexp]
  type entry =
    | Create(Create.t)
    | Read(Read.t)
    | Update(Update.t)
    | Delete(Delete.t);

  [@deriving sexp]
  type t = (model, list(entry));
};

[@deriving sexp]
type definition =
  | Model(Model.t)
  | Crud(Crud.t);