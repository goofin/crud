open Core;

module Model = {
  module Index = {
    [@deriving show]
    type entry =
      | Name(string)
      | Fields(list(string))
      | Unique;

    [@deriving show]
    type t = list(entry);
  };

  module Field = {
    [@deriving show]
    type name = string;

    [@deriving show]
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

    [@deriving show]
    type attr =
      | Column(string)
      | Nullable
      | Updatable
      | Autoinsert
      | Autoupdate
      | Length(string);

    [@deriving show]
    type t = (name, type_, option(list(attr)));
  };

  module Rel = {
    [@deriving show]
    type name = string;

    [@deriving show]
    type model = string;

    [@deriving show]
    type field = string;

    [@deriving show]
    type kind =
      | Setnull
      | Cascade
      | Restrict;

    [@deriving show]
    type attr =
      | Column(string)
      | Nullable
      | Updatable;

    [@deriving show]
    type t = (name, model, field, kind, option(list(attr)));
  };

  [@deriving show]
  type name = string;

  [@deriving show]
  type entry =
    | Table(string)
    | Key(list(string))
    | Unique(list(string))
    | Index(Index.t)
    | Field(Field.t)
    | Rel(Rel.t);

  [@deriving show]
  type t = (name, list(entry));
};

module Create = {
  [@deriving show]
  type model = string;

  [@deriving show]
  type entry =
    | Raw
    | Suffix(string);

  [@deriving show]
  type t = (model, list(entry));
};

[@deriving show]
type definition =
  | Model(Model.t)
  | Create(Create.t);
