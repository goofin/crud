open Core;

module Annotate = {
  [@deriving sexp]
  type location = {
    file: string,
    start_line: int,
    start_pos: int,
    end_line: int,
    end_pos: int,
  };

  [@deriving sexp]
  type t('a) = {
    node: 'a,
    id: int,
    loc: location,
  };

  /* TODO(jeff): gotta be a way to avoid this type hanging around */
  [@deriving sexp]
  type string_ = t(string);

  [@deriving sexp]
  type string = string_;
};

module Model = {
  module Index = {
    [@deriving sexp]
    type entry =
      | Name(Annotate.string)
      | Fields(list(Annotate.string))
      | Unique;

    [@deriving sexp]
    type t = list(Annotate.t(entry));
  };

  module Field = {
    [@deriving sexp]
    type name = Annotate.string;

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
      | Column(Annotate.string)
      | Nullable
      | Updatable
      | Autoinsert
      | Autoupdate
      | Length(Annotate.string);

    [@deriving sexp]
    type t = (name, Annotate.t(type_), option(list(Annotate.t(attr))));
  };

  module Rel = {
    [@deriving sexp]
    type name = Annotate.string;

    [@deriving sexp]
    type model = Annotate.string;

    [@deriving sexp]
    type field = Annotate.string;

    [@deriving sexp]
    type kind =
      | Setnull
      | Cascade
      | Restrict;

    [@deriving sexp]
    type attr =
      | Column(Annotate.string)
      | Nullable
      | Updatable;

    [@deriving sexp]
    type t = (name, model, field, Annotate.t(kind), option(list(Annotate.t(attr))));
  };

  [@deriving sexp]
  type name = Annotate.string;

  [@deriving sexp]
  type entry =
    | Table(Annotate.t(Annotate.string))
    | Key(Annotate.t(list(Annotate.string)))
    | Unique(Annotate.t(list(Annotate.string)))
    | Index(Annotate.t(Index.t))
    | Field(Annotate.t(Field.t))
    | Rel(Annotate.t(Rel.t));

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
      | Field(Annotate.string)
      | Literal(Annotate.string)
      | Call(Annotate.string, Annotate.t(value))
      | Join(Annotate.string, Annotate.t(t), Annotate.string)
    [@deriving sexp]
    and t =
      | Term(Annotate.t(value), Annotate.t(op), Annotate.t(value))
      | And(Annotate.t(t), Annotate.t(t))
      | Or(Annotate.t(t), Annotate.t(t));
  };

  module Create = {
    [@deriving sexp]
    type attr =
      | Raw
      | Suffix(Annotate.string);

    [@deriving sexp]
    type t = list(Annotate.t(attr));
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
    type direction =
      | Ascending
      | Descending;

    [@deriving sexp]
    type attr =
      | Suffix(Annotate.string)
      | OrderBy(Annotate.t(direction));

    [@deriving sexp]
    type t = (Annotate.t(kind), option(Annotate.t(Query.t)), option(list(Annotate.t(attr))));
  };

  module Update = {
    [@deriving sexp]
    type attr =
      | Suffix(Annotate.string);

    [@deriving sexp]
    type t = (Annotate.t(Query.t), option(list(Annotate.t(attr))));
  };

  module Delete = {
    [@deriving sexp]
    type attr =
      | Suffix(Annotate.string);

    [@deriving sexp]
    type t = (Annotate.t(Query.t), option(list(Annotate.t(attr))));
  };

  [@deriving sexp]
  type model = Annotate.string;

  [@deriving sexp]
  type entry =
    | Create(Create.t)
    | Read(Read.t)
    | Update(Update.t)
    | Delete(Delete.t);

  [@deriving sexp]
  type t = (model, list(Annotate.t(entry)));
};

[@deriving sexp]
type definition =
  | Model(Annotate.t(Model.t))
  | Crud(Annotate.t(Crud.t));