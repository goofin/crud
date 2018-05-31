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

  /* TODO(jeff): gotta be a way to avoid this type hanging around */
  [@deriving sexp]
  type unit_ = t(unit);

  [@deriving sexp]
  type unit = unit_;
};

module Model = {
  module Index = {
    [@deriving sexp]
    type entry =
      | Name(Annotate.t(Annotate.string))
      | Fields(Annotate.t(list(Annotate.string)))
      | Unique(Annotate.unit);

    [@deriving sexp]
    type t = list(entry);
  };

  module Field = {
    [@deriving sexp]
    type name = Annotate.string;

    [@deriving sexp]
    type type_ =
      | Serial(Annotate.unit)
      | Serial64(Annotate.unit)
      | Int(Annotate.unit)
      | Int64(Annotate.unit)
      | Uint(Annotate.unit)
      | Uint64(Annotate.unit)
      | Bool(Annotate.unit)
      | Text(Annotate.unit)
      | Date(Annotate.unit)
      | Timestamp(Annotate.unit)
      | Utimestamp(Annotate.unit)
      | Float(Annotate.unit)
      | Float64(Annotate.unit)
      | Blob(Annotate.unit);

    [@deriving sexp]
    type attr =
      | Column(Annotate.t(Annotate.string))
      | Nullable(Annotate.unit)
      | Updatable(Annotate.unit)
      | Autoinsert(Annotate.unit)
      | Autoupdate(Annotate.unit)
      | Length(Annotate.t(Annotate.string));

    [@deriving sexp]
    type t = (name, type_, option(list(attr)));
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
      | Setnull(Annotate.unit)
      | Cascade(Annotate.unit)
      | Restrict(Annotate.unit);

    [@deriving sexp]
    type attr =
      | Column(Annotate.t(Annotate.string))
      | Nullable(Annotate.unit)
      | Updatable(Annotate.unit);

    [@deriving sexp]
    type t = (name, model, field, kind, option(list(attr)));
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
      | NotEqual(Annotate.unit)
      | LessThan(Annotate.unit)
      | LessThanOrEqual(Annotate.unit)
      | GreaterThan(Annotate.unit)
      | GreaterThanOrEqual(Annotate.unit)
      | Equal(Annotate.unit)
      | In(Annotate.unit);

    [@deriving sexp]
    type value =
      | Placeholder(Annotate.unit)
      | Field(Annotate.t(Annotate.string))
      | Literal(Annotate.t(Annotate.string))
      | Call(Annotate.t((Annotate.string, value)))
      | Join(Annotate.t((Annotate.string, t, Annotate.string)))
    [@deriving sexp]
    and t =
      | Term(Annotate.t((value, op, value)))
      | And(Annotate.t((t, t)))
      | Or(Annotate.t((t, t)));
  };

  module Create = {
    [@deriving sexp]
    type attr =
      | Raw(Annotate.unit)
      | Suffix(Annotate.t(Annotate.string));

    [@deriving sexp]
    type t = list(attr);
  };

  module Read = {
    [@deriving sexp]
    type kind =
      | Has(Annotate.unit)
      | First(Annotate.unit)
      | One(Annotate.unit)
      | All(Annotate.unit)
      | Find(Annotate.unit)
      | Limited(Annotate.unit)
      | Paged(Annotate.unit);

    [@deriving sexp]
    type direction =
      | Ascending(Annotate.unit)
      | Descending(Annotate.unit);

    [@deriving sexp]
    type attr =
      | Suffix(Annotate.t(Annotate.string))
      | OrderBy(Annotate.t(direction));

    [@deriving sexp]
    type t = (kind, option(Query.t), option(list(attr)));
  };

  module Update = {
    [@deriving sexp]
    type attr =
      | Suffix(Annotate.t(Annotate.string));

    [@deriving sexp]
    type t = (Query.t, option(list(attr)));
  };

  module Delete = {
    [@deriving sexp]
    type attr =
      | Suffix(Annotate.t(Annotate.string));

    [@deriving sexp]
    type t = (Query.t, option(list(attr)));
  };

  [@deriving sexp]
  type model = Annotate.string;

  [@deriving sexp]
  type entry =
    | Create(Annotate.t(Create.t))
    | Read(Annotate.t(Read.t))
    | Update(Annotate.t(Update.t))
    | Delete(Annotate.t(Delete.t));

  [@deriving sexp]
  type t = (model, list(entry));
};

[@deriving sexp]
type definition =
  | Model(Annotate.t(Model.t))
  | Crud(Annotate.t(Crud.t));
