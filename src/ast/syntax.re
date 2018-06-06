open Core;

module Model = {
  module Index = {
    [@deriving sexp]
    type entry =
      | Name(Annotate.t(Annotate.t(string)))
      | Fields(Annotate.t(list(Annotate.t(string))))
      | Unique(Annotate.t(unit));

    [@deriving sexp]
    type t = list(entry);
  };

  module Field = {
    [@deriving sexp]
    type name = Annotate.t(string);

    [@deriving sexp]
    type type_ =
      | Serial(Annotate.t(unit))
      | Serial64(Annotate.t(unit))
      | Int(Annotate.t(unit))
      | Int64(Annotate.t(unit))
      | Uint(Annotate.t(unit))
      | Uint64(Annotate.t(unit))
      | Bool(Annotate.t(unit))
      | Text(Annotate.t(unit))
      | Date(Annotate.t(unit))
      | Timestamp(Annotate.t(unit))
      | Utimestamp(Annotate.t(unit))
      | Float(Annotate.t(unit))
      | Float64(Annotate.t(unit))
      | Blob(Annotate.t(unit));

    [@deriving sexp]
    type attr =
      | Column(Annotate.t(Annotate.t(string)))
      | Nullable(Annotate.t(unit))
      | Updatable(Annotate.t(unit))
      | Autoinsert(Annotate.t(unit))
      | Autoupdate(Annotate.t(unit))
      | Length(Annotate.t(Annotate.t(string)));

    [@deriving sexp]
    type t = (name, type_, option(list(attr)));
  };

  module Rel = {
    [@deriving sexp]
    type name = Annotate.t(string);

    [@deriving sexp]
    type model = Annotate.t(string);

    [@deriving sexp]
    type field = Annotate.t(string);

    [@deriving sexp]
    type kind =
      | Setnull(Annotate.t(unit))
      | Cascade(Annotate.t(unit))
      | Restrict(Annotate.t(unit));

    [@deriving sexp]
    type attr =
      | Column(Annotate.t(Annotate.t(string)))
      | Nullable(Annotate.t(unit))
      | Updatable(Annotate.t(unit));

    [@deriving sexp]
    type t = (name, model, field, kind, option(list(attr)));
  };

  [@deriving sexp]
  type name = Annotate.t(string);

  [@deriving sexp]
  type entry =
    | Table(Annotate.t(Annotate.t(string)))
    | Key(Annotate.t(list(Annotate.t(string))))
    | Unique(Annotate.t(list(Annotate.t(string))))
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
      | NotEqual(Annotate.t(unit))
      | LessThan(Annotate.t(unit))
      | LessThanOrEqual(Annotate.t(unit))
      | GreaterThan(Annotate.t(unit))
      | GreaterThanOrEqual(Annotate.t(unit))
      | Equal(Annotate.t(unit))
      | In(Annotate.t(unit));

    [@deriving sexp]
    type value =
      | Placeholder(Annotate.t(unit))
      | Field(Annotate.t(Annotate.t(string)))
      | Literal(Annotate.t(Annotate.t(string)))
      | Call(Annotate.t((Annotate.t(string), value)))
      | Join(Annotate.t((Annotate.t(string), t, Annotate.t(string))))
    [@deriving sexp]
    and t =
      | Term(Annotate.t((value, op, value)))
      | And(Annotate.t((t, t)))
      | Or(Annotate.t((t, t)));
  };

  module Create = {
    [@deriving sexp]
    type attr =
      | Raw(Annotate.t(unit))
      | Suffix(Annotate.t(Annotate.t(string)));

    [@deriving sexp]
    type t = list(attr);
  };

  module Read = {
    [@deriving sexp]
    type kind =
      | Has(Annotate.t(unit))
      | First(Annotate.t(unit))
      | One(Annotate.t(unit))
      | All(Annotate.t(unit))
      | Find(Annotate.t(unit))
      | Limited(Annotate.t(unit))
      | Paged(Annotate.t(unit));

    [@deriving sexp]
    type direction =
      | Ascending(Annotate.t(unit))
      | Descending(Annotate.t(unit));

    [@deriving sexp]
    type attr =
      | Suffix(Annotate.t(Annotate.t(string)))
      | OrderBy(Annotate.t(direction));

    [@deriving sexp]
    type t = (kind, option(Query.t), option(list(attr)));
  };

  module Update = {
    [@deriving sexp]
    type attr =
      | Suffix(Annotate.t(Annotate.t(string)));

    [@deriving sexp]
    type t = (Query.t, option(list(attr)));
  };

  module Delete = {
    [@deriving sexp]
    type attr =
      | Suffix(Annotate.t(Annotate.t(string)));

    [@deriving sexp]
    type t = (Query.t, option(list(attr)));
  };

  [@deriving sexp]
  type model = Annotate.t(string);

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
