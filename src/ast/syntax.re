open Core;

type ann('t) = Annotate.t('t);
type aunit = ann(unit);
type astring = ann(string);

module Model = {
  module Index = {
    type entry =
      | Name(ann(astring))
      | Fields(ann(list(astring)))
      | Unique(aunit);

    type t = list(entry);
  };

  module Field = {
    type name = astring;

    type type_ =
      | Serial(aunit)
      | Serial64(aunit)
      | Int(aunit)
      | Int64(aunit)
      | Uint(aunit)
      | Uint64(aunit)
      | Bool(aunit)
      | Text(aunit)
      | Date(aunit)
      | Timestamp(aunit)
      | Utimestamp(aunit)
      | Float(aunit)
      | Float64(aunit)
      | Blob(aunit);

    type attr =
      | Column(ann(astring))
      | Nullable(aunit)
      | Updatable(aunit)
      | Autoinsert(aunit)
      | Autoupdate(aunit)
      | Length(ann(astring));

    type t = (name, type_, option(list(attr)));
  };

  module Rel = {
    type name = astring;

    type model = astring;

    type field = astring;

    type kind =
      | Setnull(aunit)
      | Cascade(aunit)
      | Restrict(aunit);

    type attr =
      | Column(ann(astring))
      | Nullable(aunit)
      | Updatable(aunit);

    type t = (name, model, field, kind, option(list(attr)));
  };

  type name = astring;

  type entry =
    | Table(ann(astring))
    | Key(ann(list(astring)))
    | Unique(ann(list(astring)))
    | Index(ann(Index.t))
    | Field(ann(Field.t))
    | Rel(ann(Rel.t));

  type t = (name, list(entry));
};

module Crud = {
  module Query = {
    type op =
      | NotEqual(aunit)
      | LessThan(aunit)
      | LessThanOrEqual(aunit)
      | GreaterThan(aunit)
      | GreaterThanOrEqual(aunit)
      | Equal(aunit)
      | In(aunit);

    type value =
      | Placeholder(aunit)
      | Field(ann(astring))
      | Literal(ann(astring))
      | Call(ann((astring, value)))
      | Join(ann((astring, t, astring)))
    and t =
      | Term(ann((value, op, value)))
      | And(ann((t, t)))
      | Or(ann((t, t)));
  };

  module Create = {
    type attr =
      | Raw(aunit)
      | Suffix(ann(astring));

    type t = list(attr);
  };

  module Read = {
    type kind =
      | Has(aunit)
      | First(aunit)
      | One(aunit)
      | All(aunit)
      | Find(aunit)
      | Limited(aunit)
      | Paged(aunit);

    type direction =
      | Ascending(aunit)
      | Descending(aunit);

    type attr =
      | Suffix(ann(astring))
      | OrderBy(ann(direction));

    type t = (kind, option(Query.t), option(list(attr)));
  };

  module Update = {
    type attr =
      | Suffix(ann(astring));

    type t = (Query.t, option(list(attr)));
  };

  module Delete = {
    type attr =
      | Suffix(ann(astring));

    type t = (Query.t, option(list(attr)));
  };

  type model = astring;

  type entry =
    | Create(ann(Create.t))
    | Read(ann(Read.t))
    | Update(ann(Update.t))
    | Delete(ann(Delete.t));

  type t = (model, list(entry));
};

type definition =
  | Model(ann(Model.t))
  | Crud(ann(Crud.t));
