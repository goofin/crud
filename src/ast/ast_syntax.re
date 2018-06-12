open Core;

module type Annotation = {type t('t);};

module ASTBuilder = (Annotate: Annotation) => {
  type ann('t) = Annotate.t('t);

  type astring = ann(string);

  module Model = {
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
        | Column(astring)
        | Nullable
        | Updatable
        | Autoinsert
        | Autoupdate
        | Length(astring);

      type t = {
        name: astring,
        type_: ann(type_),
        attrs: list(ann(attr)),
      };
    };

    module Rel = {
      type kind =
        | Setnull
        | Cascade
        | Restrict;

      type attr =
        | Column(astring)
        | Nullable
        | Updatable;

      type t = {
        name: astring,
        model: astring,
        field: astring,
        kind: ann(kind),
        attrs: list(ann(attr)),
      };
    };

    type entry =
      | Table(astring)
      | Key(list(astring))
      | Unique(list(astring))
      | Index(list(astring))
      | Field(Field.t)
      | Rel(Rel.t);

    type t = {
      name: astring,
      entries: list(ann(entry)),
    };
  };

  module Crud = {
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
        left_val: ann(value),
        op: ann(op),
        right_val: ann(value),
      }
      and binop = {
        left_query: ann(t),
        right_query: ann(t),
      }
      and t =
        | Term(term)
        | And(binop)
        | Or(binop)
      and value =
        | Placeholder
        | Field(astring)
        | Literal(astring)
        | Call(astring, ann(value))
        | Join(astring, ann(t), astring);
    };

    module Create = {
      type attr =
        | Raw
        | Suffix(astring);

      type t = {attrs: list(ann(attr))};
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
        | Suffix(astring)
        | OrderBy(direction);

      type t = {
        kind: ann(kind),
        query: option(ann(Query.t)),
        attrs: list(ann(attr)),
      };
    };

    module Update = {
      type attr =
        | Suffix(astring);

      type t = {
        query: ann(Query.t),
        attrs: list(ann(attr)),
      };
    };

    module Delete = {
      type attr =
        | Suffix(astring);

      type t = {
        query: ann(Query.t),
        attrs: list(ann(attr)),
      };
    };

    type entry =
      | Create(Create.t)
      | Read(Read.t)
      | Update(Update.t)
      | Delete(Delete.t);

    type t = {
      model: astring,
      entries: list(ann(entry)),
    };
  };

  type definition =
    | Model(Model.t)
    | Crud(Crud.t);
};

include ASTBuilder(Ast_annotate);
