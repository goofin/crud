open Core;

module Utils = {
  let optionally = (option, callback) =>
    switch (option) {
    | Some(option) => callback(option)
    | None => ()
    };
};

let print_annotated = (~depth=0, header, annotated: Ast_annotate.t('t)) => {
  let prefix = String.make(depth, '\t');
  printf("%s%s\n", prefix, header);
  Ast_annotate.print_location(~depth, annotated.loc);
};

module Model = {
  open Ast_syntax.Model;

  module Field = {
    open Ast_syntax.Model.Field;

    let print_attr = (~depth=0) =>
      fun
      | Column(column) => print_annotated(~depth, "name", column)
      | Length(length) => print_annotated(~depth, "length", length)
      | Nullable
      | Updatable
      | Autoinsert
      | Autoupdate => ();

    let print = (~depth=0, field) => {
      print_annotated(~depth, "name", field.name);
      print_annotated(~depth, "type", field.type_);
      List.iter(
        field.attrs,
        attr => {
          print_annotated(~depth, "attr", attr);
          print_attr(~depth=depth + 1, attr.node);
        },
      );
    };
  };

  module Rel = {
    open Ast_syntax.Model.Rel;

    let print_attr = (~depth=0) =>
      fun
      | Column(name) => print_annotated(~depth, "name", name)
      | Nullable
      | Updatable => ();

    let print = (~depth=0, rel) => {
      print_annotated(~depth, "name", rel.name);
      print_annotated(~depth, "model", rel.model);
      print_annotated(~depth, "field", rel.field);
      print_annotated(~depth, "kind", rel.kind);
      List.iter(
        rel.attrs,
        attr => {
          print_annotated(~depth, "attr", attr);
          print_attr(~depth=depth + 1, attr.node);
        },
      );
    };
  };

  let print_entry = (~depth=0) =>
    fun
    | Table(name) => print_annotated(~depth, "name", name)
    | Key(fields) => List.iter(fields, print_annotated(~depth, "field"))
    | Unique(fields) => List.iter(fields, print_annotated(~depth, "field"))
    | Index(fields) => List.iter(fields, print_annotated(~depth, "field"))
    | Field(field) => Field.print(~depth, field)
    | Rel(rel) => Rel.print(~depth, rel);

  let print = (~depth=0, model) => {
    print_annotated(~depth, "name", model.name);
    List.iter(
      model.entries,
      entry => {
        print_annotated(~depth, "entry", entry);
        print_entry(~depth=depth + 1, entry.node);
      },
    );
  };
};

module Crud = {
  open Ast_syntax.Crud;

  module Query = {
    open Ast_syntax.Crud.Query;

    let rec print_value = (~depth=0) =>
      fun
      | Field(field) => print_annotated(~depth, "field", field)
      | Literal(literal) => print_annotated(~depth, "literal", literal)
      | Call(func, value) => {
          print_annotated(~depth, "func", func);
          print_annotated(~depth, "value", value);
          print_value(~depth=depth + 1, value.node);
        }
      | Join(model, query, field) => {
          print_annotated(~depth, "model", model);
          print_annotated(~depth, "query", query);
          print(~depth=depth + 1, query.node);
          print_annotated(~depth, "field", field);
        }
      | Placeholder => ()
    and print = (~depth=0) =>
      fun
      | Term(term) => {
          print_annotated(~depth, "left_val", term.left_val);
          print_value(~depth=depth + 1, term.left_val.node);
          print_annotated(~depth, "op", term.op);
          print_annotated(~depth, "right_val", term.right_val);
          print_value(~depth=depth + 1, term.right_val.node);
        }
      | And(and_) => {
          print_annotated(~depth, "left_query", and_.left_query);
          print(~depth=depth + 1, and_.left_query.node);
          print_annotated(~depth, "right_query", and_.right_query);
          print(~depth=depth + 1, and_.right_query.node);
        }
      | Or(or_) => {
          print_annotated(~depth, "left_query", or_.left_query);
          print(~depth=depth + 1, or_.left_query.node);
          print_annotated(~depth, "right_query", or_.right_query);
          print(~depth=depth + 1, or_.right_query.node);
        };
  };

  module Create = {
    open Ast_syntax.Crud.Create;

    let print_attr = (~depth=0) =>
      fun
      | Suffix(suffix) => print_annotated(~depth, "suffix", suffix)
      | Raw => ();

    let print = (~depth=0, create) =>
      List.iter(
        create.attrs,
        attr => {
          print_annotated(~depth, "attr", attr);
          print_attr(~depth=depth + 1, attr.node);
        },
      );
  };

  module Read = {
    open Ast_syntax.Crud.Read;

    let print_attr = (~depth=0) =>
      fun
      | Suffix(suffix) => print_annotated(~depth, "suffix", suffix)
      | OrderBy(_) => ();

    let print = (~depth=0, read) => {
      print_annotated(~depth, "kind", read.kind);
      switch (read.query) {
      | Some(query) =>
        print_annotated(~depth, "query", query);
        Query.print(~depth=depth + 1, query.node);
      | None => ()
      };
      List.iter(
        read.attrs,
        attr => {
          print_annotated(~depth, "attr", attr);
          print_attr(~depth=depth + 1, attr.node);
        },
      );
    };
  };

  module Update = {
    open Ast_syntax.Crud.Update;

    let print_attr = (~depth=0) =>
      fun
      | Suffix(suffix) => print_annotated(~depth, "suffix", suffix);

    let print = (~depth=0, update) => {
      print_annotated(~depth, "query", update.query);
      Query.print(~depth=depth + 1, update.query.node);
      List.iter(
        update.attrs,
        attr => {
          print_annotated(~depth, "attr", attr);
          print_attr(~depth=depth + 1, attr.node);
        },
      );
    };
  };

  module Delete = {
    open Ast_syntax.Crud.Delete;

    let print_attr = (~depth=0) =>
      fun
      | Suffix(suffix) => print_annotated(~depth, "suffix", suffix);

    let print = (~depth=0, delete) => {
      print_annotated(~depth, "query", delete.query);
      Query.print(~depth=depth + 1, delete.query.node);
      List.iter(
        delete.attrs,
        attr => {
          print_annotated(~depth, "attr", attr);
          print_attr(~depth=depth + 1, attr.node);
        },
      );
    };
  };

  let print_entry = (~depth=0) =>
    fun
    | Create(create) => Create.print(~depth, create)
    | Read(read) => Read.print(~depth, read)
    | Update(update) => Update.print(~depth, update)
    | Delete(delete) => Delete.print(~depth, delete);

  let print = (~depth=0, crud) => {
    print_annotated(~depth, "model", crud.model);
    List.iter(
      crud.entries,
      entry => {
        print_annotated(~depth, "entry", entry);
        print_entry(~depth=depth + 1, entry.node);
      },
    );
  };
};

let definition = (~depth=0) =>
  fun
  | Ast_syntax.Model(model) => Model.print(~depth, model)
  | Ast_syntax.Crud(crud) => Crud.print(~depth, crud);

let print = (~depth=0, def) => {
  print_annotated("definition", def);
  definition(~depth=1, def.node);
};
