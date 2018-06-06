open Core;
open Annotate;

module Utils = {
  let nth_line = (file, line) =>
    if (line <= 0) {
      None;
    } else {
      let read_lines = input => {
        for (i in 1 to line - 1) {
          ignore(In_channel.input_line(input));
        };
        In_channel.input_line(input);
      };
      try (In_channel.with_file(file, ~f=read_lines)) {
      | _ => None
      };
    };

  let optionally_iter = (list, callback) =>
    switch (list) {
    | Some(list) => List.iter(list, callback)
    | None => ()
    };

  let optionally = (option, callback) =>
    switch (option) {
    | Some(option) => callback(option)
    | None => ()
    };
};

module Annotated = {
  let print = (~depth=0, header, annotated) => {
    let start_line = annotated.loc.start_line;
    let end_line = annotated.loc.end_line;
    let prefix = String.make(depth, '\t');
    printf("%s%s\n", prefix, header);
    for (line_num in start_line to end_line) {
      let line =
        switch (Utils.nth_line(annotated.loc.file, line_num)) {
        | Some(line) => line
        | _ => assert(false)
        };
      printf("%s%4d: ", prefix, line_num);
      if (start_line == end_line) {
        print_endline(String.slice(line, annotated.loc.start_pos, annotated.loc.end_pos));
      } else if (line_num == start_line) {
        print_endline(String.slice(line, annotated.loc.start_pos, 0));
      } else if (line_num == end_line) {
        print_endline(String.slice(line, 0, annotated.loc.end_pos));
      } else {
        print_endline(line);
      };
    };
    printf("\n");
  };
};

module Model = {
  open Syntax.Model;

  module Index = {
    open Syntax.Model.Index;

    let print_entry = (~depth=0) =>
      fun
      | Name(annotated) => {
          Annotated.print(~depth, "entry", annotated);
          Annotated.print(~depth=depth + 1, "name", annotated.node);
        }
      | Fields(annotated) => {
          Annotated.print(~depth, "entry", annotated);
          List.iter(annotated.node, Annotated.print(~depth=depth + 1, "field"));
        }
      | Unique(annotated) => Annotated.print(~depth, "entry", annotated);

    let print = (~depth=0, annotated) => {
      Annotated.print(~depth, "index", annotated);
      List.iter(annotated.node, print_entry(~depth=depth + 1));
    };
  };

  module Field = {
    open Syntax.Model.Field;

    let print_type = (~depth=0) =>
      fun
      | Serial(annotated) => Annotated.print(~depth, "type", annotated)
      | Serial64(annotated) => Annotated.print(~depth, "type", annotated)
      | Int(annotated) => Annotated.print(~depth, "type", annotated)
      | Int64(annotated) => Annotated.print(~depth, "type", annotated)
      | Uint(annotated) => Annotated.print(~depth, "type", annotated)
      | Uint64(annotated) => Annotated.print(~depth, "type", annotated)
      | Bool(annotated) => Annotated.print(~depth, "type", annotated)
      | Text(annotated) => Annotated.print(~depth, "type", annotated)
      | Date(annotated) => Annotated.print(~depth, "type", annotated)
      | Timestamp(annotated) => Annotated.print(~depth, "type", annotated)
      | Utimestamp(annotated) => Annotated.print(~depth, "type", annotated)
      | Float(annotated) => Annotated.print(~depth, "type", annotated)
      | Float64(annotated) => Annotated.print(~depth, "type", annotated)
      | Blob(annotated) => Annotated.print(~depth, "type", annotated);

    let print_attr = (~depth=0) =>
      fun
      | Column(annotated) => {
          Annotated.print(~depth, "attr", annotated);
          Annotated.print(~depth=depth + 1, "name", annotated.node);
        }
      | Length(annotated) => {
          Annotated.print(~depth, "attr", annotated);
          Annotated.print(~depth=depth + 1, "length", annotated.node);
        }
      | Nullable(annotated) => Annotated.print(~depth, "attr", annotated)
      | Updatable(annotated) => Annotated.print(~depth, "attr", annotated)
      | Autoinsert(annotated) => Annotated.print(~depth, "attr", annotated)
      | Autoupdate(annotated) => Annotated.print(~depth, "attr", annotated);

    let print = (~depth=0, annotated) => {
      Annotated.print(~depth, "field", annotated);
      let (name, type_, attrs) = annotated.node;
      Annotated.print(~depth=depth + 1, "name", name);
      print_type(~depth=depth + 1, type_);
      Utils.optionally_iter(attrs, print_attr(~depth=depth + 1));
    };
  };

  module Rel = {
    open Syntax.Model.Rel;

    let print_kind = (~depth=0) =>
      fun
      | Setnull(annotated) => Annotated.print(~depth, "kind", annotated)
      | Cascade(annotated) => Annotated.print(~depth, "kind", annotated)
      | Restrict(annotated) => Annotated.print(~depth, "kind", annotated);

    let print_attr = (~depth=0) =>
      fun
      | Column(annotated) => {
          Annotated.print(~depth, "attr", annotated);
          Annotated.print(~depth=depth + 1, "name", annotated.node);
        }
      | Nullable(annotated) => Annotated.print(~depth, "attr", annotated)
      | Updatable(annotated) => Annotated.print(~depth, "attr", annotated);

    let print = (~depth=0, annotated) => {
      Annotated.print(~depth, "rel", annotated);
      let (name, model, field, kind, attrs) = annotated.node;
      Annotated.print(~depth=depth + 1, "name", name);
      Annotated.print(~depth=depth + 1, "model", model);
      Annotated.print(~depth=depth + 1, "field", field);
      print_kind(~depth=depth + 1, kind);
      Utils.optionally_iter(attrs, print_attr(~depth=depth + 1));
    };
  };

  let print_entry = (~depth=0) =>
    fun
    | Table(annotated) => {
        Annotated.print(~depth, "table", annotated);
        Annotated.print(~depth=depth + 1, "name", annotated.node);
      }
    | Key(annotated) => {
        Annotated.print(~depth, "key", annotated);
        List.iter(annotated.node, Annotated.print(~depth=depth + 1, "field name"));
      }
    | Unique(annotated) => {
        Annotated.print(~depth, "unique", annotated);
        List.iter(annotated.node, Annotated.print(~depth=depth + 1, "field name"));
      }
    | Index(annotated) => Index.print(~depth, annotated)
    | Field(annotated) => Field.print(~depth, annotated)
    | Rel(annotated) => Rel.print(~depth, annotated);

  let print = (~depth=0, annotated) => {
    Annotated.print(~depth, "model", annotated);
    let (name, entries) = annotated.node;
    Annotated.print(~depth=depth + 1, "name", name);
    List.iter(entries, print_entry(~depth=depth + 1));
  };
};

module Crud = {
  open Syntax.Crud;

  module Query = {
    open Syntax.Crud.Query;

    let print_op = (~depth=0) =>
      fun
      | NotEqual(annotated) => Annotated.print(~depth, "op", annotated)
      | LessThan(annotated) => Annotated.print(~depth, "op", annotated)
      | LessThanOrEqual(annotated) => Annotated.print(~depth, "op", annotated)
      | GreaterThan(annotated) => Annotated.print(~depth, "op", annotated)
      | GreaterThanOrEqual(annotated) => Annotated.print(~depth, "op", annotated)
      | Equal(annotated) => Annotated.print(~depth, "op", annotated)
      | In(annotated) => Annotated.print(~depth, "op", annotated);

    let rec print_value = (~depth=0) =>
      fun
      | Placeholder(annotated) => Annotated.print(~depth, "value (placeholder)", annotated)
      | Field(annotated) => {
          Annotated.print(~depth, "value (field)", annotated);
          Annotated.print(~depth=depth + 1, "name", annotated.node);
        }
      | Literal(annotated) => {
          Annotated.print(~depth, "value (literal)", annotated);
          Annotated.print(~depth=depth + 1, "value", annotated.node);
        }
      | Call(annotated) => {
          Annotated.print(~depth, "value (call)", annotated);
          let (name, value) = annotated.node;
          Annotated.print(~depth=depth + 1, "name", name);
          print_value(~depth=depth + 1, value);
        }
      | Join(annotated) => {
          Annotated.print(~depth, "value (join)", annotated);
          let (model, query, field) = annotated.node;
          Annotated.print(~depth=depth + 1, "model", model);
          Annotated.print(~depth=depth + 1, "field", field);
          print(~depth=depth + 1, query);
        }
    and print = (~depth=0) =>
      fun
      | Term(annotated) => {
          Annotated.print(~depth, "term", annotated);
          let (left, op, right) = annotated.node;
          print_value(~depth=depth + 1, left);
          print_op(~depth=depth + 1, op);
          print_value(~depth=depth + 1, right);
        }
      | And(annotated) => {
          Annotated.print(~depth, "and", annotated);
          let (left, right) = annotated.node;
          print(~depth=depth + 1, left);
          print(~depth=depth + 1, right);
        }
      | Or(annotated) => {
          Annotated.print(~depth, "or", annotated);
          let (left, right) = annotated.node;
          print(~depth=depth + 1, left);
          print(~depth=depth + 1, right);
        };
  };

  module Create = {
    open Syntax.Crud.Create;

    let print_attr = (~depth=0) =>
      fun
      | Raw(annotated) => Annotated.print(~depth, "attr", annotated)
      | Suffix(annotated) => {
          Annotated.print(~depth, "attr", annotated);
          Annotated.print(~depth=depth + 1, "suffix", annotated.node);
        };

    let print = (~depth=0, annotated) => {
      Annotated.print(~depth, "create", annotated);
      List.iter(annotated.node, print_attr(~depth=depth + 1));
    };
  };

  module Read = {
    open Syntax.Crud.Read;

    let print_kind = (~depth=0) =>
      fun
      | Has(annotated) => Annotated.print(~depth, "kind", annotated)
      | First(annotated) => Annotated.print(~depth, "kind", annotated)
      | One(annotated) => Annotated.print(~depth, "kind", annotated)
      | All(annotated) => Annotated.print(~depth, "kind", annotated)
      | Find(annotated) => Annotated.print(~depth, "kind", annotated)
      | Limited(annotated) => Annotated.print(~depth, "kind", annotated)
      | Paged(annotated) => Annotated.print(~depth, "kind", annotated);

    let print_direction = (~depth=0) =>
      fun
      | Ascending(annotated) => Annotated.print(~depth, "direction", annotated)
      | Descending(annotated) => Annotated.print(~depth, "direction", annotated);

    let print_attr = (~depth=0) =>
      fun
      | Suffix(annotated) => {
          Annotated.print(~depth, "attr", annotated);
          Annotated.print(~depth=depth + 1, "suffix", annotated.node);
        }
      | OrderBy(annotated) => {
          Annotated.print(~depth, "attr", annotated);
          print_direction(~depth=depth + 1, annotated.node);
        };

    let print = (~depth=0, annotated) => {
      Annotated.print(~depth, "read", annotated);
      let (kind, query, attrs) = annotated.node;
      print_kind(~depth=depth + 1, kind);
      Utils.optionally(query, Query.print(~depth=depth + 1));
      Utils.optionally_iter(attrs, print_attr(~depth=depth + 1));
    };
  };

  module Update = {
    open Syntax.Crud.Update;

    let print_attr = (~depth=0) =>
      fun
      | Suffix(annotated) => {
          Annotated.print(~depth, "attr", annotated);
          Annotated.print(~depth=depth + 1, "name", annotated.node);
        };

    let print = (~depth=0, annotated) => {
      Annotated.print(~depth, "update", annotated);
      let (query, attrs) = annotated.node;
      Query.print(~depth=depth + 1, query);
      Utils.optionally_iter(attrs, print_attr(~depth=depth + 1));
    };
  };

  module Delete = {
    open Syntax.Crud.Delete;

    let print_attr = (~depth=0) =>
      fun
      | Suffix(annotated) => {
          Annotated.print(~depth, "attr", annotated);
          Annotated.print(~depth=depth + 1, "name", annotated.node);
        };

    let print = (~depth=0, annotated) => {
      Annotated.print(~depth, "update", annotated);
      let (query, attrs) = annotated.node;
      Query.print(~depth=depth + 1, query);
      Utils.optionally_iter(attrs, print_attr(~depth=depth + 1));
    };
  };

  let print_entry = (~depth=0) =>
    fun
    | Create(annotated) => Create.print(~depth, annotated)
    | Read(annotated) => Read.print(~depth, annotated)
    | Update(annotated) => Update.print(~depth, annotated)
    | Delete(annotated) => Delete.print(~depth, annotated);

  let print = (~depth=0, annotated) => {
    Annotated.print(~depth, "crud", annotated);
    let (model, entries) = annotated.node;
    Annotated.print(~depth=depth + 1, "model", model);
    List.iter(entries, print_entry(~depth=depth + 1));
  };
};

let definition =
  fun
  | Syntax.Model(model) => Model.print(model)
  | Syntax.Crud(crud) => Crud.print(crud);
