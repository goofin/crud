open Core;
open Syntax.Annotate;

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

module Annotate = {
  let print = (~depth=0, header, annotated) => {
    let start_line = annotated.loc.start_line;
    let end_line = annotated.loc.end_line;
    let prefix = String.make(depth, '\t');
    printf("%s%s\n", prefix, header);
    for (line_num in start_line to end_line) {
      let line =
        switch (nth_line(annotated.loc.file, line_num)) {
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
          Annotate.print(~depth, "entry", annotated);
          Annotate.print(~depth=depth + 1, "name", annotated.node);
        }
      | Fields(annotated) => {
          Annotate.print(~depth, "entry", annotated);
          List.iter(annotated.node, Annotate.print(~depth=depth + 1, "field"));
        }
      | Unique(annotated) => Annotate.print(~depth, "entry", annotated);

    let print = (~depth=0, annotated) => {
      Annotate.print(~depth, "index", annotated);
      List.iter(annotated.node, print_entry(~depth=depth + 1));
    };
  };

  module Field = {
    open Syntax.Model.Field;

    let print_type = (~depth=0) =>
      fun
      | Serial(annotated) => Annotate.print(~depth, "type", annotated)
      | Serial64(annotated) => Annotate.print(~depth, "type", annotated)
      | Int(annotated) => Annotate.print(~depth, "type", annotated)
      | Int64(annotated) => Annotate.print(~depth, "type", annotated)
      | Uint(annotated) => Annotate.print(~depth, "type", annotated)
      | Uint64(annotated) => Annotate.print(~depth, "type", annotated)
      | Bool(annotated) => Annotate.print(~depth, "type", annotated)
      | Text(annotated) => Annotate.print(~depth, "type", annotated)
      | Date(annotated) => Annotate.print(~depth, "type", annotated)
      | Timestamp(annotated) => Annotate.print(~depth, "type", annotated)
      | Utimestamp(annotated) => Annotate.print(~depth, "type", annotated)
      | Float(annotated) => Annotate.print(~depth, "type", annotated)
      | Float64(annotated) => Annotate.print(~depth, "type", annotated)
      | Blob(annotated) => Annotate.print(~depth, "type", annotated);

    let print_attr = (~depth=0) =>
      fun
      | Column(annotated) => {
          Annotate.print(~depth, "attr", annotated);
          Annotate.print(~depth=depth + 1, "name", annotated.node);
        }
      | Length(annotated) => {
          Annotate.print(~depth, "attr", annotated);
          Annotate.print(~depth=depth + 1, "length", annotated.node);
        }
      | Nullable(annotated) => Annotate.print(~depth, "attr", annotated)
      | Updatable(annotated) => Annotate.print(~depth, "attr", annotated)
      | Autoinsert(annotated) => Annotate.print(~depth, "attr", annotated)
      | Autoupdate(annotated) => Annotate.print(~depth, "attr", annotated);

    let print = (~depth=0, annotated) => {
      Annotate.print(~depth, "field", annotated);
      let (name, type_, attrs) = annotated.node;
      Annotate.print(~depth=depth + 1, "name", name);
      print_type(~depth=depth + 1, type_);
      optionally_iter(attrs, print_attr(~depth=depth + 1));
    };
  };

  module Rel = {
    open Syntax.Model.Rel;

    let print_kind = (~depth=0) =>
      fun
      | Setnull(annotated) => Annotate.print(~depth, "kind", annotated)
      | Cascade(annotated) => Annotate.print(~depth, "kind", annotated)
      | Restrict(annotated) => Annotate.print(~depth, "kind", annotated);

    let print_attr = (~depth=0) =>
      fun
      | Column(annotated) => {
          Annotate.print(~depth, "attr", annotated);
          Annotate.print(~depth=depth + 1, "name", annotated.node);
        }
      | Nullable(annotated) => Annotate.print(~depth, "attr", annotated)
      | Updatable(annotated) => Annotate.print(~depth, "attr", annotated);

    let print = (~depth=0, annotated) => {
      Annotate.print(~depth, "rel", annotated);
      let (name, model, field, kind, attrs) = annotated.node;
      Annotate.print(~depth=depth + 1, "name", name);
      Annotate.print(~depth=depth + 1, "model", model);
      Annotate.print(~depth=depth + 1, "field", field);
      print_kind(~depth=depth + 1, kind);
      optionally_iter(attrs, print_attr(~depth=depth + 1));
    };
  };

  let print_entry = (~depth=0) =>
    fun
    | Table(annotated) => {
        Annotate.print(~depth, "table", annotated);
        Annotate.print(~depth=depth + 1, "name", annotated.node);
      }
    | Key(annotated) => {
        Annotate.print(~depth, "key", annotated);
        List.iter(annotated.node, Annotate.print(~depth=depth + 1, "field name"));
      }
    | Unique(annotated) => {
        Annotate.print(~depth, "unique", annotated);
        List.iter(annotated.node, Annotate.print(~depth=depth + 1, "field name"));
      }
    | Index(annotated) => Index.print(~depth, annotated)
    | Field(annotated) => Field.print(~depth, annotated)
    | Rel(annotated) => Rel.print(~depth, annotated);

  let print = (~depth=0, annotated) => {
    Annotate.print(~depth, "model", annotated);
    let (name, entries) = annotated.node;
    Annotate.print(~depth=depth + 1, "name", name);
    List.iter(entries, print_entry(~depth=depth + 1));
  };
};

module Crud = {
  open Syntax.Crud;

  module Query = {
    open Syntax.Crud.Query;

    let print_op = (~depth=0) =>
      fun
      | NotEqual(annotated) => Annotate.print(~depth, "op", annotated)
      | LessThan(annotated) => Annotate.print(~depth, "op", annotated)
      | LessThanOrEqual(annotated) => Annotate.print(~depth, "op", annotated)
      | GreaterThan(annotated) => Annotate.print(~depth, "op", annotated)
      | GreaterThanOrEqual(annotated) => Annotate.print(~depth, "op", annotated)
      | Equal(annotated) => Annotate.print(~depth, "op", annotated)
      | In(annotated) => Annotate.print(~depth, "op", annotated);

    let rec print_value = (~depth=0) =>
      fun
      | Placeholder(annotated) => Annotate.print(~depth, "value (placeholder)", annotated)
      | Field(annotated) => {
          Annotate.print(~depth, "value (field)", annotated);
          Annotate.print(~depth=depth + 1, "name", annotated.node);
        }
      | Literal(annotated) => {
          Annotate.print(~depth, "value (literal)", annotated);
          Annotate.print(~depth=depth + 1, "value", annotated.node);
        }
      | Call(annotated) => {
          Annotate.print(~depth, "value (call)", annotated);
          let (name, value) = annotated.node;
          Annotate.print(~depth=depth + 1, "name", name);
          print_value(~depth=depth + 1, value);
        }
      | Join(annotated) => {
          Annotate.print(~depth, "value (join)", annotated);
          let (model, query, field) = annotated.node;
          Annotate.print(~depth=depth + 1, "model", model);
          Annotate.print(~depth=depth + 1, "field", field);
          print(~depth=depth + 1, query);
        }
    and print = (~depth=0) =>
      fun
      | Term(annotated) => {
          Annotate.print(~depth, "term", annotated);
          let (left, op, right) = annotated.node;
          print_value(~depth=depth + 1, left);
          print_op(~depth=depth + 1, op);
          print_value(~depth=depth + 1, right);
        }
      | And(annotated) => {
          Annotate.print(~depth, "and", annotated);
          let (left, right) = annotated.node;
          print(~depth=depth + 1, left);
          print(~depth=depth + 1, right);
        }
      | Or(annotated) => {
          Annotate.print(~depth, "or", annotated);
          let (left, right) = annotated.node;
          print(~depth=depth + 1, left);
          print(~depth=depth + 1, right);
        };
  };

  module Create = {
    open Syntax.Crud.Create;

    let print_attr = (~depth=0) =>
      fun
      | Raw(annotated) => Annotate.print(~depth, "attr", annotated)
      | Suffix(annotated) => {
          Annotate.print(~depth, "attr", annotated);
          Annotate.print(~depth=depth + 1, "suffix", annotated.node);
        };

    let print = (~depth=0, annotated) => {
      Annotate.print(~depth, "create", annotated);
      List.iter(annotated.node, print_attr(~depth=depth + 1));
    };
  };

  module Read = {
    open Syntax.Crud.Read;

    let print_kind = (~depth=0) =>
      fun
      | Has(annotated) => Annotate.print(~depth, "kind", annotated)
      | First(annotated) => Annotate.print(~depth, "kind", annotated)
      | One(annotated) => Annotate.print(~depth, "kind", annotated)
      | All(annotated) => Annotate.print(~depth, "kind", annotated)
      | Find(annotated) => Annotate.print(~depth, "kind", annotated)
      | Limited(annotated) => Annotate.print(~depth, "kind", annotated)
      | Paged(annotated) => Annotate.print(~depth, "kind", annotated);

    let print_direction = (~depth=0) =>
      fun
      | Ascending(annotated) => Annotate.print(~depth, "direction", annotated)
      | Descending(annotated) => Annotate.print(~depth, "direction", annotated);

    let print_attr = (~depth=0) =>
      fun
      | Suffix(annotated) => {
          Annotate.print(~depth, "attr", annotated);
          Annotate.print(~depth=depth + 1, "suffix", annotated.node);
        }
      | OrderBy(annotated) => {
          Annotate.print(~depth, "attr", annotated);
          print_direction(~depth=depth + 1, annotated.node);
        };

    let print = (~depth=0, annotated) => {
      Annotate.print(~depth, "read", annotated);
      let (kind, query, attrs) = annotated.node;
      print_kind(~depth=depth + 1, kind);
      optionally(query, Query.print(~depth=depth + 1));
      optionally_iter(attrs, print_attr(~depth=depth + 1));
    };
  };

  module Update = {
    open Syntax.Crud.Update;

    let print_attr = (~depth=0) =>
      fun
      | Suffix(annotated) => {
          Annotate.print(~depth, "attr", annotated);
          Annotate.print(~depth=depth + 1, "name", annotated.node);
        };

    let print = (~depth=0, annotated) => {
      Annotate.print(~depth, "update", annotated);
      let (query, attrs) = annotated.node;
      Query.print(~depth=depth + 1, query);
      optionally_iter(attrs, print_attr(~depth=depth + 1));
    };
  };

  module Delete = {
    open Syntax.Crud.Delete;

    let print_attr = (~depth=0) =>
      fun
      | Suffix(annotated) => {
          Annotate.print(~depth, "attr", annotated);
          Annotate.print(~depth=depth + 1, "name", annotated.node);
        };

    let print = (~depth=0, annotated) => {
      Annotate.print(~depth, "update", annotated);
      let (query, attrs) = annotated.node;
      Query.print(~depth=depth + 1, query);
      optionally_iter(attrs, print_attr(~depth=depth + 1));
    };
  };

  let print_entry = (~depth=0) =>
    fun
    | Create(annotated) => Create.print(~depth, annotated)
    | Read(annotated) => Read.print(~depth, annotated)
    | Update(annotated) => Update.print(~depth, annotated)
    | Delete(annotated) => Delete.print(~depth, annotated);

  let print = (~depth=0, annotated) => {
    Annotate.print(~depth, "crud", annotated);
    let (model, entries) = annotated.node;
    Annotate.print(~depth=depth + 1, "model", model);
    List.iter(entries, print_entry(~depth=depth + 1));
  };
};

let rec definition =
  fun
  | Syntax.Model(model) => Model.print(model)
  | Syntax.Crud(crud) => Crud.print(crud);