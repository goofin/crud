open Core

let print_annotated ?(depth=0) header (annotated : 't Ast_annotate.t) =
  let prefix = String.make depth '\t' in
  printf "%s%s\n" prefix header;
  Ast_annotate.print_location ~depth annotated.loc

module Utils = struct
  let optionally option callback =
    match option with
    | Some option -> callback option
    | None -> ()

  let print_iter ?(depth=0) title iterable (callback : ?depth:int -> 'a -> unit) =
    List.iter iterable ~f:(
      fun value ->
        print_annotated ~depth title value;
        callback ~depth:(depth+1) value.node
    )
end

module Field = struct
  open Ast_syntax.Field

  let print_attr ?(depth=0) = 
    function
    | Column column -> print_annotated ~depth "column" column
    | Length length -> print_annotated ~depth "length" length
    | Nullable | Updatable | Autoinsert | Autoupdate -> ()

  let print ?(depth=0) field =
    print_annotated ~depth "name" field.name;
    print_annotated ~depth "type" field.type_;
    Utils.print_iter ~depth "attr" field.attrs print_attr;
end

module Rel = struct
  open Ast_syntax.Rel

  let print_attr ?(depth=0) = 
    function
    | Column column -> print_annotated ~depth "column" column
    | Nullable | Updatable -> ()

  let print ?(depth=0) rel =
    print_annotated ~depth "name" rel.name;
    print_annotated ~depth "model" rel.model;
    print_annotated ~depth "field" rel.field;
    print_annotated ~depth "kind" rel.kind;
    Utils.print_iter ~depth "attr" rel.attrs print_attr;
end

module Model = struct
  open Ast_syntax.Model

  let print_entry ?(depth=0) = 
    function
    | Table table -> print_annotated ~depth "table" table
    | Key fields -> List.iter fields ~f:(print_annotated ~depth "field")
    | Unique unique -> List.iter unique ~f:(print_annotated ~depth "unique")
    | Index index -> List.iter index ~f:(print_annotated ~depth "index")
    | Field field -> Field.print ~depth field
    | Rel rel -> Rel.print ~depth rel

  let print ?(depth=0) model =
    print_annotated ~depth "name" model.name;
    Utils.print_iter ~depth "entry" model.entries print_entry;
end

module Query = struct
  open Ast_syntax.Query

  let rec print_value ?(depth=0) =
    function
    | Field field -> print_annotated ~depth "field" field
    | Literal literal -> print_annotated ~depth "literal" literal
    | Call (func, value) ->
      print_annotated ~depth "func" func;
      print_annotated ~depth "value" value;
      print_value ~depth:(depth+1) value.node
    | Join (model, query, field) ->
      print_annotated ~depth "model" model;
      print_annotated ~depth "query" query;
      print ~depth:(depth+1) query.node;
      print_annotated ~depth "field" field;
    | Placeholder -> ()

  and print ?(depth=0) =
    function
    | Term term -> 
      print_annotated ~depth "left_val" term.left_val;
      print_value ~depth:(depth+1) term.left_val.node;
      print_annotated ~depth "op" term.op;
      print_annotated ~depth "right_val" term.right_val;
      print_value ~depth:(depth+1) term.right_val.node
    | And and_ -> 
      print_annotated ~depth "left_query" and_.left_query;
      print ~depth:(depth+1) and_.left_query.node;
      print_annotated ~depth "right_query" and_.right_query;
      print ~depth:(depth+1) and_.right_query.node
    | Or or_ ->
      print_annotated ~depth "left_query" or_.left_query;
      print ~depth:(depth+1) or_.left_query.node;
      print_annotated ~depth "right_query" or_.right_query;
      print ~depth:(depth+1) or_.right_query.node
end

module Create = struct
  open Ast_syntax.Create

  let print_attr ?(depth=0) =
    function
    | Suffix suffix -> print_annotated ~depth "suffix" suffix
    | Raw -> ()

  let print ?(depth=0) create =
    Utils.print_iter ~depth "attr" create.attrs print_attr
end

module Read = struct
  open Ast_syntax.Read

  let print_attr ?(depth=0) =
    function
    | Suffix suffix -> print_annotated ~depth "suffix" suffix
    | OrderBy _ -> ()

  let print ?(depth=0) read =
    print_annotated ~depth "kind" read.kind;
    Utils.optionally read.query (fun query ->
        print_annotated ~depth "query" query;
        Query.print ~depth:(depth+1) query.node
      );
    Utils.print_iter ~depth "attr" read.attrs print_attr
end

module Update = struct
  open Ast_syntax.Update

  let print_attr ?(depth=0) =
    function
    | Suffix suffix -> print_annotated ~depth "suffix" suffix

  let print ?(depth=0) update =
    print_annotated ~depth "query" update.query;
    Query.print ~depth:(depth+1) update.query.node;
    Utils.print_iter ~depth "attr" update.attrs print_attr
end

module Delete = struct
  open Ast_syntax.Delete

  let print_attr ?(depth=0) =
    function
    | Suffix suffix -> print_annotated ~depth "suffix" suffix

  let print ?(depth=0) delete =
    print_annotated ~depth "query" delete.query;
    Query.print ~depth:(depth+1) delete.query.node;
    Utils.print_iter ~depth "attr" delete.attrs print_attr
end

module Crud = struct
  open Ast_syntax.Crud

  let print_entry ?(depth=0) =
    function
    | Create create -> Create.print ~depth create
    | Read read -> Read.print ~depth read
    | Update update -> Update.print ~depth update
    | Delete delete -> Delete.print ~depth delete

  let print ?(depth=0) crud =
    print_annotated ~depth "model" crud.model;
    Utils.print_iter ~depth "entry" crud.entries print_entry
end

let print_definition ?(depth=0) =
  function
  | Ast_syntax.Model model -> Model.print ~depth model
  | Ast_syntax.Crud crud -> Crud.print ~depth crud

let print ?(depth=0) def =
  print_annotated ~depth "definition" def;
  print_definition ~depth:(depth+1) def.node
