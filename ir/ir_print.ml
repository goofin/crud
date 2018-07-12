open Base
open Stdio

let dprintf ?(depth=0) =
  printf "%s" @@ String.make depth '\t';
  printf

let print_field ?(depth=0) (field : Ir_xform_transform.Model.field ref)  =
  match !field with
  | Field field ->
    dprintf ~depth "field: %s.%s\n" !(field.parent).name field.name

  | Rel rel ->
    let field_name = match !(rel.field) with
      | Field {name; _} -> name
      | Rel {name; _} -> name
    in
    dprintf ~depth "rel: %s.%s\n" !(rel.parent).name rel.name;
    dprintf ~depth:(depth+1) "model: %s\n" !(rel.model).name;
    dprintf ~depth:(depth+1) "field: %s\n" field_name

let print_model ?(depth=0) (model : Ir_xform_transform.Model.t ref) =
  dprintf ~depth "model: %s\n" !model.name;
  Hashtbl.iter !model.fields ~f:(print_field ~depth:(depth+1));
  dprintf ~depth:(depth+1) "key:\n";
  List.iter !model.key ~f:(print_field ~depth:(depth+2));
  List.iter !model.unique ~f:(
    fun unique ->
      dprintf ~depth:(depth+1) "unique:\n";
      List.iter unique ~f:(print_field ~depth:(depth+2))
  );
  List.iter !model.index ~f:(
    fun index ->
      dprintf ~depth:(depth+1) "index:\n";
      List.iter index ~f:(print_field ~depth:(depth+2))
  )

let print {Ir_xform_transform.models; fields; cruds} =
  Hashtbl.iter models ~f:print_model;
  ignore fields;
  ignore cruds
