open Core
open Crud_ast
open Ir_xform_hashes

type field =
  | Field of Syntax.Field.t
  | Rel of Syntax.Rel.t

type t =
  { models: Syntax.Model.t StringHash.t
  ;fields: field FieldHash.t
  }

let create defs =
  let out =
    { models = StringHash.create ()
    ; fields = FieldHash.create ()
    }
  in

  let walk_model ({ Syntax.Model.name = { node = model; _ }; entries } as node) =
    StringHash.set out.models ~key:model ~data:node;
    List.iter entries ~f:(
      fun { Annotate.node; _ } ->
        match node with
        | Field ({ name = { node = field; _ }; _ } as node) ->
          FieldHash.set out.fields ~key:(model, field) ~data:(Field node)
        | Rel ({ name = { node = rel; _ }; _ } as node) ->
          FieldHash.set out.fields ~key:(model, rel) ~data:(Rel node)
        | Table _ | Key _ | Unique _ | Index _ -> ()
    )
  in

  List.iter defs ~f:(
    fun { Annotate.node; _ } ->
      match node with
      | Syntax.Model model -> walk_model model
      | Syntax.Crud crud -> ignore crud
  );
  out

let find (type key) (module Hash : Hashtbl.S with type key = key) hash kind key loc =
  match Hash.find hash key with
  | None -> raise (Ir_error.Exn (Undefined (kind, loc)))
  | Some value -> value

let find_model t { Annotate.node = model; loc } =
  find (module StringHash) t.models "model" model loc

let find_field t model { Annotate.node = field; loc } =
  find (module FieldHash) t.fields "field" (model, field) loc
