open Base
open Crud_ast
open Ir_xform_hashes

type field =
  | Field of Syntax.Field.t
  | Rel of Syntax.Rel.t

type t =
  { models: (string, Syntax.Model.t) Hashtbl.t
  ; fields: (string * string, field) Hashtbl.t
  }

let create defs =
  let out =
    { models = Hashtbl.create (module String)
    ; fields = Hashtbl.create (module Fields)
    }
  in

  let walk_model ({ Syntax.Model.name = { node = model; _ }; entries } as node) =
    Hashtbl.set out.models ~key:model ~data:node;
    List.iter entries ~f:(
      fun { Annotate.node; _ } ->
        match node with
        | Field ({ name = { node = field; _ }; _ } as node) ->
          Hashtbl.set out.fields ~key:(model, field) ~data:(Field node)
        | Rel ({ name = { node = rel; _ }; _ } as node) ->
          Hashtbl.set out.fields ~key:(model, rel) ~data:(Rel node)
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

let find_model t { Annotate.node = model; loc } =
  match Hashtbl.find t.models model with
  | None -> raise (Ir_error.Exn (Undefined ("model", loc)))
  | Some value -> value

let find_field t model { Annotate.node = field; loc } =
  match Hashtbl.find t.fields (model, field) with
  | None -> raise (Ir_error.Exn (Undefined ("field", loc)))
  | Some value -> value
