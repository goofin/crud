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

  let walk_model ({ Syntax.Model.name = { node = model }; entries } as node) =
    StringHash.set out.models model node;
    List.iter entries ~f:(
      fun { Annotate.loc = loc; node } ->
        match node with
        | Field ({ name = { node = field } } as node) ->
          FieldHash.set out.fields (model, field) (Field node)
        | Rel ({ name = { node = rel } } as node) ->
          FieldHash.set out.fields (model, rel) (Rel node)
        | Table _ | Key _ | Unique _ | Index _ -> ()
    )
  in

  List.iter defs ~f:(
    fun { Annotate.loc = loc; node } ->
      match node with
      | Syntax.Model model -> walk_model model
      | Syntax.Crud crud -> ()
  );
  out

let find_model t { Annotate.node = model; loc } =
  match StringHash.find t.models model with
  | None -> raise (Ir_error.Exn (Undefined ("model", loc)))
  | Some model -> model

let find_field t model { Annotate.node = field; loc } =
  match FieldHash.find t.fields (model, field) with
  | None -> raise (Ir_error.Exn (Undefined ("field", loc)))
  | Some field -> field