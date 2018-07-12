open Base
open Crud_ast

module rec Field : sig
  type t =
    { parent: Model.t ref
    ; name: string
    ; type_: Syntax.Field.type_
    ; column: string option
    ; nullable: bool
    ; updatable: bool
    ; autoinsert: bool
    ; autoupdate: bool
    ; length: int option
    }
end = Field

and Rel : sig
  type t =
    { parent: Model.t ref
    ; name: string
    ; model: Model.t ref
    ; field: Model.field ref
    ; kind: Syntax.Rel.kind
    ; column: string option
    ; nullable: bool
    ; updatable: bool
    }
end = Rel

and Model : sig
  type field =
    | Field of Field.t
    | Rel of Rel.t

  type t =
    { name: string
    ; fields: (string, field ref) Hashtbl.t
    ; table: string option
    ; key: field ref list
    ; unique: field ref list list
    ; index: field ref list list
    ; cruds: Crud.entry ref list
    }
end = Model

and Query : sig
  type term =
    { t_left: value
    ; t_op: Syntax.Query.op
    ; t_right: value
    }

  and binop =
    { b_left: t
    ; b_right: t
    }

  and t =
    | Term of term
    | And of binop
    | Or of binop

  and value =
    | Placeholder
    | Field of Model.field ref
    | Literal of string
    | Call of string * value
    | Join of Model.t ref * t * Model.field ref
end = Query

and Create : sig
  type t =
    { parent: Crud.t ref
    ; raw: bool
    ; suffix: string option
    }
end = Create

and Read : sig
  type t =
    { parent: Crud.t ref
    ; kind: Syntax.Read.kind
    ; query: Query.t option
    ; suffix: string option
    ; order_by: Syntax.Read.direction option
    }
end = Read

and Update : sig
  type t =
    { parent: Crud.t ref
    ; query: Query.t
    ; suffix: string option
    }
end = Update

and Delete : sig
  type t =
    { parent: Crud.t ref
    ; query: Query.t
    ; suffix: string option
    }
end = Delete

and Crud : sig
  type entry =
    | Create of Create.t
    | Read of Read.t
    | Update of Update.t
    | Delete of Delete.t

  type t =
    { model: Model.t ref
    ; entries: entry list
    }
end = Crud

module QueryTypes = struct
  type type_ =
    | Any
    | Integer
    | Float
    | String
    | DateTime
    | Fixed of Syntax.Field.type_

  let type_of_value (value : Query.value) =
    let rec field_type (field : Model.field ref) =
      match !field with
      | Field field -> field.type_
      | Rel rel -> field_type rel.field
    in

    match value with
    | Placeholder -> Any
    | Field field ->  Fixed (field_type field)
    | Join (_, _, field) ->  Fixed (field_type field)
    | Literal literal ->
      begin match Int.of_string literal with
        | _ -> Integer
        | exception _ -> String
      end
    | Call (fn, _) ->
      begin match fn with
        | "lower" -> String
        | _ -> Any
      end
end

type t =
  { models: (string, Model.t ref) Hashtbl.t
  ; fields: (string * string, Model.field ref) Hashtbl.t
  ; cruds: (string, Crud.t ref) Hashtbl.t
  }
