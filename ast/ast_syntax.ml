module Field = struct
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
    | Blob

  type attr =
    | Column of string Ast_annotate.t
    | Nullable
    | Updatable
    | Autoinsert
    | Autoupdate
    | Length of string Ast_annotate.t

  type t =
    { name: string Ast_annotate.t
    ; type_: type_ Ast_annotate.t
    ; attrs: attr Ast_annotate.t list
    }
end

module Rel = struct
  type kind =
    | Setnull
    | Cascade
    | Restrict

  type attr =
    | Column of string Ast_annotate.t
    | Nullable
    | Updatable
  type t =
    { name: string Ast_annotate.t
    ; model: string Ast_annotate.t
    ; field: string Ast_annotate.t
    ; kind: kind Ast_annotate.t
    ; attrs: attr Ast_annotate.t list
    }
end

module Model = struct
  type entry =
    | Table of string Ast_annotate.t
    | Key of string Ast_annotate.t list
    | Unique of string Ast_annotate.t list
    | Index of string Ast_annotate.t list
    | Field of Field.t
    | Rel of Rel.t

  type t =
    { name: string Ast_annotate.t
    ; entries: entry Ast_annotate.t list
    }
end

module Query = struct
  type op =
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equal
    | In

  type term =
    { left_val: value Ast_annotate.t
    ; op: op Ast_annotate.t
    ; right_val: value Ast_annotate.t
    }

  and binop =
    { left_query: t Ast_annotate.t
    ; right_query: t Ast_annotate.t
    }

  and t =
    | Term of term
    | And of binop
    | Or of binop

  and value =
    | Placeholder
    | Field of string Ast_annotate.t
    | Literal of string Ast_annotate.t
    | Call of string Ast_annotate.t * value Ast_annotate.t
    | Join of string Ast_annotate.t * t Ast_annotate.t * string Ast_annotate.t
end

module Create = struct
  type attr =
    | Raw
    | Suffix of string Ast_annotate.t

  type t =
    { attrs: attr Ast_annotate.t list }
end

module Read = struct
  type kind =
    | Has
    | First
    | One
    | All
    | Find
    | Limited
    | Paged

  type direction =
    | Ascending
    | Descending

  type attr =
    | Suffix of string Ast_annotate.t
    | OrderBy of direction

  type t =
    { kind: kind Ast_annotate.t
    ; query: Query.t Ast_annotate.t option
    ; attrs: attr Ast_annotate.t list
    }
end

module Update = struct
  type attr =
    | Suffix of string Ast_annotate.t

  type t =
    { query: Query.t Ast_annotate.t
    ; attrs: attr Ast_annotate.t list
    }
end

module Delete = struct
  type attr =
    | Suffix of string Ast_annotate.t

  type t =
    { query: Query.t Ast_annotate.t
    ; attrs: attr Ast_annotate.t list
    }
end

module Crud = struct
  type entry =
    | Create of Create.t
    | Read of Read.t
    | Update of Update.t
    | Delete of Delete.t

  type t =
    { model: string Ast_annotate.t
    ; entries: entry Ast_annotate.t list
    }
end

type definition =
  | Model of Model.t
  | Crud of Crud.t
