open Core

module Model = struct
  module Index = struct
    type entry =
      | Name of string
      | Fields of string list
      | Unique
    [@@deriving show]

    type t = entry list
    [@@deriving show]
  end

  module Field = struct
    type name = string [@@deriving show]

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
    [@@deriving show]

    type attr =
      | Column of string
      | Nullable
      | Updatable
      | Autoinsert
      | Autoupdate
      | Length of string
    [@@deriving show]

    type t = name * type_ * attr list option
    [@@deriving show]
  end

  module Rel = struct
    type name = string [@@deriving show]
    type model = string [@@deriving show]
    type field = string [@@deriving show]

    type kind =
      | Setnull
      | Cascade
      | Restrict
    [@@deriving show]

    type attr =
      | Column of string
      | Nullable
      | Updatable
    [@@deriving show]

    type t = name * model * field * kind * attr list option
    [@@deriving show]
  end

  type name = string
  [@@deriving show]

  type entry =
    | Table of string
    | Key of string list
    | Unique of string list
    | Index of Index.t
    | Field of Field.t
    | Rel of Rel.t
  [@@deriving show]

  type t = name * entry list
  [@@deriving show]
end

module Create = struct
  type model = string
  [@@deriving show]

  type entry =
    | Raw
    | Suffix of string
  [@@deriving show]

  type t = model * entry list
  [@@deriving show]
end

type definition =
  | Model of Model.t
  | Create of Create.t
[@@deriving show]
