open Core

module StringHash = Hashtbl.Make(String)

module FieldHash = Hashtbl.Make(struct
    type t = (string * string)
    [@@deriving (sexp, hash, compare)]
  end)