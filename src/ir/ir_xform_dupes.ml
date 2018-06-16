open Core
open Crud_ast
open Ir_xform_hashes

type t = string * Annotate.location StringHash.t

let create kind = (kind, StringHash.create ())

let check (kind, hash) key loc =
  match StringHash.find hash key with
  | Some prev_loc -> raise (Ir_error.Exn (Duplicate (kind, prev_loc, loc)))
  | None -> StringHash.set hash key loc
