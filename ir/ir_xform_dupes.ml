open Base
open Crud_ast

type t = string * (string, Annotate.location) Hashtbl.t

let create kind = (kind, Hashtbl.create (module String))

let check (kind, hash) key loc =
  match Hashtbl.find hash key with
  | Some prev_loc -> raise (Ir_error.Exn (Duplicate (kind, prev_loc, loc)))
  | None -> Hashtbl.set hash ~key:key ~data:loc
