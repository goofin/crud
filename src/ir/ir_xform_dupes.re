open Core;
open Crud_ast;
open Ir_xform_hashes;

type t = (string, StringHash.t(Annotate.location));

let create = kind => (kind, StringHash.create());

let check = ((kind, hash), key, loc) =>
  switch (StringHash.find(hash, key)) {
  | Some(prev_loc) => raise(Ir_error.Exn(Duplicate(kind, prev_loc, loc)))
  | None => StringHash.set(hash, key, loc)
  };
