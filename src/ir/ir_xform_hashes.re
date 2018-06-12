open Core;

module StringHash = Hashtbl.Make(String);
module FieldHash =
  Hashtbl.Make({
    [@deriving (sexp, hash, compare)]
    type t = (string, string);
  });
