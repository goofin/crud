open Base

module Fields = struct
  type t = (string * string)
  [@@deriving (sexp, hash, compare)]
end
