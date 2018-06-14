type location =
  { file: string
  ; start_line: int
  ; start_pos: int
  ; end_line: int
  ; end_pos: int
  }
[@@deriving (sexp, hash, compare)]

type 'a t =
  { node: 'a
  ; loc: location
  }

val print_location : ?depth:int -> ?highlight:bool -> ?context:int -> location -> unit