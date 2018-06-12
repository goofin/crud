[@deriving (sexp, hash, compare)]
type location = {
  file: string,
  start_line: int,
  start_pos: int,
  end_line: int,
  end_pos: int,
};

type t('a) = {
  node: 'a,
  loc: location,
};

let print_location: (~depth: int=?, ~highlight: bool=?, ~context: int=?, location) => unit;
