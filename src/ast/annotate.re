open Core;

[@deriving sexp]
type location = {
  file: string,
  start_line: int,
  start_pos: int,
  end_line: int,
  end_pos: int,
};

[@deriving sexp]
type t('a) = {
  node: 'a,
  id: int,
  loc: location,
};
