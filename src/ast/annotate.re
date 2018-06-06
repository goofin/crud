open Core;

type location = {
  file: string,
  start_line: int,
  start_pos: int,
  end_line: int,
  end_pos: int,
};

type t('a) = {
  node: 'a,
  id: int,
  loc: location,
};
