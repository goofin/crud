open Core;

let definition: (~depth: int=?, Ast_syntax.definition) => unit;
let print: (~depth: int=?, Ast_annotate.t(Ast_syntax.definition)) => unit;
