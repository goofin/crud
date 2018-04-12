{
open Parser

exception SyntaxError of string

let needs_comma = ref false
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let number  = ['0'-'9']+
let ident   = ['a'-'z' '_'] ['a'-'z' '_' '0'-'9']*
let quoted  = '"' [^'"']* '"'

rule read =
    parse
    | white   { read lexbuf }

    (* automatic comma insertion *)
    | newline {
        if !needs_comma then begin
            needs_comma := false;
            COMMA
        end else begin
            read lexbuf
        end
    }

    (* symbols *)
    | ','     { needs_comma := false; COMMA }
    | '('     { needs_comma := false; LEFT_PAREN }
    | ')'     { needs_comma := true;  RIGHT_PAREN }
    | '.'     { needs_comma := true;  DOT }
    | '?'     { needs_comma := true;  QUESTION }
    | "!="    { needs_comma := true;  COMPARISON (Lexing.lexeme lexbuf) }
    | '<'     { needs_comma := true;  COMPARISON (Lexing.lexeme lexbuf) }
    | "<="    { needs_comma := true;  COMPARISON (Lexing.lexeme lexbuf) }
    | '>'     { needs_comma := true;  COMPARISON (Lexing.lexeme lexbuf) }
    | ">="    { needs_comma := true;  COMPARISON (Lexing.lexeme lexbuf) }
    | '='     { needs_comma := true;  COMPARISON (Lexing.lexeme lexbuf) }

    (* some literals *)
    | number  { needs_comma := true; NUMBER (Lexing.lexeme lexbuf) }
    | ident   { needs_comma := true; IDENT  (Lexing.lexeme lexbuf) }
    | quoted  { needs_comma := true; QUOTED (Lexing.lexeme lexbuf) }

    (* keywords *)
    (*
    | "model"
    | "table"
    | "key"
    | "unique"

    | "index"
    | "name"
    | "fields"

    | "field"
    | "column"
    | "nullable"
    | "updatable"
    | "autoinsert"
    | "autoupdate"
    | "length"
    | "serial"
    | "serial64"
    | "int"
    | "int64"
    | "uint"
    | "uint64"
    | "bool"
    | "text"
    | "timestamp"
    | "utimestamp"
    | "float"
    | "float64"
    | "blob"
    | "setnull"
    | "cascade"
    | "restrict"

    | "create"
    | "raw"
    | "suffix"

    | "read"
    | "count"
    | "has"
    | "first"
    | "scalar"
    | "one"
    | "all"
    | "limitoffset"
    | "paged"

    | "update"

    | "delete"
    *)

    | eof     { EOF }
    | _       { raise (SyntaxError (Lexing.lexeme lexbuf)) }
