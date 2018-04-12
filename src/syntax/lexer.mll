{
open Parser

exception SyntaxError of string

let needs_comma = ref false
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let ident   = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let symbol  = '.' | '?' | '=' | '>' | '<'

rule read =
    parse
    | white   { read lexbuf }

    | newline {
        if !needs_comma then begin
            needs_comma := false;
            COMMA
        end else begin
            read lexbuf
        end
    }

    | ','     { needs_comma := false; COMMA }
    | '('     { needs_comma := false; LEFT_PAREN }
    | ')'     { needs_comma := true;  RIGHT_PAREN }
    | symbol  { needs_comma := true;  IDENT (Lexing.lexeme lexbuf) }
    | ident   { needs_comma := true;  IDENT (Lexing.lexeme lexbuf) }
    | eof     { EOF }
    | _       { raise (SyntaxError (Lexing.lexeme lexbuf)) }
