{
open Core
open Parser

exception Error of string * Lexing.position

let lexing_error lexbuf =
    let invalid_input = String.make 1 (Lexing.lexeme_char lexbuf 0) in
    raise (Error (invalid_input, lexbuf.Lexing.lex_curr_p))

let needs_comma = ref false

let unquote lexeme =
    String.drop_prefix (String.drop_suffix lexeme 1) 1

let emit token =
    (*
    let open Core in
    printf "%s\n" (match token with
        | COMMA -> "comma"
        | MODEL -> "model"
        | IDENT _ -> "ident"
        | LEFT_PAREN -> "left paren"
        | RIGHT_PAREN -> "right paren"
        | KEY -> "key"
        | FIELD -> "field"
        | _ -> "token"
    );
    *)
    token
}

let white   = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

let number  = ['0'-'9']+
let ident   = ['a'-'z' '_'] ['a'-'z' '_' '0'-'9']*

rule token = parse
    | white+ { token lexbuf }

    (* automatic comma insertion *)
    | '\\'    { needs_comma := false; token lexbuf }
    | newline {
        Lexing.new_line lexbuf;
        if !needs_comma then begin
            needs_comma := false;
            emit COMMA
        end else begin
            token lexbuf
        end
    }

    (* symbols *)
    | ','     { needs_comma := false; emit COMMA }
    | '('     { needs_comma := false; emit LEFT_PAREN }
    | ')'     { needs_comma := true;  emit RIGHT_PAREN }
    | '['     { needs_comma := true;  emit LEFT_BRACKET }
    | ']'     { needs_comma := true;  emit RIGHT_BRACKET }
    | '.'     { needs_comma := true;  emit DOT }
    | '?'     { needs_comma := true;  emit QUESTION }
    | "!="    { needs_comma := true;  emit NOT_EQUAL }
    | '<'     { needs_comma := true;  emit LESS_THAN }
    | "<="    { needs_comma := true;  emit LESS_THAN_OR_EQUAL }
    | '>'     { needs_comma := true;  emit GREATER_THAN }
    | ">="    { needs_comma := true;  emit GREATER_THAN_OR_EQUAL }
    | '='     { needs_comma := true;  emit EQUAL }
    | "in"    { needs_comma := true;  emit IN }

    (* model *)
    | "model"  { needs_comma := true;  emit MODEL }
    | "table"  { needs_comma := true;  emit TABLE }
    | "key"    { needs_comma := true;  emit KEY }
    | "unique" { needs_comma := true;  emit UNIQUE }
    | "index"  { needs_comma := true;  emit INDEX }
    | "field"  { needs_comma := true;  emit FIELD }

    (* index *)
    | "name"   { needs_comma := true;  emit NAME }
    | "fields" { needs_comma := true;  emit FIELDS }
    (* unique: already handled *)

    (* field types *)
    | "serial"     { needs_comma := true;  emit SERIAL }
    | "serial64"   { needs_comma := true;  emit SERIAL64 }
    | "int"        { needs_comma := true;  emit INT }
    | "int64"      { needs_comma := true;  emit INT64 }
    | "uint"       { needs_comma := true;  emit UINT }
    | "uint64"     { needs_comma := true;  emit UINT64 }
    | "bool"       { needs_comma := true;  emit BOOL }
    | "text"       { needs_comma := true;  emit TEXT }
    | "date"       { needs_comma := true;  emit DATE }
    | "timestamp"  { needs_comma := true;  emit TIMESTAMP }
    | "utimestamp" { needs_comma := true;  emit UTIMESTAMP }
    | "float"      { needs_comma := true;  emit FLOAT }
    | "float64"    { needs_comma := true;  emit FLOAT64 }
    | "blob"       { needs_comma := true;  emit BLOB }

    (* field attributes *)
    | "column"     { needs_comma := true;  emit COLUMN }
    | "nullable"   { needs_comma := true;  emit NULLABLE }
    | "updatable"  { needs_comma := true;  emit UPDATABLE }
    | "autoinsert" { needs_comma := true;  emit AUTOINSERT }
    | "autoupdate" { needs_comma := true;  emit AUTOUPDATE }
    | "length"     { needs_comma := true;  emit LENGTH }

    (* relation kinds *)
    | "setnull"  { needs_comma := true;  emit SETNULL }
    | "cascade"  { needs_comma := true;  emit CASCADE }
    | "restrict" { needs_comma := true;  emit RESTRICT }

    (* crud *)
    | "crud"     { needs_comma := true;  emit CRUD }
    | "create"   { needs_comma := true;  emit CREATE }
    | "read"     { needs_comma := true;  emit READ }
    | "update"   { needs_comma := true;  emit UPDATE }
    | "delete"   { needs_comma := true;  emit DELETE }
    | "has"      { needs_comma := true;  emit HAS }
    | "first"    { needs_comma := true;  emit FIRST }
    | "one"      { needs_comma := true;  emit ONE }
    | "all"      { needs_comma := true;  emit ALL }
    | "find"     { needs_comma := true;  emit FIND }
    | "limited"  { needs_comma := true;  emit LIMITED }
    | "paged"    { needs_comma := true;  emit PAGED }
    | "suffix"   { needs_comma := true;  emit SUFFIX }
    | "raw"      { needs_comma := true;  emit RAW }
    | "orderby"  { needs_comma := true; emit ORDERBY }
    | "asc"      { needs_comma := true; emit ASC }
    | "desc"     { needs_comma := true; emit DESC }
    | "noreturn" { needs_comma := true;  emit NORETURN }
    | "and"      { needs_comma := false; emit AND }
    | "or"       { needs_comma := false; emit OR }

    (* some literals *)
    | number  { needs_comma := true;  emit (NUMBER (Lexing.lexeme lexbuf)) }
    | ident   { needs_comma := true;  emit (IDENT  (Lexing.lexeme lexbuf)) }
    | '"'     { needs_comma := true;  read_ident (Buffer.create 0) lexbuf }
    | '\''    { needs_comma := true;  read_quote (Buffer.create 0) lexbuf }

    (* eof *)
    | eof     { EOF }
    | _       { lexing_error lexbuf }

and read_quote buf = parse
    | '\''            { emit (STRING (Buffer.contents buf)) }
    | '\\' '\''       { Buffer.add_char buf '\''; read_quote buf lexbuf }
    | '\\' '\\'       { Buffer.add_char buf '\\'; read_quote buf lexbuf }
    | [^ '\'' '\\' ]+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_quote buf lexbuf }
    | _   { lexing_error lexbuf }
    | eof { raise (Error ("EOF", lexbuf.Lexing.lex_curr_p)) }

and read_ident buf = parse
    | '"'            { emit (IDENT (Buffer.contents buf)) }
    | '\\' '"'       { Buffer.add_char buf '"';  read_ident buf lexbuf }
    | '\\' '\\'      { Buffer.add_char buf '\\'; read_ident buf lexbuf }
    | [^ '"' '\\' ]+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_ident buf lexbuf }
    | _   { lexing_error lexbuf }
    | eof { raise (Error ("EOF", lexbuf.Lexing.lex_curr_p)) }