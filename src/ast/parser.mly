// symbols
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token DOT
%token QUESTION
%token NOT_EQUAL
%token LESS_THAN
%token LESS_THAN_OR_EQUAL
%token GREATER_THAN
%token GREATER_THAN_OR_EQUAL
%token EQUAL
%token IN

// model tokens
%token MODEL
%token TABLE
%token KEY
%token UNIQUE
%token INDEX
%token FIELD

// index tokens
%token NAME
%token FIELDS

// field types
%token SERIAL
%token SERIAL64
%token INT
%token INT64
%token UINT
%token UINT64
%token BOOL
%token TEXT
%token DATE
%token TIMESTAMP
%token UTIMESTAMP
%token FLOAT
%token FLOAT64
%token BLOB

// field attrs
%token COLUMN
%token NULLABLE
%token UPDATABLE
%token AUTOINSERT
%token AUTOUPDATE
%token LENGTH

// relation kind
%token SETNULL
%token CASCADE
%token RESTRICT

// crud
%token CRUD
%token CREATE
%token READ
%token UPDATE
%token DELETE
%token HAS
%token FIRST
%token ONE
%token ALL
%token FIND
%token LIMITED
%token PAGED
%token SUFFIX
%token RAW
%token NORETURN
%token ORDERBY
%token ASC
%token DESC
%token AND
%token OR

%left AND OR

// some literals
%token <string> NUMBER
%token <string> IDENT
%token <string> STRING

%token EOF

%start <Syntax.definition list> dbx

%{
let global_id = ref 0;;

let annotate start_pos end_pos node =
    let id        = !global_id in
    global_id := id + 1;
    { Syntax.Annotate.
      node = node
    ; id   = id
    ; loc  = { file       = start_pos.Lexing.pos_fname
             ; start_line = start_pos.Lexing.pos_lnum
             ; start_pos  = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol
             ; end_line   = end_pos.Lexing.pos_lnum
             ; end_pos    = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol
             }
    }
;;
%}

%%

///////////////////////////////////////////////////////////////////////////////
// helpers
///////////////////////////////////////////////////////////////////////////////

//
// support for left recursive list that allows optional trailing delimiter
//

rev_nonempty_sep_list(DELIM, X):
  | x = X { [ x ] }
  | xs = rev_nonempty_sep_list(DELIM, X); DELIM; x = X { x :: xs }
  ;

%inline
rev_sep_list(DELIM, X):
    | { [] }
    | xs = rev_nonempty_sep_list(DELIM, X) { xs }
    ;

%inline
sep_list(DELIM, X):
  xs = rev_sep_list(DELIM, X) DELIM? { List.rev xs }

//
// annotation helpers
//

annotated_ident:
    ident = IDENT
    { annotate $startpos $endpos (ident) }
    ;

annotated_number:
    number = NUMBER
    { annotate $startpos $endpos (number) }
    ;

annotated_string:
    string = STRING
    { annotate $startpos $endpos (string) }
    ;

///////////////////////////////////////////////////////////////////////////////
// dbx
///////////////////////////////////////////////////////////////////////////////

dbx:
    defs = sep_list(COMMA, parse_definition)
    EOF
    { defs }
    ;

parse_definition:
    | model = parse_model { model }
    | crud  = parse_crud  { crud }
    ;

///////////////////////////////////////////////////////////////////////////////
// models
///////////////////////////////////////////////////////////////////////////////

parse_model:
    MODEL
    name = annotated_ident
    LEFT_PAREN
    entries = sep_list(COMMA, parse_model_entry)
    RIGHT_PAREN
    { Syntax.Model (annotate $startpos $endpos (name, entries)) }
    ;

parse_model_entry:
    | table  = parse_model_table  { table }
    | key    = parse_model_key    { key }
    | unique = parse_model_unique { unique }
    | index  = parse_model_index  { index }
    | field  = parse_model_field  { field }
    | rel    = parse_model_rel    { rel }
    ;

//
// model table
//

parse_model_table:
    TABLE
    name = annotated_ident
    { Syntax.Model.Table (annotate $startpos $endpos (name)) }
    ;

//
// model key
//

parse_model_key:
    KEY
    fields = list(annotated_ident)
    { Syntax.Model.Key (annotate $startpos $endpos (fields)) }
    ;

//
// model unique
//

parse_model_unique:
    UNIQUE
    fields = list(annotated_ident)
    { Syntax.Model.Unique (annotate $startpos $endpos (fields)) }
    ;

//
// model index
//

parse_model_index:
    INDEX
    LEFT_PAREN
    entries = sep_list(COMMA, parse_model_index_entry)
    RIGHT_PAREN
    { Syntax.Model.Index (annotate $startpos $endpos (entries)) }
    ;

parse_model_index_entry:
    | name   = parse_model_index_name   { name }
    | fields = parse_model_index_fields { fields }
    | unique = parse_model_index_unique { unique }
    ;

parse_model_index_name:
    NAME
    name = annotated_ident
    { annotate $startpos $endpos (Syntax.Model.Index.Name name) }
    ;

parse_model_index_fields:
    FIELDS
    fields = list(annotated_ident)
    { annotate $startpos $endpos (Syntax.Model.Index.Fields fields) }
    ;

parse_model_index_unique:
    UNIQUE
    { annotate $startpos $endpos (Syntax.Model.Index.Unique) }
    ;

//
// model fiels
//

parse_model_field:
    FIELD
    name = annotated_ident
    typ = parse_model_field_type
    attrs = option(parse_model_field_attrs)
    { Syntax.Model.Field (annotate $startpos $endpos (name, typ, attrs)) }
    ;

parse_model_field_type:
    | SERIAL     { annotate $startpos $endpos (Syntax.Model.Field.Serial) }
    | SERIAL64   { annotate $startpos $endpos (Syntax.Model.Field.Serial64) }
    | INT        { annotate $startpos $endpos (Syntax.Model.Field.Int) }
    | INT64      { annotate $startpos $endpos (Syntax.Model.Field.Int64) }
    | UINT       { annotate $startpos $endpos (Syntax.Model.Field.Uint) }
    | UINT64     { annotate $startpos $endpos (Syntax.Model.Field.Uint64) }
    | BOOL       { annotate $startpos $endpos (Syntax.Model.Field.Bool) }
    | TEXT       { annotate $startpos $endpos (Syntax.Model.Field.Text) }
    | DATE       { annotate $startpos $endpos (Syntax.Model.Field.Date) }
    | TIMESTAMP  { annotate $startpos $endpos (Syntax.Model.Field.Timestamp) }
    | UTIMESTAMP { annotate $startpos $endpos (Syntax.Model.Field.Utimestamp) }
    | FLOAT      { annotate $startpos $endpos (Syntax.Model.Field.Float) }
    | FLOAT64    { annotate $startpos $endpos (Syntax.Model.Field.Float64) }
    | BLOB       { annotate $startpos $endpos (Syntax.Model.Field.Blob) }
    ;

parse_model_field_attrs:
    LEFT_PAREN
    attrs = sep_list(COMMA, parse_model_field_attr)
    RIGHT_PAREN
    { attrs }
    ;

parse_model_field_attr:
    | COLUMN name = annotated_ident    { annotate $startpos $endpos (Syntax.Model.Field.Column name) }
    | NULLABLE                         { annotate $startpos $endpos (Syntax.Model.Field.Nullable) }
    | UPDATABLE                        { annotate $startpos $endpos (Syntax.Model.Field.Updatable) }
    | AUTOINSERT                       { annotate $startpos $endpos (Syntax.Model.Field.Autoinsert) }
    | AUTOUPDATE                       { annotate $startpos $endpos (Syntax.Model.Field.Autoupdate) }
    | LENGTH length = annotated_number { annotate $startpos $endpos (Syntax.Model.Field.Length length) }
    ;

parse_model_rel:
    FIELD
    name = annotated_ident
    model = annotated_ident
    DOT
    field = annotated_ident
    kind = parse_model_rel_kind
    attrs = option(parse_model_rel_attrs)
    { Syntax.Model.Rel (annotate $startpos $endpos (name, model, field, kind, attrs)) }
    ;

parse_model_rel_kind:
    | SETNULL  { annotate $startpos $endpos (Syntax.Model.Rel.Setnull) }
    | CASCADE  { annotate $startpos $endpos (Syntax.Model.Rel.Cascade) }
    | RESTRICT { annotate $startpos $endpos (Syntax.Model.Rel.Restrict) }
    ;

parse_model_rel_attrs:
    LEFT_PAREN
    attrs = sep_list(COMMA, parse_model_rel_attr)
    RIGHT_PAREN
    { attrs }
    ;

parse_model_rel_attr:
    | COLUMN name = annotated_ident { annotate $startpos $endpos (Syntax.Model.Rel.Column name) }
    | NULLABLE                      { annotate $startpos $endpos (Syntax.Model.Rel.Nullable) }
    | UPDATABLE                     { annotate $startpos $endpos (Syntax.Model.Rel.Updatable) }
    ;

///////////////////////////////////////////////////////////////////////////////
// cruds
///////////////////////////////////////////////////////////////////////////////

parse_crud:
    CRUD
    name = annotated_ident
    entries = parse_crud_entries
    { Syntax.Crud (annotate $startpos $endpos (name, entries)) }
    ;

parse_crud_attrs_optional(inner):
    attrs = parse_crud_attrs(inner)
    { annotate $startpos $endpos (attrs) }
    ;

parse_crud_attrs(inner):
    LEFT_PAREN
    attrs = sep_list(COMMA, inner)
    RIGHT_PAREN
    { attrs }
    ;

parse_crud_entries:
    LEFT_PAREN
    entries = sep_list(COMMA, parse_crud_entry)
    RIGHT_PAREN
    { entries }
    ;

parse_crud_entry:
    | create = parse_crud_create { create }
    | read   = parse_crud_read   { read }
    | update = parse_crud_update { update }
    | delete = parse_crud_delete { delete }
    ;

//
// crud create
//

parse_crud_create:
    CREATE
    attrs = parse_crud_attrs(parse_crud_create_attr)
    { annotate $startpos $endpos (Syntax.Crud.Create attrs) }
    ;

parse_crud_create_attr:
    | RAW                             { annotate $startpos $endpos (Syntax.Crud.Create.Raw) }
    | SUFFIX suffix = annotated_ident { annotate $startpos $endpos (Syntax.Crud.Create.Suffix suffix) }

//
// crud read
//

parse_crud_read:
    | read = parse_crud_read_noquery { read }
    | read = parse_crud_read_query   { read }
    ;

parse_crud_read_noquery:
    READ
    kind = parse_crud_read_kind
    attrs = option(parse_crud_attrs(parse_crud_read_attr))
    { annotate $startpos $endpos (Syntax.Crud.Read (kind, None, attrs)) }
    ;

parse_crud_read_query:
    READ
    kind = parse_crud_read_kind
    query = parse_crud_query
    attrs = option(parse_crud_attrs(parse_crud_read_attr))
    { annotate $startpos $endpos (Syntax.Crud.Read (kind, Some query, attrs)) }
    ;

parse_crud_read_kind:
    | HAS     { annotate $startpos $endpos (Syntax.Crud.Read.Has) }
    | FIRST   { annotate $startpos $endpos (Syntax.Crud.Read.First) }
    | ONE     { annotate $startpos $endpos (Syntax.Crud.Read.One) }
    | ALL     { annotate $startpos $endpos (Syntax.Crud.Read.All) }
    | FIND    { annotate $startpos $endpos (Syntax.Crud.Read.Find) }
    | LIMITED { annotate $startpos $endpos (Syntax.Crud.Read.Limited) }
    | PAGED   { annotate $startpos $endpos (Syntax.Crud.Read.Paged) }

parse_crud_read_attr:
    | orderby = parse_crud_read_attr_orderby { orderby }
    | suffix  = parse_crud_read_attr_suffix  { suffix }

parse_crud_read_attr_orderby:
    ORDERBY
    direction = parse_crud_read_attr_orderby_direction
    { annotate $startpos $endpos (Syntax.Crud.Read.OrderBy direction) }
    ;

parse_crud_read_attr_orderby_direction:
    | ASC  { annotate $startpos $endpos (Syntax.Crud.Read.Ascending) }
    | DESC { annotate $startpos $endpos (Syntax.Crud.Read.Descending) }
    ;

parse_crud_read_attr_suffix:
    SUFFIX
    suffix = annotated_ident
    { annotate $startpos $endpos (Syntax.Crud.Read.Suffix suffix) }
    ;

//
// crud update
//

parse_crud_update:
    UPDATE
    query = parse_crud_query
    attrs = option(parse_crud_attrs(parse_crud_update_attr))
    { annotate $startpos $endpos (Syntax.Crud.Update (query, attrs)) }
    ;

parse_crud_update_attr:
    | SUFFIX suffix = annotated_ident { annotate $startpos $endpos (Syntax.Crud.Update.Suffix suffix) }
    ;

//
// crud delete
//

parse_crud_delete:
    DELETE
    query = parse_crud_query
    attrs = option(parse_crud_attrs(parse_crud_delete_attr))
    { annotate $startpos $endpos (Syntax.Crud.Delete (query, attrs)) }
    ;

parse_crud_delete_attr:
    | SUFFIX suffix = annotated_ident { annotate $startpos $endpos (Syntax.Crud.Delete.Suffix suffix) }
    ;

//
// crud query
//

parse_crud_query:
    | group = parse_crud_query_group { group }
    | term  = parse_crud_query_term  { term }
    | and_  = parse_crud_query_and   { and_ }
    | or_   = parse_crud_query_or    { or_ }
    ;

parse_crud_query_group:
    LEFT_PAREN
    query = parse_crud_query
    RIGHT_PAREN
    { query }
    ;

parse_crud_query_term:
    left = parse_crud_query_value
    op = parse_crud_query_op
    right = parse_crud_query_value
    { annotate $startpos $endpos (Syntax.Crud.Query.Term (left, op, right)) }
    ;

parse_crud_query_op:
    | NOT_EQUAL             { annotate $startpos $endpos (Syntax.Crud.Query.NotEqual) }
    | LESS_THAN             { annotate $startpos $endpos (Syntax.Crud.Query.LessThan) }
    | LESS_THAN_OR_EQUAL    { annotate $startpos $endpos (Syntax.Crud.Query.LessThanOrEqual) }
    | GREATER_THAN          { annotate $startpos $endpos (Syntax.Crud.Query.GreaterThan) }
    | GREATER_THAN_OR_EQUAL { annotate $startpos $endpos (Syntax.Crud.Query.GreaterThanOrEqual) }
    | EQUAL                 { annotate $startpos $endpos (Syntax.Crud.Query.Equal) }
    | IN                    { annotate $startpos $endpos (Syntax.Crud.Query.In) }
    ;

parse_crud_query_value:
    | QUESTION                              { annotate $startpos $endpos (Syntax.Crud.Query.Placeholder) }
    | number = annotated_number             { annotate $startpos $endpos (Syntax.Crud.Query.Literal number) }
    | string = annotated_string             { annotate $startpos $endpos (Syntax.Crud.Query.Literal string) }
    | DOT field = annotated_ident           { annotate $startpos $endpos (Syntax.Crud.Query.Field field) }
    | call    = parse_crud_query_value_call { call }
    | join    = parse_crud_query_value_join { join }
    ;

parse_crud_query_value_call:
    func = annotated_ident
    LEFT_PAREN
    arg = parse_crud_query_value
    RIGHT_PAREN
    { annotate $startpos $endpos (Syntax.Crud.Query.Call (func, arg)) }
    ;

parse_crud_query_value_join:
    model = annotated_ident
    LEFT_PAREN
    query = parse_crud_query
    RIGHT_PAREN
    DOT
    field = annotated_ident
    { annotate $startpos $endpos (Syntax.Crud.Query.Join (model, query, field)) }
    ;

parse_crud_query_and:
    left = parse_crud_query
    AND
    right = parse_crud_query
    { annotate $startpos $endpos (Syntax.Crud.Query.And (left, right)) }
    ;

parse_crud_query_or:
    left = parse_crud_query
    OR
    right = parse_crud_query
    { annotate $startpos $endpos (Syntax.Crud.Query.Or (left, right)) }
    ;
