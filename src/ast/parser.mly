// symbols
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token DOT
%token QUESTION
%token <string> COMPARISON

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
%token AND
%token OR

// some literals
%token <string> NUMBER
%token <string> IDENT

%token EOF

%start <Syntax.definition list> prog
%%

// support for left recursive list that allows optional trailing delimiter

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

// end support for left recursion

prog:
    defs = sep_list(COMMA, parse_definition)
    EOF
    { defs }
    ;

parse_definition:
    | model = parse_model { model }
    | crud  = parse_crud  { crud }
    ;

parse_model:
    MODEL
    name = IDENT
    entries = parse_model_entries
    { Syntax.Model (name, entries) }
    ;

parse_model_entries:
    LEFT_PAREN
    entries = sep_list(COMMA, parse_model_entry)
    RIGHT_PAREN
    { entries }
    ;

parse_model_entry:
    | table  = parse_model_table  { table }
    | key    = parse_model_key    { key }
    | unique = parse_model_unique { unique }
    | index  = parse_model_index  { index }
    | field  = parse_model_field  { field }
    | rel    = parse_model_rel    { rel }
    ;

parse_model_table:
    TABLE
    name = IDENT
    { Syntax.Model.Table name }
    ;

parse_model_key:
    KEY
    fields = list(IDENT)
    { Syntax.Model.Key fields }
    ;

parse_model_unique:
    UNIQUE
    fields = list(IDENT)
    { Syntax.Model.Unique fields }
    ;

parse_model_index:
    INDEX
    LEFT_PAREN
    entries = sep_list(COMMA, parse_model_index_entry)
    RIGHT_PAREN
    { Syntax.Model.Index entries }
    ;

parse_model_index_entry:
    | name   = parse_model_index_name   { name }
    | fields = parse_model_index_fields { fields }
    | unique = parse_model_index_unique { unique }
    ;

parse_model_index_name:
    NAME
    name = IDENT
    { Syntax.Model.Index.Name name }
    ;

parse_model_index_fields:
    FIELDS
    fields = list(IDENT)
    { Syntax.Model.Index.Fields fields }
    ;

parse_model_index_unique:
    UNIQUE
    { Syntax.Model.Index.Unique }
    ;

parse_model_field:
    FIELD
    name = IDENT
    typ = parse_model_field_type
    attrs = option(parse_model_field_attrs)
    { Syntax.Model.Field (name, typ, attrs) }
    ;

parse_model_field_type:
    | SERIAL     { Syntax.Model.Field.Serial }
    | SERIAL64   { Syntax.Model.Field.Serial64 }
    | INT        { Syntax.Model.Field.Int }
    | INT64      { Syntax.Model.Field.Int64 }
    | UINT       { Syntax.Model.Field.Uint }
    | UINT64     { Syntax.Model.Field.Uint64 }
    | BOOL       { Syntax.Model.Field.Bool }
    | TEXT       { Syntax.Model.Field.Text }
    | DATE       { Syntax.Model.Field.Date }
    | TIMESTAMP  { Syntax.Model.Field.Timestamp }
    | UTIMESTAMP { Syntax.Model.Field.Utimestamp }
    | FLOAT      { Syntax.Model.Field.Float }
    | FLOAT64    { Syntax.Model.Field.Float64 }
    | BLOB       { Syntax.Model.Field.Blob }
    ;

parse_model_field_attrs:
    LEFT_PAREN
    attrs = sep_list(COMMA, parse_model_field_attr)
    RIGHT_PAREN
    { attrs }
    ;

parse_model_field_attr:
    | COLUMN name = IDENT    { Syntax.Model.Field.Column name }
    | NULLABLE               { Syntax.Model.Field.Nullable }
    | UPDATABLE              { Syntax.Model.Field.Updatable }
    | AUTOINSERT             { Syntax.Model.Field.Autoinsert }
    | AUTOUPDATE             { Syntax.Model.Field.Autoupdate }
    | LENGTH length = NUMBER { Syntax.Model.Field.Length length }
    ;

parse_model_rel:
    FIELD
    name = IDENT
    model = IDENT
    DOT
    field = IDENT
    kind = parse_model_rel_kind
    attrs = option(parse_model_rel_attrs)
    { Syntax.Model.Rel (name, model, field, kind, attrs) }
    ;

parse_model_rel_kind:
    | SETNULL  { Syntax.Model.Rel.Setnull }
    | CASCADE  { Syntax.Model.Rel.Cascade }
    | RESTRICT { Syntax.Model.Rel.Restrict }
    ;

parse_model_rel_attrs:
    LEFT_PAREN
    attrs = sep_list(COMMA, parse_model_rel_attr)
    RIGHT_PAREN
    { attrs }
    ;

parse_model_rel_attr:
    | COLUMN name = IDENT { Syntax.Model.Rel.Column name }
    | NULLABLE            { Syntax.Model.Rel.Nullable }
    | UPDATABLE           { Syntax.Model.Rel.Updatable }
    ;

parse_crud:
    CRUD
    name = IDENT
    entries = parse_crud_entries
    { Syntax.Crud (name, entries) }
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

parse_crud_create:
    CREATE
    LEFT_PAREN
    attrs = sep_list(COMMA, parse_crud_create_attr)
    RIGHT_PAREN
    { Syntax.Crud.Create attrs }
    ;

parse_crud_create_attr:
    | RAW                   { Syntax.Crud.Create.Raw }
    | SUFFIX suffix = IDENT { Syntax.Crud.Create.Suffix suffix }

parse_crud_read:
    READ
    kind = parse_crud_read_kind
    query = parse_crud_query
    attrs = option(parse_crud_read_attrs)
    { Syntax.Crud.Read (kind, query, attrs) }
    ;

parse_crud_read_kind:
    | HAS     { Syntax.Crud.Read.Has }
    | FIRST   { Syntax.Crud.Read.First }
    | ONE     { Syntax.Crud.Read.One }
    | ALL     { Syntax.Crud.Read.All }
    | FIND    { Syntax.Crud.Read.Find }
    | LIMITED { Syntax.Crud.Read.Limited }
    | PAGED   { Syntax.Crud.Read.Paged }

parse_crud_read_attrs:
    LEFT_PAREN
    attrs = sep_list(COMMA, parse_crud_read_attr)
    RIGHT_PAREN
    { attrs }
    ;

parse_crud_read_attr:
    | SUFFIX suffix = IDENT { Syntax.Crud.Read.Suffix suffix }
    ;    

parse_crud_update:
    UPDATE
    query = parse_crud_query
    attrs = option(parse_crud_update_attrs)
    { Syntax.Crud.Update (query, attrs) }
    ;

parse_crud_update_attrs:
    LEFT_PAREN
    attrs = sep_list(COMMA, parse_crud_update_attr)
    RIGHT_PAREN
    { attrs }
    ;

parse_crud_update_attr:
    | SUFFIX suffix = IDENT { Syntax.Crud.Update.Suffix suffix }
    ;    

parse_crud_delete:
    DELETE
    query = parse_crud_query
    attrs = option(parse_crud_delete_attrs)
    { Syntax.Crud.Delete (query, attrs) }
    ;

parse_crud_delete_attrs:
    LEFT_PAREN
    attrs = sep_list(COMMA, parse_crud_delete_attr)
    RIGHT_PAREN
    { attrs }
    ;

parse_crud_delete_attr:
    | SUFFIX suffix = IDENT { Syntax.Crud.Delete.Suffix suffix }
    ;    

parse_crud_query:
    LEFT_PAREN
    clauses = sep_list(COMMA, parse_crud_query_clause)
    RIGHT_PAREN
    { clauses }
    ;

parse_crud_query_clause:
    left = parse_crud_query_value
    op = COMPARISON
    right = parse_crud_query_value
    { (left, op, right) }
    ;

parse_crud_query_value:
    | QUESTION                           { Syntax.Crud.Query.Placeholder }
    | field = IDENT                      { Syntax.Crud.Query.Field field }
    | call = parse_crud_query_value_call { call }
    | join = parse_crud_query_value_join { join }
    ;

parse_crud_query_value_call:
    func = IDENT
    LEFT_PAREN
    arg = parse_crud_query_value
    RIGHT_PAREN
    { Syntax.Crud.Query.Call (func, arg) }
    ;

parse_crud_query_value_join:
    model = IDENT
    LEFT_BRACKET
    query = parse_crud_query_clause
    RIGHT_BRACKET
    DOT
    field = IDENT
    { Syntax.Crud.Query.Join (model, query, field) }
    ;
