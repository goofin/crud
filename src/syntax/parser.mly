// symbols
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
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

// field attributes
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

// create
%token CREATE
%token RAW
%token SUFFIX

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
    | model  = parse_model { model }
    | create = parse_create { create }
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
    attrs = option(parse_model_field_attributes)
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

parse_model_field_attributes:
    LEFT_PAREN
    attrs = sep_list(COMMA, parse_model_field_attributes_attr)
    RIGHT_PAREN
    { attrs }
    ;

parse_model_field_attributes_attr:
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
    attrs = option(parse_model_rel_attributes)
    { Syntax.Model.Rel (name, model, field, kind, attrs) }
    ;

parse_model_rel_kind:
    | SETNULL  { Syntax.Model.Rel.Setnull }
    | CASCADE  { Syntax.Model.Rel.Cascade }
    | RESTRICT { Syntax.Model.Rel.Restrict }
    ;

parse_model_rel_attributes:
    LEFT_PAREN
    attrs = sep_list(COMMA, parse_model_rel_attributes_attr);
    RIGHT_PAREN
    { attrs }
    ;

parse_model_rel_attributes_attr:
    | COLUMN name = IDENT { Syntax.Model.Rel.Column name }
    | NULLABLE            { Syntax.Model.Rel.Nullable }
    | UPDATABLE           { Syntax.Model.Rel.Updatable }
    ;

parse_create:
    CREATE
    model = IDENT
    entries = parse_create_entries
    { Syntax.Create (model, entries) }
    ;

parse_create_entries:
    LEFT_PAREN
    entries = sep_list(COMMA, parse_create_entry)
    RIGHT_PAREN
    { entries }
    ;

parse_create_entry:
    | raw    = parse_create_raw    { raw }
    | suffix = parse_create_suffix { suffix }

parse_create_raw:
    RAW
    { Syntax.Create.Raw }
    ;

parse_create_suffix:
    SUFFIX
    suffix = IDENT
    { Syntax.Create.Suffix suffix }
    ;
