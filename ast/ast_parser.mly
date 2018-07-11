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

%start <Ast_syntax.definition Ast_annotate.t list> dbx

%{
let annotate start_pos end_pos node =
    { Ast_annotate.node; loc =
        { file       = start_pos.Lexing.pos_fname
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

//
// bare and comma separated list helpers
//

parse_bare_or_list(X):
    | LEFT_PAREN entries = sep_list(COMMA, X) RIGHT_PAREN { entries }
    | entries = list(X)                                   { entries }
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
    { annotate $startpos $endpos (Ast_syntax.Model
          { Ast_syntax.Model.name
          ; entries
          })
    }
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
    { annotate $startpos $endpos (Ast_syntax.Model.Table name) }
    ;

//
// model key
//

parse_model_key:
    KEY
    fields = parse_bare_or_list(annotated_ident)
    { annotate $startpos $endpos (Ast_syntax.Model.Key fields) }
    ;

//
// model unique
//

parse_model_unique:
    UNIQUE
    fields = parse_bare_or_list(annotated_ident)
    { annotate $startpos $endpos (Ast_syntax.Model.Unique fields) }
    ;

//
// model index
//

parse_model_index:
    INDEX
    fields = parse_bare_or_list(annotated_ident)
    { annotate $startpos $endpos (Ast_syntax.Model.Index fields) }
    ;

//
// model fiels
//

parse_model_field:
    FIELD
    name = annotated_ident
    type_ = parse_model_field_type
    attrs = parse_bare_or_list(parse_model_field_attr)
    { annotate $startpos $endpos (Ast_syntax.Model.Field
        { Ast_syntax.Field.name
        ; type_
        ; attrs
        })
    }
    ;

parse_model_field_type:
    | SERIAL     { annotate $startpos $endpos (Ast_syntax.Field.Serial) }
    | SERIAL64   { annotate $startpos $endpos (Ast_syntax.Field.Serial64) }
    | INT        { annotate $startpos $endpos (Ast_syntax.Field.Int) }
    | INT64      { annotate $startpos $endpos (Ast_syntax.Field.Int64) }
    | UINT       { annotate $startpos $endpos (Ast_syntax.Field.Uint) }
    | UINT64     { annotate $startpos $endpos (Ast_syntax.Field.Uint64) }
    | BOOL       { annotate $startpos $endpos (Ast_syntax.Field.Bool) }
    | TEXT       { annotate $startpos $endpos (Ast_syntax.Field.Text) }
    | DATE       { annotate $startpos $endpos (Ast_syntax.Field.Date) }
    | TIMESTAMP  { annotate $startpos $endpos (Ast_syntax.Field.Timestamp) }
    | UTIMESTAMP { annotate $startpos $endpos (Ast_syntax.Field.Utimestamp) }
    | FLOAT      { annotate $startpos $endpos (Ast_syntax.Field.Float) }
    | FLOAT64    { annotate $startpos $endpos (Ast_syntax.Field.Float64) }
    | BLOB       { annotate $startpos $endpos (Ast_syntax.Field.Blob) }
    ;

parse_model_field_attr:
    | COLUMN name = annotated_ident    { annotate $startpos $endpos (Ast_syntax.Field.Column name) }
    | NULLABLE                         { annotate $startpos $endpos (Ast_syntax.Field.Nullable) }
    | UPDATABLE                        { annotate $startpos $endpos (Ast_syntax.Field.Updatable) }
    | AUTOINSERT                       { annotate $startpos $endpos (Ast_syntax.Field.Autoinsert) }
    | AUTOUPDATE                       { annotate $startpos $endpos (Ast_syntax.Field.Autoupdate) }
    | LENGTH length = annotated_number { annotate $startpos $endpos (Ast_syntax.Field.Length length) }
    ;

parse_model_rel:
    FIELD
    name = annotated_ident
    model = annotated_ident
    DOT
    field = annotated_ident
    kind = parse_model_rel_kind
    attrs = parse_bare_or_list(parse_model_rel_attr)
    { annotate $startpos $endpos (Ast_syntax.Model.Rel
        { Ast_syntax.Rel.name
        ; model
        ; field
        ; kind
        ; attrs
        })
    }
    ;

parse_model_rel_kind:
    | SETNULL  { annotate $startpos $endpos (Ast_syntax.Rel.Setnull) }
    | CASCADE  { annotate $startpos $endpos (Ast_syntax.Rel.Cascade) }
    | RESTRICT { annotate $startpos $endpos (Ast_syntax.Rel.Restrict) }
    ;

parse_model_rel_attr:
    | COLUMN name = annotated_ident { annotate $startpos $endpos (Ast_syntax.Rel.Column name) }
    | NULLABLE                      { annotate $startpos $endpos (Ast_syntax.Rel.Nullable) }
    | UPDATABLE                     { annotate $startpos $endpos (Ast_syntax.Rel.Updatable) }
    ;

///////////////////////////////////////////////////////////////////////////////
// cruds
///////////////////////////////////////////////////////////////////////////////

parse_crud:
    CRUD
    model = annotated_ident
    entries = parse_crud_entries
    { annotate $startpos $endpos (Ast_syntax.Crud
        { Ast_syntax.Crud.model
        ; entries
        })
    }
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
    attrs = parse_bare_or_list(parse_crud_create_attr)
    { annotate $startpos $endpos (Ast_syntax.Crud.Create
        { Ast_syntax.Create.attrs
        })
    }
    ;

parse_crud_create_attr:
    | RAW                             { annotate $startpos $endpos (Ast_syntax.Create.Raw) }
    | SUFFIX suffix = annotated_ident { annotate $startpos $endpos (Ast_syntax.Create.Suffix suffix) }

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
    attrs = parse_bare_or_list(parse_crud_read_attr)
    { annotate $startpos $endpos (Ast_syntax.Crud.Read
        { Ast_syntax.Read.kind
        ; query = None
        ; attrs
        })
    }
    ;

parse_crud_read_query:
    READ
    kind = parse_crud_read_kind
    query = parse_crud_query
    attrs = parse_bare_or_list(parse_crud_read_attr)
    { annotate $startpos $endpos (Ast_syntax.Crud.Read
        { Ast_syntax.Read.kind
        ; query = Some query
        ; attrs
        })
    }
    ;

parse_crud_read_kind:
    | HAS     { annotate $startpos $endpos (Ast_syntax.Read.Has) }
    | FIRST   { annotate $startpos $endpos (Ast_syntax.Read.First) }
    | ONE     { annotate $startpos $endpos (Ast_syntax.Read.One) }
    | ALL     { annotate $startpos $endpos (Ast_syntax.Read.All) }
    | FIND    { annotate $startpos $endpos (Ast_syntax.Read.Find) }
    | LIMITED { annotate $startpos $endpos (Ast_syntax.Read.Limited) }
    | PAGED   { annotate $startpos $endpos (Ast_syntax.Read.Paged) }

parse_crud_read_attr:
    | orderby = parse_crud_read_attr_orderby { orderby }
    | suffix  = parse_crud_read_attr_suffix  { suffix }

parse_crud_read_attr_orderby:
    ORDERBY
    direction = parse_crud_read_attr_orderby_direction
    { annotate $startpos $endpos (Ast_syntax.Read.OrderBy direction) }
    ;

%inline
parse_crud_read_attr_orderby_direction:
    | ASC  { Ast_syntax.Read.Ascending }
    | DESC { Ast_syntax.Read.Descending }
    ;

parse_crud_read_attr_suffix:
    SUFFIX
    suffix = annotated_ident
    { annotate $startpos $endpos (Ast_syntax.Read.Suffix suffix) }
    ;

//
// crud update
//

parse_crud_update:
    UPDATE
    query = parse_crud_query
    attrs = parse_bare_or_list(parse_crud_update_attr)
    { annotate $startpos $endpos (Ast_syntax.Crud.Update
        { Ast_syntax.Update.query
        ; attrs
        })
    }

    ;

parse_crud_update_attr:
    | SUFFIX suffix = annotated_ident { annotate $startpos $endpos (Ast_syntax.Update.Suffix suffix) }
    ;

//
// crud delete
//

parse_crud_delete:
    DELETE
    query = parse_crud_query
    attrs = parse_bare_or_list(parse_crud_delete_attr)
    { annotate $startpos $endpos (Ast_syntax.Crud.Delete
        { Ast_syntax.Delete.query
        ; attrs
        })
    }
    ;

parse_crud_delete_attr:
    | SUFFIX suffix = annotated_ident { annotate $startpos $endpos (Ast_syntax.Delete.Suffix suffix) }
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
    left_val = parse_crud_query_value
    op = parse_crud_query_op
    right_val = parse_crud_query_value
    { annotate $startpos $endpos (Ast_syntax.Query.Term
        { Ast_syntax.Query.left_val
        ; op
        ; right_val
        })
    }
    ;

parse_crud_query_op:
    | NOT_EQUAL             { annotate $startpos $endpos (Ast_syntax.Query.NotEqual) }
    | LESS_THAN             { annotate $startpos $endpos (Ast_syntax.Query.LessThan) }
    | LESS_THAN_OR_EQUAL    { annotate $startpos $endpos (Ast_syntax.Query.LessThanOrEqual) }
    | GREATER_THAN          { annotate $startpos $endpos (Ast_syntax.Query.GreaterThan) }
    | GREATER_THAN_OR_EQUAL { annotate $startpos $endpos (Ast_syntax.Query.GreaterThanOrEqual) }
    | EQUAL                 { annotate $startpos $endpos (Ast_syntax.Query.Equal) }
    | IN                    { annotate $startpos $endpos (Ast_syntax.Query.In) }
    ;

parse_crud_query_value:
    | QUESTION                                { annotate $startpos $endpos (Ast_syntax.Query.Placeholder) }
    | number    = annotated_number            { annotate $startpos $endpos (Ast_syntax.Query.Literal number) }
    | string    = annotated_string            { annotate $startpos $endpos (Ast_syntax.Query.Literal string) }
    | DOT field = annotated_ident             { annotate $startpos $endpos (Ast_syntax.Query.Field field) }
    | call      = parse_crud_query_value_call { call }
    | join      = parse_crud_query_value_join { join }
    ;

parse_crud_query_value_call:
    func = annotated_ident
    LEFT_PAREN
    arg = parse_crud_query_value
    RIGHT_PAREN
    { annotate $startpos $endpos (Ast_syntax.Query.Call (func, arg)) }
    ;

parse_crud_query_value_join:
    model = annotated_ident
    LEFT_PAREN
    query = parse_crud_query
    RIGHT_PAREN
    DOT
    field = annotated_ident
    { annotate $startpos $endpos (Ast_syntax.Query.Join (model, query, field)) }
    ;

parse_crud_query_and:
    left_query = parse_crud_query
    AND
    right_query = parse_crud_query
    { annotate $startpos $endpos (Ast_syntax.Query.And
        { Ast_syntax.Query.left_query
        ; right_query
        })
    }
    ;

parse_crud_query_or:
    left_query = parse_crud_query
    OR
    right_query = parse_crud_query
    { annotate $startpos $endpos (Ast_syntax.Query.Or
        { Ast_syntax.Query.left_query
        ; right_query
        })
    }
    ;
