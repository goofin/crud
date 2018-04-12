%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token DOT
%token QUESTION
%token <string> COMPARISON

%token <string> NUMBER
%token <string> IDENT
%token <string> QUOTED

%token EOF

%start <Syntax.tuple Syntax.value> prog
%%

prog:
    | elts = parse_tuple { elts }
    ;

parse_tuple:
    | entries = nonempty_list(parse_other) { Syntax.Tuple entries }
    ;

parse_other:
    | ident = IDENT                                    { Syntax.Ident ident }
    | LEFT_PAREN; elts = parse_inner_list; RIGHT_PAREN { Syntax.List elts }
    ;

parse_inner_list:
    | elts = sep_list(COMMA, parse_tuple) COMMA? { elts }
    ;

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
  xs = rev_sep_list(DELIM, X) { List.rev xs }
