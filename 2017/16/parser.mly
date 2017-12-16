%{
open Core_kernel
open Dance
%}

%token <int> NUMBER
%token <char> LETTER
%token SLASH
%token COMMA
%token EOF

%start <Dance.t list> moves
%%

moves:
| i = separated_list(COMMA, move); EOF { i }

move:
| c = LETTER; n = NUMBER; { Spin n }
| c = LETTER; n = NUMBER; SLASH; j = NUMBER; { Exchange (n, j) }
| c = LETTER; a = LETTER; SLASH; b = LETTER; { Partner (a, b) }