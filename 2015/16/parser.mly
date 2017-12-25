%{
open Core_kernel
open Aunt
%}

%token <int> NUMBER
%token <string> PROPERTY
%token SUE
%token COLON
%token COMMA
%token EOF

%start <Aunt.t list> aunts
%%

aunts:
| l = list(aunt); EOF { l }

aunt:
| SUE; n = NUMBER; COLON; p = separated_list(COMMA, property) { Aunt.of_assoc p }

property:
| p = PROPERTY; COLON; n = NUMBER; { (p, n) }