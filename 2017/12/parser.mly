%{
open Core_kernel
open Pipe
%}

%token <int> NUMBER
%token ARROW
%token COMMA
%token EOF

%start <Pipe.t list> pipes
%%

pipes:
| i = list(pipe); EOF { i }

pipe:
| n = NUMBER; ARROW; links = get_links; { {n; links} }

get_links:
| l = separated_list(COMMA, link) { l }

link:
| n = NUMBER; { n }