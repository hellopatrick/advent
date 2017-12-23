%{
open Core_kernel
open Gate
%}

%token <int> NUMBER
%token <string> NAME
%token ARROW
%token AND
%token OR
%token LSHIFT
%token RSHIFT
%token NOT
%token EOF

%start <(string * Gate.t) list> gates
%%

gates:
| l = list(gate); EOF { l }

gate:
| v = value; ARROW; c = NAME { (c, v) }
| x = value; AND; y = value; ARROW; c = NAME { (c, And (x, y)) }
| x = value; OR; y = value; ARROW; c = NAME { (c, Or (x, y)) }
| x = value; RSHIFT; y = value; ARROW; c = NAME { (c, Right (x, y)) }
| x = value; LSHIFT; y = value; ARROW; c = NAME { (c, Left (x, y)) }
| NOT; x = value; ARROW; c = NAME; { (c, Not x) }

value:
| s = scalar    { Value s }
| r = register  { Value r }

scalar:
| s = NUMBER { Scalar (s) }

register:
| c = NAME { Register (c) }