%{
open Core_kernel
%}

%token <int> NUMBER
%token P
%token A
%token V
%token EQUAL
%token LBRACKET
%token RBRACKET
%token COMMA
%token EOF

%start <Particle.t list> particles
%%

particles:
| i = list(particle); EOF { i }

particle:
| P; EQUAL; p = vector; COMMA; V; EQUAL; v = vector; COMMA; A; EQUAL; a = vector; { Particle.{p; a; v;} }

vector:
| LBRACKET; x = NUMBER; COMMA; y = NUMBER; COMMA; z = NUMBER; RBRACKET { Vector.{x; y; z;} }