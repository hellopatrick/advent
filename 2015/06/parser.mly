%{
open Core_kernel
open Instruction
%}

%token <int> NUMBER
%token ON
%token OFF
%token TOGGLE
%token THROUGH
%token COMMA
%token EOF

%start <Instruction.t list> instructions
%%

instructions:
| l = list(instruction); EOF { l }

instruction:
| ON; top_left = point; THROUGH; bottom_right = point; { On ({top_left; bottom_right;})}
| OFF; top_left = point; THROUGH; bottom_right = point; { Off ({top_left; bottom_right;})}
| TOGGLE; top_left = point; THROUGH; bottom_right = point; { Toggle ({top_left; bottom_right;})}

point:
| a = NUMBER; COMMA; b = NUMBER { (a, b) }