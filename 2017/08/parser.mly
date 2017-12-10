%{
open Core_kernel
open Instruction
%}

%token <int> NUMBER
%token <string> NAME
%token LT
%token GT
%token LTE
%token GTE
%token NE
%token EQ
%token IF
%token INC
%token DEC
%token EOF

%start <Instruction.t list> instructions
%%

instructions:
| i = list(instruction); EOF { i }

instruction:
| action = get_action; IF; condition = get_condition; {{ action; condition}}

get_action:
| n = NAME; INC; v = NUMBER; { Increment (n, v) }
| n = NAME; DEC; v = NUMBER; { Decrement (n, v) }

get_condition:
| n = NAME; LT; v = NUMBER;   { LessThan (n, v) }
| n = NAME; LTE; v = NUMBER;  { LessThanOrEqual (n, v) }
| n = NAME; GT; v = NUMBER;   { GreaterThan (n, v) }
| n = NAME; GTE; v = NUMBER;  { GreaterThanOrEqual (n, v) }
| n = NAME; EQ; v = NUMBER;   { Equal (n, v) }
| n = NAME; NE; v = NUMBER;   { NotEqual (n, v) }