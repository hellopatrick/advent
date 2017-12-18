%{
open Core_kernel
open Instruction
%}

%token <int> NUMBER
%token <char> LETTER
%token SEND
%token SET
%token ADD
%token MULTIPLY
%token MODULUS
%token RECEIVE
%token JUMP
%token EOF

%start <Instruction.t list> instructions
%%

instructions:
| i = list(instruction); EOF { i }

instruction:
| SEND; l = LETTER { Send (l) }
| SET; l = LETTER; v = value { Set (l, v) }
| ADD; l = LETTER; v = value { Add (l, v) }
| MULTIPLY; l = LETTER; v = value { Multiply (l, v) }
| MODULUS; l = LETTER; v = value { Modulus (l, v) }
| RECEIVE; l = LETTER; { Receive (l) }
| JUMP; l = value; v = value { Jump (l, v) }

value:
| l = LETTER { Register (l) }
| n = NUMBER { Value (n) }