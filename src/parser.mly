
%{
open ImpAST
open Str

exception ParseException of string
%}

(* Tokens *)

%token EOF
%token <int> NUM
%token <string> ID
%token SEMICOLON
%token IF
%token ELSE 
%token PLUS
%token TIMES
%token SUB
%token EQUALS
%token LEQ
%token SKIP
%token TRUE
%token FALSE
%token AND 
%token OR 
%token PRINT

(* Precedences *)
%left PLUS SUB
%left TIMES
%left OR AND


(* After declaring associativity and precedence, we need to declare what
   the starting point is for parsing the language.  The following
   declaration says to start with a rule (defined below) named [prog].
   The declaration also says that parsing a [prog] will return an OCaml
   value of type [ImpAST.prog]. *)

%start main
%type <ImpAST.prog> main

(* The following %% ends the declarations section of the grammar definition. *)

%%

main:
  | c = com; EOF;
    { c }

com:
  | SKIP;
    { Skip }
  | PRINT; a = aexp;
    { Printa(a) }
  | PRINT; b = bexp;
    { Printb(b) }
  | x = ID; EQUALS; a = aexp;
    { Assign(x, a) }
  | c1 = com; SEMICOLON; c2 = com;
    { Seq(c1, c2) }
  | IF; b = bexp; c1 = com; ELSE; c2 = com;
    { Cond(b, c1, c2) }

bexp:
  | TRUE;
    { True }
  | FALSE;
    { False }
  | a1 = aexp; EQUALS; a2 = aexp;
    { Eq(a1, a2) }
  | a1 = aexp; LEQ; a2 = aexp;
    { Leq(a1, a2) }
  | b1 = bexp; OR; b2 = bexp;
    { Or(b1, b2) }
  | b1 = bexp; AND; b2 = bexp;
    { And(b1, b2) }

aexp:
  | n = NUM;
    { Num(n) }
  | x = ID;
    { Var(x) }
  | a1 = aexp; PLUS; a2 = aexp;
    { Plus(a1, a2) }
  | a1 = aexp; TIMES; a2 = aexp;
    {Mult(a1, a2) }
  | a1 = aexp; SUB; a2 = aexp;
    {Sub(a1, a2) }

%%