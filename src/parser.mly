
%{
open ImpAST
open Str

exception ParseException of string
%}

(* Tokens *)

%token EOF
%token <int> NUM
%token <string> ID
%token SKIP
%token TRUE
%token FALSE
%token PRINT
%token MULT
%token PLUS
%token MINUS
%token EQUAL

(* Precedences *)
%left PLUS MINUS
%left MULT


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
    { APrint(a) }
  | PRINT; b = bexp;
    { BPrint(b) }

bexp:
  | TRUE;
    { True }
  | FALSE;
    { False }
  | n1 = aexp; EQUAL; n2 = aexp;
   { Equal (n1, n2) }

aexp:
  | n = NUM;
    { Num(n) }
  | n1 = aexp; PLUS; n2 = aexp;
     { Plus (n1, n2) }
  | n1 = aexp; MINUS; n2 = aexp;
     { Minus (n1, n2) }
  | n1 = aexp; MULT; n2 = aexp;
     { Mult (n1, n2) }

%%
