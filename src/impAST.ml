type aexp =
  | Num of int
  | Var of string
  | Plus of aexp * aexp
  | Mult of aexp * aexp
  | Sub of aexp * aexp

type bexp =
  | True
  | False
  | Eq of aexp * aexp
  | Leq of aexp * aexp
  | Or of bexp * bexp
  | And of bexp * bexp

type com = 
  | Skip
  | Assign of string * aexp
  | Seq of com * com
  | Cond of bexp * com * com
  (* We add print as a command for ease of use *)
  (* The semantics are just evaluate aexp until we get a number *)
  (* Then print that number and return "Skip" *)
  | Printa of aexp
  | Printb of bexp

type prog = com