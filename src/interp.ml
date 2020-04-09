(* Small-step interpreter for imp *)
(* Big-step interpreter setup left as an exercise *)
(* Note that I'm being extremely pedantic in this implementation *)
(* This is intentional to get started, but you can make it less annoying if you want *)

open ImpAST

type sigma = int Assoc.context

let get_val (a: aexp) (s: sigma): int = 
  match a with 
  | Num n -> n
  | Var x -> Assoc.lookup x s
  |_ -> 0

let rec step_aexp (a : aexp) (s : sigma): int =
  match a with
  | Num n -> n
  | Var x ->  (Assoc.lookup x s)
  | Plus (a1, a2) -> ((step_aexp a1 s)  + (step_aexp a2 s))
  | Mult (a1, a2) -> ((step_aexp a1 s)  * (step_aexp a2 s))
  | Sub (a1, a2) -> ((step_aexp a1 s)  - (step_aexp a2 s))
    (* (match a1,a2 with 
    | (Num n1, Num n2) -> Num (n1 + n2)
    | (Num n1, a2) -> Num (n1 + (get_val (step_aexp a2 s) s))
    | a1, _ -> Num (get_val (step_aexp a1 s) s)) *)
  (* | Mult (a1, a2) -> 
    (match a1,a2 with 
    | (Num n1, Num n2) -> Num (n1 * n2)
    | (Num n1, a2) -> Num (n1 * (get_val (step_aexp a2 s) s))
    | a1, _ -> Num (get_val (step_aexp a1 s) s))
  | Sub (a1, a2) -> 
    (match a1, a2 with 
    | (Num n1, Num n2) -> Num (n1 - n2)
    | (Num n1, a2) -> Num (n1 - (get_val (step_aexp a2 s) s))
    | a1, _ -> Num (get_val (step_aexp a1 s) s)) *)

let rec step_bexp (b : bexp) (s: sigma): bool =
  match b with
  | True -> failwith "No evaluation possible for true"
  | False -> failwith "No evaluation possible for false"
  | Eq (a1, a2) -> (match step_aexp a1 s, step_aexp a2 s with
    | n1, n2 -> if n1 = n2 then true else false)
  | Leq (a1, a2) -> (match step_aexp a1 s, step_aexp a2 s with
    | n1, n2 -> if n1 <= n2 then true else false)
  | Or (b1, b2) -> (match (step_bexp b1 s, step_bexp b2 s) with 
    |true, _ -> true
    |_, true -> true
    | _ -> false)
  | And (b1, b2) -> (match (step_bexp b1 s, step_bexp b2 s) with 
    |true, true -> true
    | _ -> false)

let rec step_com (c : com) (s: sigma): sigma =
  match c with
  | Skip -> failwith "No evaluation possible for skip"
  | Assign (x,a) -> Assoc.update x (step_aexp a s) s
  | Seq (c1, c2) -> step_com c2 (step_com c1 s)
  | Cond (b, c1, c2) -> if (step_bexp b s) then (step_com c1 s) else (step_com c2 s)
  | Printa a -> print_string (string_of_int (step_aexp a s)); s
  | Printb b -> print_string (string_of_bool (step_bexp b s)); s

let rec eval_prog (p: prog) : unit =
  let s: sigma = Assoc.empty in 
  ignore (step_com p s) 