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

let rec step_com (c : com) (s: sigma): com =
  match c with
  | Skip -> failwith "No evaluation possible for skip"
  | Printa a -> (match a with
    | Num n -> print_endline (string_of_int n); Skip
    | Var x -> print_endline (x); Skip
    | Plus (a1, a2) -> (match step_aexp a1 s , step_aexp a2 s with
      | n1, n2 -> print_endline(string_of_int n1 ^ " + " ^ string_of_int n2); Skip)
    | Mult (a1, a2) -> (match step_aexp a1 s , step_aexp a2 s with
      | n1, n2 -> print_endline(string_of_int n1 ^ " * " ^ string_of_int n2); Skip)
    | Sub (a1, a2) -> (match step_aexp a1 s , step_aexp a2 s with
      | n1, n2 -> print_endline(string_of_int n1 ^ " - " ^ string_of_int n2); Skip))
  | Printb b -> (match b with 
    | True -> print_string "True"; Skip
    | False -> print_string "False"; Skip
    | Eq (a1, a2) -> print_string "equals"; Skip
    | Leq (a1, a2) -> print_string "leq"; Skip
    | Or (b1, b2) -> print_string "or"; Skip
    | And (b1, b2) -> print_string "and"; Skip)

let rec eval_prog (p : prog) : unit =
  let s: sigma = Assoc.empty in 
  match p with
  | Skip -> ()
  | _ -> eval_prog (step_com p s)