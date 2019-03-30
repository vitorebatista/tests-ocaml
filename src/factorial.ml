(** Fatorial de um numero *)
let rec factorial numero : int =
    match numero with
    | 1 -> 1
    | n -> n * factorial (n - 1)

(*
OCaml offers updatable memory cells, called references:
ref init returns a new cell with initial contents init, 
!cell returns the current contents of cell, and 
cell := v writes the value v into cell.
*)
let factorial2 n =
    let result = ref 1 in
    for i = 2 to n do
      result := i * !result
    done;
    !result;;

let () =
    let f = factorial 3 in
    print_int f;
    print_newline ();

    let f = factorial2 5 in
    print_int f;
    print_newline ()