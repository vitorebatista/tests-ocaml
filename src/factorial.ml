(** Fatorial de um numero *)
let rec factorial x : int =
    match x with
    | 1 -> 1
    | n -> n * factorial (n - 1)

let rec factorial2 x =
    if x <= 1 then 1 else x * factorial2 (x - 1);;

(*
OCaml offers updatable memory cells, called references:
ref init returns a new cell with initial contents init, 
!cell returns the current contents of cell, and 
cell := v writes the value v into cell.
*)
let factorial3 n =
    let result = ref 1 in
    for i = 2 to n do
      result := i * !result
    done;
    !result;;


let () =
    let value = 5 in

    let f = factorial value in
    print_int f;
    print_newline ();

    let f = factorial2 value in
    print_int f;
    print_newline ();

    let f = factorial3 value in
    print_int f;
    print_newline ();