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
    let result = { contents = 1 } in
    for i = 2 to n do
      result := i * !result;
      (* result.contents <- i * result.contents; *)
    done;
    !result;;

let () =
    let value = 
      match int_of_string Sys.argv.(1) with
      | n -> n
      | exception Invalid_argument _ -> 1 in

    Printf.printf "%d, " (factorial value);
    Printf.printf "%d, " (factorial2 value);
    Printf.printf "%d " (factorial3 value)
