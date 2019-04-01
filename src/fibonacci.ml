let rec fib n =
    match n with
    | (0 | 1) -> 1
    | x when x > 0 -> (fib (x-2) + fib (x-1))
    | _ -> raise (Invalid_argument "Negative value supplied to fib");;

let rec fib2 = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib2 (n-1) + fib2 (n-2)


let rec fibonacci =
  function
  | 0 | 1        -> 1
  | x when x > 0 -> computa x  
  | _            -> raise (Invalid_argument "Negative value supplied to fib")

and computa x =
  fibonacci (x - 2) + fibonacci (x - 1)


let execute () =
  match fibonacci (-1) with
  | value -> print_int value
  | exception Invalid_argument message -> print_endline message

let rec fib3 n =
  if n < 3 then
    1
  else
    fib3 (n-1) + fib3 (n-2)

let () =
  let repeat = 40 in

  Core.List.range 1 repeat
  |> Core.List.map ~f:fib
  |> Core.List.iter ~f:(Printf.printf "%d, ");

  (* for n = 1 to repeat do
    Printf.printf "%d, " (fib n)
  done; *)
  
  print_endline "...\n";
  
  for n = 1 to repeat do
    Printf.printf "%d, " (fib2 n)
  done;

  print_endline "...\n";
  
  for n = 1 to repeat do
    Printf.printf "%d, " (fib3 n)
  done;
  print_endline "...";

  execute ();