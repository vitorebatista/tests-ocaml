(** Fatorial de um numero *)
let rec factorial numero : int =
    match numero with
    | 1 -> 1
    | n -> n * factorial (n - 1)

let () =
    let f = factorial 5 in
    print_int f;
    print_newline ()