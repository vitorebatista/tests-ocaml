let rec sigma f = function
    | [] -> 0
    | x :: l -> f x + sigma f l;;

let () =
    let f = sigma (fun x -> x * x) [1; 2; 3] in
        print_int f;
        print_newline ();