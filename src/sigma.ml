
let rec sigma ~f = 
    function
    | [] -> 0
    | hd :: tl -> f hd + sigma ~f tl

let tail_sigma ~f lista =
    let rec loop lista resultado =
        match lista with
        | [] -> resultado
        | hd :: tl -> loop tl (f hd + resultado)
    in
    loop lista 0

let () =
    let f = tail_sigma ~f:(fun x -> x * x) [1; 2; 3] in
    print_int f;
    print_newline ();

    Sys.argv
    |> Core.Array.iter ~f:print_endline
