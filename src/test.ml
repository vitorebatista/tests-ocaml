
module List = Core.List

type language =
 | Haskell
 | OCaml
 | Javo
 | Other of { qual: string }
 [@@deriving show]

let diga_seu_nome linguagem =
    match linguagem with
    | Haskell -> "Haskell"
    | OCaml   -> "Ocaml"
    | Javo    -> "Javo"
    | Other { qual } -> Printf.sprintf "Other: %s" qual

(* let fff = 
    let fat = factorial ~numero:5 in
    let imprimir_fatorial = Printf.printf "fatorial de 5 = %d\n" in
    imprimir_fatorial fat
    
let fsdf =
    let lang = Other { qual = "coisa" } in
    print_endline @@ diga_seu_nome lang;
    print_endline @@ show_language lang *)

let rec range a b =
    if a > b then []
    else a :: range (a+1) b;;

let positive_sum a b = 
    let a = max a 0
    and b = max b 0 in
    a + b;;

let rec sort = function
    | [] -> []
    | x :: l -> insert x (sort l)
  and insert elem = function
    | [] -> [elem]
    | x :: l -> if elem < x then elem :: x :: l
                else x :: insert elem l;;

let add_polynom p1 p2 =
    let n1 = Array.length p1
    and n2 = Array.length p2 in
    let result = Array.make (max n1 n2) 0 in
    for i = 0 to n1 - 1 do result.(i) <- p1.(i) done;
    for i = 0 to n2 - 1 do result.(i) <- result.(i) + p2.(i) done;
    result;;

let reverse lista =
  let rec visit resultado pendente = 
    match pendente with
    | [] -> resultado
    | pivo :: cauda -> visit (pivo :: resultado) cauda
  in
  visit [] lista;;

let rec reverse' lista =
  match lista with
  | pivo :: cauda -> reverse cauda @ [pivo]
  | [] -> []

(* 
  [1;2;3]

  [] [1;2;3]
  1 :: [] -> [2;3] -> [1]

  2 [3] -> 2 :: [1] -> [2; 1]

  3 :: 2 :: 1 []
  -> [3;2;1] *)

let () =
    let items = ["A"; "B"; "C"; "BA"; "CA"] in
    items
    |> List.filter ~f:(Core.String.is_suffix ~suffix:"A")
    |> List.map ~f:String.lowercase_ascii
    |> List.iter ~f:print_endline;

    let rev = reverse [1; 2; 3 ] in
    rev
    |> List.iter ~f: print_int; print_newline ();

    let li = 1 :: 2 :: 3 :: [] in
    li
    |> List.iter ~f: print_int; print_newline ();


    let r = range 10 30 in 
    r
    |> List.iter ~f: print_int; print_newline ();
    
    let s = sort [3; 4; 2; 1] in 
    s
    |> List.iter ~f: print_int; print_newline ();

    let s2 = sort ["yes"; "ok"; "sure"; "zup"; "yep"] in
    print_newline ();
    s2
    |> List.iter ~f: print_string; print_newline ();
    
    (* TODO how print this array list? *)
    let ap = add_polynom [| 1; 2 |] [| 1; 2; 3 |] in 
    ();;

let olar () = "olar"