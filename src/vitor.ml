
module List = Core.List

type language =
 | Haskell
 | OCaml
 | Javo
 | Outra of { qual: string }
 [@@deriving show]

let diga_seu_nome linguagem =
    match linguagem with
    | Haskell -> "Haskell"
    | OCaml   -> "Ocaml"
    | Javo    -> "Javo"
    | Outra { qual } -> Printf.sprintf "Outra: %s" qual

(** Fatorial de um numero *)
let rec factorial ~numero : int =
    match numero with
    | 1 -> 1
    | n -> n * factorial ~numero:(n - 1)

(* let fff = 
    let fat = factorial ~numero:5 in
    let imprimir_fatorial = Printf.printf "fatorial de 5 = %d\n" in
    imprimir_fatorial fat
    
let fsdf =
    let lang = Outra { qual = "coisa" } in
    print_endline @@ diga_seu_nome lang;
    print_endline @@ show_language lang *)


let () =
    let usuarios = ["olar"; "bom"; "dia"; "bommmm"] in
    usuarios
    |> List.map ~f:String.uppercase_ascii
    |> List.filter ~f:(Core.String.is_prefix ~prefix:"BOM")
    |> List.iter ~f:print_endline