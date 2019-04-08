let heapsort a =

  let swap i j =
    let t = a.(i) in 
      a.(i) <- a.(j);
      a.(j) <- t in
 
  let sift k l =
    let rec check left right =
      if 2 * left + 1 < l then
        let ch =
          match right < l - 1 && a.(right) < a.(right+1) with
          | true  -> right + 1
          | false -> right in
        match  a.(left) < a.(ch) with
        | true  ->
          swap left ch;
          check ch (2 * ch + 1)
        | false -> () in
    check k (2 * k + 1) in
 
  let len = Array.length a in
    Core.List.range ~stride:(-1) ~stop:`inclusive (len / 2 - 1) 0
    |> Core.List.iter ~f:((Core.Fn.flip sift) len);
  (* This is the same as here... *)
  (* for start = (len / 2) - 1 downto 0 do
    sift start len;
  done; *)
  
  for term = len - 1 downto 1 do
    swap term 0;
    sift 0 term;
  done

let () =
    let a = [|11;1;4;7;5;9;2;6;5;3;5;8;97;93;23;84;62;64;33;83;27;95|] in
    heapsort a;
    Array.iter (Printf.printf "%d ") a;
    print_newline ();
    print_newline ();
    
    let s = "456 Vitor Emanuel Batista 321" in
    let b = Array.init (String.length s) (String.get s) in
    heapsort b;
    Array.iter print_char b;
    print_newline ();