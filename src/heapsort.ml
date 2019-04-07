let heapsort a =
 
  let swap i j =
    let t = a.(i) in a.(i) <- a.(j); a.(j) <- t in
 
  let sift k l =
    let rec check x y =
      if 2*x+1 < l then
        let ch =
          if y < l-1 && a.(y) < a.(y+1) then y+1 else y in
        if a.(x) < a.(ch) then (swap x ch; check ch (2*ch+1)) in
    check k (2*k+1) in
 
  let len = Array.length a in
 
  for start = (len/2)-1 downto 0 do
    sift start len;
  done;
 
  for term = len-1 downto 1 do
    swap term 0;
    sift 0 term;
  done;;

let () =
    let a = [|3;1;4;1;5;9;2;6;5;3;5;8;97;93;23;84;62;64;33;83;27;95|] in
    heapsort a;
    Array.iter (Printf.printf "%d ") a;
    print_newline ();
    print_newline ();
    
    let s = "Just to show this is a type-checked polymorphic function" in
    let b = Array.init (String.length s) (String.get s) in
    heapsort b;
    Array.iter print_char b;;
    print_newline ();