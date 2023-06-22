(*let rec x = (x; ());;

let rec x = "x";;

let rec x = let y = () in y;;

let rec x = let y = (x; ()) in y;;

let rec x = let y = () in x;;

let rec x = y 0 and y _ = ();;

let rec x = y and y = ();;

let rec x = let y = x in y;;

let rec x = A x;;

let rec r = (let rec x = A r and y = fun () -> x in y);;

let rec x = A x
and y = ignore x; 0;;

let rec x = [y]
and y = let x = () in x;;

let rec x = [y]
and y = let rec x = () in x;;

let rec x =
  let a = x in
  fun () -> a ()
and y = [x];;
*)

let rec x = Tree [(print_endline "effect"; y); z]
and y = Tree (print_endline "effect"; [])
and z = Tree (print_endline "effect"; [x])
in
match (x, y, z) with
  | (Tree [y1; z1], Tree[], Tree[x1]) ->
    (assert (y1 == y);
    assert (z1 == z);
    assert (x1 == x))
  | _ ->
    assert false
;;