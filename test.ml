let cat2 lst1 lst2 =
  let rec aux acc lst =
    match lst with [] -> acc | first :: rest -> aux (first :: acc) rest
  in
  aux lst2 (List.rev lst1)
  (* aux (aux [] (List.rev lst2)) (List.rev lst1) *)

let () =
  let lst = cat2 [ 0; 1; 2; 3; 4; 5 ] [ 6; 7; 8; 9 ] in
  List.iter (fun x -> print_endline (string_of_int x)) lst
