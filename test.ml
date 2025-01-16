(* let rec range n1 n2 = if n1 > n2 then [] else n1 :: range (n1 + 1) n2;; *)

let range n1 n2 =
  let rec loop acc n = if n < n1 then acc else loop (n :: acc) (n - 1) in
  loop [] n2
;;

List.iter (fun n -> print_endline (string_of_int n)) (range 1 10)

let physical_equal (n1 : int) (n2 : int) : bool = n1 == n2
