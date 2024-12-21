module IntSet = Set.Make(struct
  type t = int
  let compare = Int.compare
end)

let () =
  let set = IntSet.empty in

  (* 要素を追加 *)
  let set = IntSet.add 1 set in
  let set = IntSet.add 2 set in
  let set = IntSet.add 3 set in
  
  (* 要素の確認 *)
  Printf.printf "Contains 2? %b\n" (IntSet.mem 2 set);
  
  (* 全要素を表示 *)
  IntSet.iter (Printf.printf "%d ") set;
  print_newline ()