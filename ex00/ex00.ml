module StringSet = Set.Make (struct
  type t = string

  let compare = String.compare
end)

let () =
  let set =
    List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty
  in
  StringSet.iter print_endline set;
  print_endline (StringSet.fold ( ^ ) set "")
