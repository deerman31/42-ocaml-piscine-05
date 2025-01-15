module StringHashtbl = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash = String.hash
end)

