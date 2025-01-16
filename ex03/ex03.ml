(* 入力用のシグネチャ *)
module type FRACTIONNAL_BITS = sig
  val bits : int
end

(* 出力用のシグネチャ *)
module type FIXED = sig
  type t

  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool
  val eqs : t -> t -> bool
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type MAKE = functor (F : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
functor
  (F : FRACTIONNAL_BITS)
  ->
  struct
    type t = int

    (* 受け取ったxをtに丸める *)
    let of_float (x : float) : t =
      int_of_float (Float.round (x *. Float.pow 2.0 (float_of_int F.bits)))

    let of_int (x : int) : t = x lsl F.bits

    let to_float (x : t) : float =
      float_of_int x /. Float.pow 2.0 (float_of_int F.bits)

    let to_int (x : t) : int = x lsr F.bits
    let to_string (x : t) : string = string_of_float (to_float x)
    let zero : t = 0
    let one : t = of_int 1
    let succ (x : t) : t = x + 1
    let pred (x : t) : t = x - 1
    let min (n1 : t) (n2 : t) : t = if n1 < n2 then n1 else n2
    let max (n1 : t) (n2 : t) : t = if n1 > n2 then n1 else n2
    let gth (n1 : t) (n2 : t) : bool = n1 > n2
    let lth (n1 : t) (n2 : t) : bool = n1 < n2
    let gte (n1 : t) (n2 : t) : bool = n1 >= n2
    let lte (n1 : t) (n2 : t) : bool = n1 <= n2
    let eqp (n1 : t) (n2 : t) : bool = n1 == n2
    let eqs (n1 : t) (n2 : t) : bool = n1 = n2
    let add (n1 : t) (n2 : t) : t = n1 + n2
    let sub (n1 : t) (n2 : t) : t = n1 - n2
    let mul (n1 : t) (n2 : t) : t = of_float (to_float n1 *. to_float n2)
    let div (n1 : t) (n2 : t) : t = of_float (to_float n1 /. to_float n2)

    let foreach (n1 : t) (n2 : t) (f : t -> unit) : unit =
      let range (n1 : t) (n2 : t) : t list =
        let rec loop (acc : t list) (n : t) : t list =
          if n < n1 then acc else loop (n :: acc) (n - 1)
        in
        loop [] n2
      in
      let lst = range n1 n2 in
      List.iter f lst
  end

module Fixed4 : FIXED = Make (struct
  let bits = 4
end)

module Fixed8 : FIXED = Make (struct
  let bits = 8
end)

let () =
  print_endline "TEST <subject>";
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach Fixed4.zero Fixed4.one (fun f ->
      print_endline (Fixed4.to_string f));
  print_endline "----------------";

  print_endline "TEST <of_float>";
  print_endline (Fixed4.to_string (Fixed4.of_float 1.5));
  print_endline "----------------";
  print_endline "TEST <of_int>";
  print_endline (Fixed4.to_string (Fixed4.of_int 5));
  print_endline "----------------";
  print_endline "TEST <to_float>";
  print_endline (string_of_float (Fixed8.to_float (Fixed8.of_float 4.2)));
  print_endline "----------------";
  print_endline "TEST <to_int>";
  print_endline (string_of_int (Fixed8.to_int (Fixed8.of_int 4)));
  print_endline "----------------";
  print_endline "TEST <to_string>";
  print_endline (Fixed4.to_string (Fixed4.of_float 4.5));
  print_endline "----------------";
  print_endline "TEST <zero>";
  print_endline (Fixed8.to_string Fixed8.zero);
  print_endline "----------------";
  print_endline "TEST <one>";
  print_endline (Fixed8.to_string Fixed8.one);
  print_endline "----------------";
  print_endline "TEST <succ>";
  print_endline (Fixed8.to_string (Fixed8.succ Fixed8.zero));
  print_endline "----------------";
  print_endline "TEST <pred>";
  print_endline (Fixed8.to_string (Fixed8.pred (Fixed8.succ Fixed8.zero)));
  print_endline "----------------";
  print_endline "TEST <min>";
  print_endline
    (Fixed8.to_string (Fixed8.min (Fixed8.of_float 1.5) (Fixed8.of_float 1.6)));
  print_endline "----------------";
  print_endline "TEST <max>";
  print_endline
    (Fixed8.to_string (Fixed8.max (Fixed8.of_float 4.8) (Fixed8.of_float 4.7)));
  print_endline "----------------";
  print_endline "TEST <gth>";
  print_endline
    (string_of_bool (Fixed8.gth (Fixed8.of_float 4.8) (Fixed8.of_float 4.7)));
  print_endline
    (string_of_bool (Fixed8.gth (Fixed8.of_float 4.8) (Fixed8.of_float 4.8)));
  print_endline
    (string_of_bool (Fixed8.gth (Fixed8.of_float 4.8) (Fixed8.of_float 4.9)));
  print_endline "----------------";

  print_endline "TEST <lth>";
  print_endline
    (string_of_bool (Fixed8.lth (Fixed8.of_float 4.8) (Fixed8.of_float 4.7)));
  print_endline
    (string_of_bool (Fixed8.lth (Fixed8.of_float 4.8) (Fixed8.of_float 4.8)));
  print_endline
    (string_of_bool (Fixed8.lth (Fixed8.of_float 4.8) (Fixed8.of_float 4.9)));
  print_endline "----------------";

  print_endline "TEST <gte>";
  print_endline
    (string_of_bool (Fixed8.gte (Fixed8.of_float 4.8) (Fixed8.of_float 4.7)));
  print_endline
    (string_of_bool (Fixed8.gte (Fixed8.of_float 4.8) (Fixed8.of_float 4.8)));
  print_endline
    (string_of_bool (Fixed8.gte (Fixed8.of_float 4.8) (Fixed8.of_float 4.9)));
  print_endline "----------------";

  print_endline "TEST <lte>";
  print_endline
    (string_of_bool (Fixed8.lte (Fixed8.of_float 4.8) (Fixed8.of_float 4.7)));
  print_endline
    (string_of_bool (Fixed8.lte (Fixed8.of_float 4.8) (Fixed8.of_float 4.8)));
  print_endline
    (string_of_bool (Fixed8.lte (Fixed8.of_float 4.8) (Fixed8.of_float 4.9)));
  print_endline "----------------";

  print_endline "TEST <eqp>";
  let a = Fixed8.of_float 4.0 in
  let b = Fixed8.of_float 4.4 in
  print_endline (string_of_bool (Fixed8.eqp a a));
  print_endline (string_of_bool (Fixed8.eqp a b));
  print_endline "----------------";

  print_endline "TEST <eqs>";
  let a = Fixed8.of_float 4.0 in
  let b = Fixed8.of_float 4.0 in
  print_endline (string_of_bool (Fixed8.eqs a a));
  print_endline (string_of_bool (Fixed8.eqs a b));
  print_endline "----------------";

  print_endline "TEST <add>";
  let a = Fixed8.of_float 4.5 in
  let b = Fixed8.of_float 4.5 in
  print_endline (Fixed8.to_string (Fixed8.add a b));
  print_endline "----------------";

  print_endline "TEST <sub>";
  let a = Fixed8.of_float 4.5 in
  let b = Fixed8.of_float 3.5 in
  print_endline (Fixed8.to_string (Fixed8.sub a b));
  print_endline "----------------";

  print_endline "TEST <mul>";
  let a = Fixed8.of_float 4.5 in
  let b = Fixed8.of_float 2.0 in
  print_endline (Fixed8.to_string (Fixed8.mul a b));
  print_endline "----------------";

  print_endline "TEST <div>";
  let a = Fixed8.of_float 4.5 in
  let b = Fixed8.of_float 1.5 in
  print_endline (Fixed8.to_string (Fixed8.div a b));
  print_endline "----------------"
