(*
内部にintのペア型の変数pairを持つ、シグネチャを作成
*)
module type PAIR = sig
  val pair : int * int
end

(*
内部にint型の変数xを持つ、シグネチャを作成
*)
module type VAL = sig
  val x : int
end

(* FIX ME !!! *)
(* まず、Fanctor自体のシグネチャを定義 *)
(* シグネチャの詳細について:
   入力モジュール -> PAIR
   出力モジュール -> VAL
*)
module type MAKEPROJECTION = functor (X : PAIR) -> VAL

(* MAKEPROJECTIOをもとにし、MakeFstを作成 シグネチャ通り、引数にPAIRを取り、出力はVALのため、int型のxを持つ、それにPAIRの最初の値をfstで代入する *)
module MakeFst : MAKEPROJECTION =
functor
  (X : PAIR)
  ->
  struct
    let x = fst X.pair
  end

(* MAKEPROJECTIOをもとにし、MakeFstを作成 シグネチャ通り、引数にPAIRを取り、出力ははVALのため、int型のxを持つ、それにPAIRの2番目の値をsndで代入する *)
module MakeSnd : MAKEPROJECTION =
functor
  (X : PAIR)
  ->
  struct
    let x = snd X.pair
  end

(*
PAIRシグネチャをもとにmodule pairを作成.
*)
module Pair : PAIR = struct
  let pair = (21, 42)
end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
