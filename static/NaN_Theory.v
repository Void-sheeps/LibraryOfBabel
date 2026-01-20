(*
  NaN_Theory.v
  Postulado: a identidade não é garantida no silício.
*)

(* =============================== *)
(* 1. Domínio abstrato             *)
(* =============================== *)

Parameter Double : Type.

Parameter eqD : Double -> Double -> Prop.
Infix "==" := eqD (at level 70).

(* =============================== *)
(* 2. Lei esperada da identidade   *)
(* =============================== *)

Axiom Eq_reflexiva :
  forall x : Double, x == x.

(* =============================== *)
(* 3. Predicado NaN                *)
(* =============================== *)

Parameter isNaN : Double -> Prop.

(* =============================== *)
(* 4. Axioma IEEE-754              *)
(* =============================== *)

Axiom NaN_nao_reflexivo :
  forall x : Double, isNaN x -> ~(x == x).

(* =============================== *)
(* 5. Existência de NaN            *)
(* =============================== *)

Axiom existe_NaN :
  exists n : Double, isNaN n.

(* =============================== *)
(* 6. Teorema: contradição         *)
(* =============================== *)

Theorem contradicao_identidade :
  False.
Proof.
  destruct existe_NaN as [n Hnan].
  apply (NaN_nao_reflexivo n Hnan).
  apply Eq_reflexiva.
Qed.
