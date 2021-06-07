From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Module finSetDef.
  Variable T : finType.

  Inductive set_type : Type := FinSet of {ffun pred T}.
  Definition finfun_of_set A := let: FinSet f := A in f.

  Canonical set_subType := Eval hnf in [newType for finfun_of_set].
  Definition set_eqMixin := Eval hnf in [eqMixin of set_type by <:].
  Canonical set_eqType := Eval hnf in EqType set_type set_eqMixin.
  (* Definition set_finMixin := [finMixin of set_type by <:]. *)
  (* Canonical set_finType := Eval hnf in FinType set_type set_finMixin. *)
End finSetDef.

Notation "{ ’set’ T }" := (set_type T).  

Lemma setP (T : finType) (A B : {set T}): A =i B <-> A = B. Admitted.

Lemma example (T : finType) (x : T) (A : {set T}) :
  (A \subset x |: A) && (A :==: A :&: A) && (x \in [set y | y == x]). Admitted.

Lemma setCP (T : finType) x (A : {set T}) :
  reflect (~ x \in A) (x \in ~: A). Admitted.

Lemma subsets_disjoint (T : finType) (A B : {set T}) :
  (A \subset B) = [disjoint A & ~: B]. Admitted.

Definition powerset (T : finType) (D : {set T}) :
  {set {set T}} := [set A : {set T} | A \subset D].

Lemma card_powerset (T : finType) (A : {set T}) :
  #|powerset A| = 2 ^ #|A|. Admitted.
