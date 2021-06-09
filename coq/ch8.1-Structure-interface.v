From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Module Chapter811.
  Module OrdDef.
    Inductive ordinal n := Ordinal m of m < n.
    Notation "''I_' n" := (ordinal n).
    Coercion nat_of_ord n (i : 'I_n) := let: @Ordinal _ m _ := i in m.
  End OrdDef.

  Variable p' : nat.
  Local Notation p := p'.+1.
  Implicit Types x y : 'I_p.
  Definition inZp i := Ordinal (ltn_pmod i (ltn0Sn p')).

  Definition Zp0 : 'I_p := ord0.
  Definition Zp1 := inZp 1.
  Definition Zp_opp x := inZp (p - x).
  Definition Zp_add x y := inZp (x + y).
  Definition Zp_mul x y := inZp (x * y).

  Lemma Zp_add0z : left_id Zp0 Zp_add. Admitted.
  Lemma Zp_mulC : commutative Zp_mul. Admitted.

  (* Definition Zp_zmodMixin := ZmodMixin Zp_addA Zp_addC Zp_add0z Zp_addNz. *)
  (* Canonical Zp_zmodType := ZmodType 'I_p Zp_zmodMixin. *)
End Chapter811.

Module Chapter812.
  Variable p' : nat.
  Local Notation p := p'.+2.
End Chapter812.
