From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Module Chapter77.

  Variable (T : finType).

  Inductive perm_of : Type :=
    Perm (pval : {ffun T -> T}) & injective pval.
  Definition pval p := let: Perm f _ := p in f.
  Notation "{ 'perm' T }" := (perm_of T).

  (* Canonical perm_subType := Eval hnf in [subType for pval]. *)
  (* Definition perm_eqMixin := Eval hnf in [eqMixin of perm_type by <:]. *)
  (* Canonical perm_eqType := Eval hnf in EqType perm_type perm_eqMixin. *)
  (* Definition perm_finMixin := [finMixin of perm_type by <:]. *)
  (* Canonical perm_finType := Eval hnf in FinType perm_type perm_finMixin. *)

  Notation "''S_' n" := {perm 'I_n} (at level 200).

  (* Definition perm_on (S : {set T}) : pred {perm T} := *)
  (*   fun s => [set x | s x != x] \subset S. *)
  (* Lemma card_perm A : #|perm_on A| = #|A| `!. *)
End Chapter77.





