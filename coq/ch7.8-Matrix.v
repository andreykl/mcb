From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.


Module Chapter78.
  Inductive matrix R m n : Type := Matrix of {ffun 'I_m * 'I_n -> R}.
  Definition mx_val R m n (A : @matrix R m n) := let: Matrix g := A in g.

  Notation "''M[' R ]_ ( m , n )" := (matrix R m n).
  Notation "''M_' ( m , n )" := (matrix _ m n).
  Notation "''M[' R ]_ n" := (matrix R n n) (at level 2).

  (* Definition mxtrace R n (A : @matrix R n n) : @matrix R n n := *)
  (*   \sum_i A i i. *)
  (* Local Notation "'\tr' A" := (mxtrace R n A). *)

  Definition matrix_of_fun R m n F : @matrix R m n :=
    Matrix [ffun ij : 'I_m * 'I_n => F ij.1 ij.2].
  Notation "\matrix_ ( i < m , j < n ) E" :=
    (matrix_of_fun (fun (i : 'I_m) (j : 'I_n) => E)) (at level 3).
  Notation "\matrix_ ( i , j ) E" := (matrix_of_fun (fun i j => E)) (at level 4).
End Chapter78.

(* Example diagonal R m n : @matrix R m n := *)
(*   \matrix_( i < 3 , j < 7) if i == j then 1 else 0. *)
(* Definition determinant n R (A : 'M_n) : R := *)
(*   \sum_(s : 'S_n) (-1) ^+ s * \prod_i A i (s i). *)

(* Definition mulmx R m n p (A : 'M_(m, n)) (B : 'M_(n, p)) : 'M[R]_(m, p) := *)
(*   \matrix_(i, k) \sum_j (A i j * B j k). *)
(* Notation "A *m B" := (mulmx A B) : ring_scope. *)
