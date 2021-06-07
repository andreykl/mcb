From mathcomp Require Import all_ssreflect.
From mathcomp Require Import all_algebra.
From mathcomp Require Import all_fingroup.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Open Scope ring_scope.

Lemma mxtrace_mulC (R : comRingType) m n (A : 'M[R]_(m, n)) B :
  (\tr ( A *m B)) = (\tr ( B *m A)).
Proof.
  gen have trE, trAB : m n A B / \tr (A *m B) = \sum_i \sum_j A i j * B j i.
    by apply: eq_bigr => i _; rewrite mxE.
  rewrite trAB trE.
  rewrite exchange_big /=.
    by do 2!apply: eq_bigr => ? _; apply: GRing.mulrC.
Qed.

Module Chapter782.
  Variables (R : Type) (n n1 n2 m m1 m2 : nat).
  
  Definition lsubmx (A : 'M[R]_(m, n1 + n2)) : 'M[R]_(m, n1). Admitted.
  Definition usubmx (A : 'M[R]_(m1 + m2, n)) : 'M[R]_(m1, n). Admitted.
  Definition ulsubmx (A : 'M[R]_(m1 + m2, n1 + n2)) : 'M[R]_(m1, n1). Admitted.

  Definition row_mx (A1 : 'M[R]_(m, n1)) (A2 : 'M[R]_(m, n2)) :
    'M[R]_(m, n1 + n2). Admitted.
  Definition col_mx (A1 : 'M[R]_(m1, n)) (A2 : 'M[R]_(m2, n)) :
    'M[R]_(m1 + m2, n). Admitted.

  Definition block_mx (Aul : 'M[R]_(m1, n1)) (Aur : 'M[R]_(m1, n2))
                      (Adl : 'M[R]_(m2, n1)) (Adr : 'M[R]_(m2, n2)) :
    'M[R]_(m1 + m2, n1 + n2). Admitted.
End Chapter782.

Module SizeCast.
  Variables (R : Type) (n n1 n2 n3 m m1 m2 m3 : nat).

  Fail Lemma row_mxA (A1 : 'M[R]_(m, n1))
                     (A2 : 'M[R]_(m, n2)) (A3 : 'M[R]_(m, n3)) :
    row_mx A1 (row_mx A2 A3) = row_mx (row_mx A1 A2) A3.

  Lemma row_mxA (A1 : 'M[R]_(m, n1)) (A2 : 'M[R]_(m, n2)) (A3 : 'M[R]_(m, n3)) :
    let cast := (erefl m, esym (addnA n1 n2 n3)) in 
    row_mx A1 (row_mx A2 A3) = castmx cast (row_mx (row_mx A1 A2) A3). Admitted.

  Lemma castmxKV (eq_m : m1 = m2) (eq_n : n1 = n2) :
    cancel (@castmx R _ _ _ _ (esym eq_m, esym eq_n)) (castmx (eq_m, eq_n)).
  Admitted.

  Lemma castmx_id m n erefl_mn (A : 'M[R]_(m, n)) : castmx erefl_mn A = A.
  Admitted.

  Definition conform_mx (B : 'M[R]_(m1, n1)) (A : 'M[R]_(m, n)) :=
    match m =P m1, n =P n1 with
    | ReflectT eq_m, ReflectT eq_n => castmx (eq_m, eq_n) A
    | _, _ => B
    end.
End SizeCast.

Lemma conform_mx_id R m n (B A : 'M[R]_(m, n)) : conform_mx B A = A. Admitted.
Lemma nonconform_mx R m1 n1 m n (B : 'M[R]_(m1, n1)) (A : 'M[R]_(m, n)) :
  (m != m1) || (n != n1) -> conform_mx B A = B. Admitted.    
