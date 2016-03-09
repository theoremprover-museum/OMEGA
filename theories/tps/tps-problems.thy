;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :omega)

;;; cebrown 7/8/01, added some thms from the TPS library

; Example wffs from the TPS library
(th~defproblem CS-DUC-RELNS
 (in tps)
 (conclusion CS-DUC-RELNS-conc
  (all-types c b (forall (lam (P (b b b)) (forall (lam (|0| b) (CLOS-SYS2 (lam (R (o b c)) (forall (lam (|c| c) (S-DUC |0| P (R |c|))))))))))))
 (help "   Given a pairing algebra B (with zero element 0(B) and pairing operation P(BBB)),
we can define a notion of join and a notion of
inclusion.  A subset of the pairing algebra is a DUC-set (downward union closed)
if it is downward closed with respect to inclusion, and closed with respect to
joins.  A relation R between any set C and the pairing algebra is DUC-valued if
for any c in C, {y | R(c,y)} is a DUC-set.  The theorem CS-DUC-RELNS states that
the DUC-valued relations form a closure system, i.e., 
an arbitrary intersection of DUC-valued relations is a DUC-valued relation.

CS-DUC-RELNS: CLOS-SYS2(O(O(OBC))).LAMBDA R(OBC) FORALL c(C) S-DUC(O(OB)(BBB)B) 0(B) P(BBB).R c

   To prove the theorem, the definitions of join and inclusion are irrelevant
(they do not need to be instantiated).  Even without instantiating these definitions,
the search space is quite large, as many literals have flexible heads, allowing
them to unify with any other literal.  TPS finds a proof after about 6 hrs."))

(th~defproblem THM574
 (in tps)
 (conclusion THM574-conc
  (all-types a b (forall (lam (|h| (b (o a))) (implies (INJECTIVE |h|) (exists (lam (|g| (o a b)) (SURJECTIVE |g|))))))))
 (help "Challenge from Dana Scott stemming from X5309,
injective Cantor Theorem, Andrews' Challenge at CADE17

If h(B(OA)) is an injection, then there is a surjection g(OAB).
This is a generalization of THM573, and more meaningful since
there actually can be an injection h(B(OA)), but
not an injection h(I(OI))."))

(th~defproblem THM563
 (in tps)
 (conclusion THM563-conc
  (all-types b (forall (lam (JOIN (o b b b)) (forall (lam (LESS (o b b)) (forall (lam (|0| b) (CLOS-SYS1 (lam (W (o b)) (and (and (W |0|) (forall (lam (|x| b) (forall (lam (|y| b) (implies (and (W |y|) (LESS |x| |y|)) (W |x|))))))) (forall (lam (|x| b) (forall (lam (|y| b) (forall (lam (|z| b) (implies (and (and (W |x|) (W |y|)) (JOIN |x| |y| |z|)) (W |z|)))))))))))))))))))
 (help "A closure system is a set of sets closed under arbitrary intersections.
Let $0$ be any element, $LESS$ be any binary relation and $JOIN$ be any tertiary
relation.  The next theorem states that the set of sets containing $0$,
downward closed with respect to $LESS$ and closed with respect to $JOIN$
is a closure system.

This theorem is related to CS-DUC-RELNS -- a generalization in a sense,
except that this theorem deals with sets rather than relations."))

(th~defproblem THM145
 (in tps)
 (conclusion THM145-conc
  (all-types a (forall (lam (R (o a a)) (forall (lam (U (a (o a))) (implies (and (TRANSITIVE R) (forall (lam (|s| (o a)) (and (forall (lam (|z| a) (implies (|s| |z|) (R |z| (U |s|))))) (forall (lam (|j| a) (implies (forall (lam (|k| a) (implies (|s| |k|) (R |k| |j|)))) (R (U |s|) |j|)))))))) (forall (lam (|f| (a a)) (implies (forall (lam (|x| a) (forall (lam (|y| a) (implies (R |x| |y|) (R (|f| |x|) (|f| |y|))))))) (exists (lam (|w| a) (and (R |w| (|f| |w|)) (R (|f| |w|) |w|))))))))))))))
 (help "Tarski's (actually Knaster's) Fixed Point Theorem for Lattices;
formulation due to Coquand;
abstract version of THM2;
In a complete lattice, every monotone function has a fixed point;
U s is the least upper bound of the set S;
R is a reflexive ordering, but we seem not to need reflexivity.
find the fixed point [LAMBDA x(A). R(OAA) x .f x]

Renamed THM145 (was T145) after proof found."))

(th~defproblem THM303
 (in tps)
 (conclusion THM303-conc
 (forall (lam (NUMBER (o i)) (forall (lam (\|0\| i) (forall (lam (ODD (o i)) (forall (lam (S (i i)) (forall (lam (|0| i) (forall (lam (EVEN (o i)) (implies (and (and (and (and (EVEN |0|) (forall (lam (|n| i) (implies (EVEN |n|) (EVEN (S (S |n|))))))) (and (ODD (S |0|)) (forall (lam (|n| i) (implies (ODD |n|) (ODD (S (S |n|)))))))) (IND-TPS \|0\| S)) (forall (lam (|n| i) (equiv (NUMBER |n|) (or (EVEN |n|) (ODD |n|)))))) (forall (lam (|n| i) (NUMBER |n|)))))))))))))))))
 (help "variation of thm302"))

(th~defproblem THM542
 (in tps)
 (conclusion THM542-conc
  (all-types a (forall (lam (R (o a a)) (implies (WELL-ORD R) (REFLEXIVE R))))))
 (help "A well-ordering is reflexive"))

(th~defproblem THM146
 (in tps)
 (conclusion THM146-conc
 (forall (lam (|r| (o i i)) (forall (lam (|x| i) (forall (lam (|y| i) (equiv (forall (lam (|p| (o i i)) (implies (and (SUBRELATION |r| |p|) (TCLOSED |p| |r|)) (|p| |x| |y|)))) (forall (lam (|p| (o i i)) (implies (and (SUBRELATION |r| |p|) (TRANSITIVE |p|)) (|p| |x| |y|))))))))))))
 (help "Equivalence of two definitions of transitive closure.
Theorem suggested by Dana Scott."))

(th~defproblem THM57
 (in tps)
 (conclusion THM57-conc
 (forall (lam (P (o i)) (forall (lam (Q (o i)) (equiv (equiv (exists (lam (X i) (forall (lam (Y i) (equiv (P X) (P Y)))))) (equiv (exists (lam (X i) (Q X))) (forall (lam (Y i) (P Y))))) (equiv (exists (lam (X i) (forall (lam (Y i) (equiv (Q X) (Q Y)))))) (equiv (exists (lam (X i) (P X))) (forall (lam (Y i) (Q Y)))))))))))
 (help "CHALLENGE NO. 1 FOR FIFTH CONFERENCE ON AUTOMATED DEDUCTION  
128 CLAUSES  "))

(th~defproblem DISTRIB-THM
 (in tps)
 (conclusion DISTRIB-THM-conc
  (all-types a (forall (lam (JOIN (a a a)) (forall (lam (MEET (a a a)) (implies (LATTICE JOIN MEET) (equiv (DISTRIBUTIVE JOIN MEET) (DISTRIBUTIVE MEET JOIN)))))))))
 (help "In a lattice, join distributes over meet iff meet distributes over join."))

(th~defproblem MODULAR-EQUIV-THM
 (in tps)
 (conclusion MODULAR-EQUIV-THM-conc
  (all-types a (forall (lam (JOIN (a a a)) (forall (lam (MEET (a a a)) (implies (LATTICE JOIN MEET) (equiv (MODULAR MEET JOIN) (MODULAR-DEF2 MEET JOIN)))))))))
 (help "The equivalence of two definitions of modularity.
Due to Jordan."))

(th~defproblem PENTAGON-THM2B
 (in tps)
 (conclusion PENTAGON-THM2B-conc
  (all-types a (forall (lam (JOIN (a a a)) (forall (lam (MEET (a a a)) (implies (LATTICE JOIN MEET) (implies (PENTAGON MEET JOIN) (not (MODULAR-DEF2 MEET JOIN))))))))))
 (help "The easy direction of MODULAR-THM2-DEF2"))

(th~defproblem THM583
 (in tps)
 (conclusion THM583-conc
 (forall (lam (\|0\| i) (forall (lam (S (i i)) (forall (lam (|0| i) (forall (lam (PLUS-LIBCONST (i i i)) (implies (and (and (and (IND-TPS \|0\| S) (PLUS-INDEQS-TPS PLUS-LIBCONST |0| S)) (forall (lam (|m| i) (= (PLUS-LIBCONST |0| |m|) |m|)))) (forall (lam (|n| i) (forall (lam (|m| i) (= (PLUS-LIBCONST (S |n|) |m|) (S (PLUS-LIBCONST |n| |m|)))))))) (forall (lam (|n| i) (forall (lam (|m| i) (= (PLUS-LIBCONST |n| |m|) (PLUS-LIBCONST |m| |n|))))))))))))))))
 (help "Commutativity of + on N, given IND at type I, rec eqns for +, and
two lemmas 0 + m = m and [S n] + m = S [n + m] (see THM581 and THM582)."))

(th~defproblem 3-DIAMOND-THM
 (in tps)
 (conclusion 3-DIAMOND-THM-conc
  (all-types a (forall (lam (JOIN (a a a)) (forall (lam (MEET (a a a)) (implies (LATTICE JOIN MEET) (implies (3-DIAMOND MEET JOIN) (not (DISTRIBUTIVE MEET JOIN))))))))))
 (help "Analogous to PENTAGON-THM2B"))

(th~defproblem CD-LATTICE-THM
 (in tps)
 (conclusion CD-LATTICE-THM-conc
  (all-types a (forall (lam (JOIN (a a a)) (forall (lam (MEET (a a a)) (forall (lam (TOP a) (forall (lam (BOTTOM a) (implies (CD-LATTICE JOIN MEET TOP BOTTOM) (forall (lam (|x| a) (forall (lam (|y| a) (forall (lam (|z| a) (implies (and (LATTICE-COMP JOIN MEET TOP BOTTOM |x| |y|) (LATTICE-COMP JOIN MEET TOP BOTTOM |x| |z|)) (= |y| |z|)))))))))))))))))))
 (help "A complemented distributive lattice has unique complements"))

(th~defproblem THM152
 (in tps)
 (conclusion THM152-conc
 (forall (lam (|r| (o i i)) (forall (lam (|x| i) (forall (lam (|y| i) (equiv (forall (lam (|q| (o i)) (implies (and (forall (lam (|w| i) (implies (|r| |x| |w|) (|q| |w|)))) (forall (lam (|v| i) (forall (lam (|w| i) (implies (and (|q| |v|) (|r| |v| |w|)) (|q| |w|))))))) (|q| |y|)))) (forall (lam (|p| (o i i)) (implies (and (SUBRELATION |r| |p|) (TRANSITIVE |p|)) (|p| |x| |y|))))))))))))
 (help "Equivalence of two definitions of transitive closure.
Modified version of T146 (THM146). Is this a theorem?
Theorem suggested by Dana Scott."))

(th~defproblem NBHD-THM2
 (in tps)
 (conclusion NBHD-THM2-conc
  (all-types a (forall (lam (T (o (o a))) (implies (forall (lam (K (o (o a))) (forall (lam (R (o a)) (implies (and (SUBSET K T) (= R (SETUNION K))) (T R)))))) (forall (lam (S (o a)) (equiv (T S) (forall (lam (|x| a) (implies (S |x|) (exists (lam (R (o a)) (and (NBHD T R |x|) (SUBSET R S)))))))))))))))
 (help "A set is open iff it contains a neighbourhood of each of its points.
Uses just the required axiom from TOPOLOGY.
Needs primsub lambda q(oa) . q SUBSET S AND T q for K(o(oa)), plus SKOLEM-DEFAULT NIL.
Also completes proof with lambda q(oa) . K1 q AND . k2 q OR k3 q"))

(th~defproblem THM587
 (in tps)
 (conclusion THM587-conc
 (forall (lam (\|0\| i) (forall (lam (S (i i)) (forall (lam (|0| i) (forall (lam (PLUS-LIBCONST (i i i)) (implies (and (IND-TPS \|0\| S) (PLUS-INDEQS-TPS PLUS-LIBCONST |0| S)) (forall (lam (|x| i) (forall (lam (|y| i) (= (PLUS-LIBCONST (PLUS-LIBCONST |x| |y|) |y|) (PLUS-LIBCONST |x| (PLUS-LIBCONST |y| |y|)))))))))))))))))
 (help "A special case of associativity of addition (THM584).  [x + y] + y = x + [y + y].
Without a lemma giving Sx + y = S[x + y], we can't do the induction on x, and induction on y fails too.
The solution is to generalize this to associativity.  In terms of unification (PR00), the setvar P
should go to lambda z . [x + y] + z = x + [y + z].  So, this is an example of how to use unification to
generalize the problem to be shown by induction.

This was suggested by Michael Kohlhase, Nov 2000."))

(th~defproblem THM569
 (in tps)
 (conclusion THM569-conc
  (all-types b (forall (lam (LESSEQ (o b b)) (forall (lam (S (o (o b))) (implies (and (and (SUBSET S (DEDEKIND-CUT LESSEQ)) (exists (lam (|x| (o b)) (S |x|)))) (exists (lam (|c| (o b)) (and (DEDEKIND-CUT LESSEQ |c|) (forall (lam (|x| (o b)) (implies (S |x|) (SUBSET |c| |x|)))))))) (GLB (DEDEKIND-CUT LESSEQ) (lam (|x| (o b)) (lam (|y| (o b)) (SUBSET |x| |y|))) S (SETINTERSECT S)))))))))
 (help "Similar to THM568, but also proves SETINTERSECT S is glb of S (wrt SUBSET).

HOWEVER(!!!):  I accidentally defined DEDEKIND-CUT wrong.  See DDEDEKIND-CUT
and THM571 for the corrected thm."))

(th~defproblem EQUIV-02-03
 (in tps)
 (conclusion EQUIV-02-03-conc
  (all-types g (forall (lam (|f| (g g g)) (forall (lam (|e| g) (equiv (GROUP2 |f| |e|) (GROUP3 |f| |e|))))))))
 (help ""))

(th~defproblem THM130-NAT
 (in tps)
 (conclusion THM130-NAT-conc
 (forall (lam (|r| (o (o (o i)) (o (o i)))) (implies (and (and INDUCTION (|r| ZERO ZERO)) (forall (lam (|x| (o (o i))) (forall (lam (|y| (o (o i))) (implies (|r| |x| |y|) (|r| (SUCC |x|) (SUCC |y|)))))))) (forall (lam (|x| (o (o i))) (implies (NAT |x|) (exists (lam (|y| (o (o i))) (|r| |x| |y|))))))))))
 (help "thm130, rewritten for natural nos"))

(th~defproblem THM15B-V2
 (in tps)
 (conclusion THM15B-V2-conc
 (forall (lam (|f| (i i)) (implies (exists (lam (|g| (i i)) (and (ITERATE+ |f| |g|) (exists (lam (|x| i) (UNIQUE-FIXPOINT |g| |x|)))))) (exists (lam (|y| i) (FIXPOINT |f| |y|)))))))
 (help ""))

(th~defproblem THM135
 (in tps)
 (conclusion THM135-conc
  (all-types a (forall (lam (|f| (a a)) (forall (lam (|g1| (a a)) (forall (lam (|g2| (a a)) (implies (and (ITERATE |f| |g1|) (ITERATE |f| |g2|)) (ITERATE |f| (COMPOSE |g1| |g2|)))))))))))
 (help "The composition of iterates of a function is also an iterate of that function. Not provable without eta-conversion"))

(th~defproblem THM260
 (in tps)
 (conclusion THM260-conc
  (all-types a (forall (lam (R (o a a)) (implies (EQUIVALENCE-REL R) (PARTITION (EQUIVALENCE-CLASSES R)))))))
 (help "An equivalence relation defines a partition"))

(th~defproblem THM260-B
 (in tps)
 (conclusion THM260-B-conc
  (all-types a (forall (lam (R (o a a)) (implies (EQUIVALENCE-REL R) (PARTITION-B (EQUIVALENCE-CLASSES-B R)))))))
 (help "An equivalence relation defines a partition"))

(th~defproblem THM126-EXPANDED
 (in tps)
 (conclusion THM126-EXPANDED-conc
  (all-types a g b (forall (lam (|h1| (b g)) (forall (lam (|h2| (a b)) (forall (lam (|s1| (o g)) (forall (lam (|f1| (g g g)) (forall (lam (|s2| (o b)) (forall (lam (|f2| (b b b)) (forall (lam (|s3| (o a)) (forall (lam (|f3| (a a a)) (implies (and (and (and (and (forall (lam (|x| g) (forall (lam (|y| g) (implies (and (|s1| |x|) (|s1| |y|)) (|s1| (|f1| |x| |y|))))))) (forall (lam (|x| b) (forall (lam (|y| b) (implies (and (|s2| |x|) (|s2| |y|)) (|s2| (|f2| |x| |y|)))))))) (forall (lam (|x| g) (implies (|s1| |x|) (|s2| (|h1| |x|)))))) (forall (lam (|x| g) (forall (lam (|y| g) (implies (and (|s1| |x|) (|s1| |y|)) (= (|h1| (|f1| |x| |y|)) (|f2| (|h1| |x|) (|h1| |y|))))))))) (and (and (and (forall (lam (|x| b) (forall (lam (|y| b) (implies (and (|s2| |x|) (|s2| |y|)) (|s2| (|f2| |x| |y|))))))) (forall (lam (|x| a) (forall (lam (|y| a) (implies (and (|s3| |x|) (|s3| |y|)) (|s3| (|f3| |x| |y|)))))))) (forall (lam (|x| b) (implies (|s2| |x|) (|s3| (|h2| |x|)))))) (forall (lam (|x| b) (forall (lam (|y| b) (implies (and (|s2| |x|) (|s2| |y|)) (= (|h2| (|f2| |x| |y|)) (|f3| (|h2| |x|) (|h2| |y|)))))))))) (and (and (and (forall (lam (|x| g) (forall (lam (|y| g) (implies (and (|s1| |x|) (|s1| |y|)) (|s1| (|f1| |x| |y|))))))) (forall (lam (|x| a) (forall (lam (|y| a) (implies (and (|s3| |x|) (|s3| |y|)) (|s3| (|f3| |x| |y|)))))))) (forall (lam (|x| g) (implies (|s1| |x|) (|s3| ((lam (|x^0| g) (|h2| (|h1| |x^0|))) |x|)))))) (forall (lam (|x| g) (forall (lam (|y| g) (implies (and (and (|s1| |x|) (|s1| |y|)) (and (|s1| |x|) (|s1| |y|))) (= ((lam (|x^1| g) (|h2| (|h1| |x^1|))) (|f1| |x| |y|)) (|f3| ((lam (|x^2| g) (|h2| (|h1| |x^2|))) |x|) ((lam (|x^3| g) (|h2| (|h1| |x^3|))) |y|))))))))))))))))))))))))))))
 (help "thm126 expanded so that it only needs max-mates 1"))

(th~defproblem THM250A
 (in tps)
 (conclusion THM250A-conc
  (all-types a (forall (lam (R (o a a)) (forall (lam (S (o a a)) (= (lam (|p| a) (lam (|q| a) (TRCL (PAIRUNION R S) |p| |q|))) (lam (|p| a) (lam (|q| a) (TRCL (PAIRUNION (TRCL R) (TRCL S)) |p| |q|))))))))))
 (help ""))

(th~defproblem THM250
 (in tps)
 (conclusion THM250-conc
  (all-types a (forall (lam (R (o a a)) (forall (lam (S (o a a)) (= (TRCL (PAIRUNION R S)) (TRCL (PAIRUNION (TRCL R) (TRCL S))))))))))
 (help "thm about TRCL"))

(th~defproblem THM119
 (in tps)
 (conclusion THM119-conc
 (forall (lam (|a| i) (forall (lam (|b| i) (forall (lam (R (o i)) (forall (lam (|d| i) (forall (lam (|c| i) (forall (lam (Q (o i)) (forall (lam (P (o i)) (not (and (and (forall (lam (|z| i) (and (or (P |z|) (R |z|)) (Q |z|)))) (forall (lam (|x| i) (exists (lam (|y| i) (or (or (or (or (P |x|) (not (Q |x|))) (not (Q |y|))) (not (Q |c|))) (not (Q |d|)))))))) (or (not (P |a|)) (not (P |b|))))))))))))))))))))
 (help "Example from Issar where path-focused duplication of the form
(1 2) does not succeed, while (2 (1 1 ...)) does succeed."))

(th~defproblem THM531E
 (in tps)
 (conclusion THM531E-conc
  (all-types a (forall (lam (C (o a)) (forall (lam (B (o a)) (implies (and (FINITE-SET-SUBSET-EXT C) (SUBSET B C)) (FINITE-SET-SUBSET-EXT B))))))))
 (help "Subset of a finite set is finite, using FINITE-SET-SUBSET-EXT.
This one has an easy proof."))

(th~defproblem EQUIV-01-03
 (in tps)
 (conclusion EQUIV-01-03-conc
  (all-types g (forall (lam (|f| (g g g)) (forall (lam (|e| g) (equiv (GROUP1 |f| |e|) (GROUP3 |f| |e|))))))))
 (help ""))

(th~defproblem EQUIV-01-02
 (in tps)
 (conclusion EQUIV-01-02-conc
  (all-types g (forall (lam (|f| (g g g)) (forall (lam (|e| g) (equiv (GROUP1 |f| |e|) (GROUP2 |f| |e|))))))))
 (help ""))

(th~defproblem THM15A
 (in tps)
 (conclusion THM15A-conc
 (forall (lam (F (i i)) (implies (exists (lam (G (i i)) (and (ITERATES G F) (exists (lam (|x| i) (and (= (G |x|) |x|) (forall (lam (|y| i) (implies (= (G |y|) |y|) (= |x| |y|)))))))))) (exists (lam (Y i) (= (F Y) Y)))))))
 (help ""))

(th~defproblem THM47D
 (in tps)
 (conclusion THM47D-conc
 (forall (lam (X i) (forall (lam (Y i) (equiv (forall (lam (|q| (o i)) (implies (|q| X) (|q| Y)))) (forall (lam (R (o i i)) (implies (forall (lam (Z i) (R Z Z))) (R X Y))))))))))
 (help "THM47 with expand= applied"))

(th~defproblem THM252C
 (in tps)
 (conclusion THM252C-conc
  (all-types a (forall (lam (|h| (o (o a a))) (forall (lam (|r| (o a a)) (forall (lam (|s| (o a a)) (= (REL-PROP-CLOSURE |h| (PAIRUNION |r| |s|)) (REL-PROP-CLOSURE |h| (PAIRUNION (REL-PROP-CLOSURE |h| |r|) (REL-PROP-CLOSURE |h| |s|))))))))))))
 (help "THM250, generalised to any sort of closure"))

(th~defproblem THM115
 (in tps)
 (conclusion THM115-conc
 (forall (lam (|f| (i i)) (forall (lam (|a| i) (forall (lam (P (o i)) (exists (lam (A (o i)) (and (forall (lam (|x| i) (implies (A (|f| |x|)) (P |x|)))) (implies (and (P |a|) (INJECTIVE |f|)) (exists (lam (|z| i) (A |z|))))))))))))))
 (help "Sunil's example. 
The second conjunct is there only to force us to consider non empty sets
as substitutions for A."))

(th~defproblem THM582
 (in tps)
 (conclusion THM582-conc
 (forall (lam (\|0\| i) (forall (lam (S (i i)) (forall (lam (|0| i) (forall (lam (PLUS-LIBCONST (i i i)) (implies (and (IND-TPS \|0\| S) (PLUS-INDEQS-TPS PLUS-LIBCONST |0| S)) (forall (lam (|n| i) (forall (lam (|m| i) (= (PLUS-LIBCONST (S |n|) |m|) (S (PLUS-LIBCONST |n| |m|)))))))))))))))))
 (help "A lemma for proving + is commutative.  [S n] + m = S [n + m]."))

(th~defproblem THM3-TPS2
 (in tps)
 (conclusion THM3-TPS2-conc
 (exists (lam (|f| (o i i)) (INJECTIVE |f|))))
 (help "Hard theorem. See THM104 for a solution"))

(th~defproblem THM568
 (in tps)
 (conclusion THM568-conc
  (all-types b (forall (lam (LESSEQ (o b b)) (forall (lam (S (o (o b))) (implies (and (and (SUBSET S (DEDEKIND-CUT LESSEQ)) (exists (lam (|x| (o b)) (S |x|)))) (exists (lam (|c| (o b)) (and (DEDEKIND-CUT LESSEQ |c|) (forall (lam (|x| (o b)) (implies (S |x|) (SUBSET |c| |x|)))))))) (DEDEKIND-CUT LESSEQ (SETINTERSECT S)))))))))
 (help "The idea of this theorem is that if a collection of real numbers (given by
Dedekind Cuts) is bounded from below, then the inf of the collection is a real number.

HOWEVER(!!!):  I accidentally defined DEDEKIND-CUT wrong.  See DDEDEKIND-CUT
for the corrected definition.

SEE ALSO: THM569, THM570, THM571, THM572, DEDEKIND-CUT, DDEDEKIND-CUT."))

(th~defproblem THM126A
 (in tps)
 (conclusion THM126A-conc
  (all-types a g b (forall (lam (|h1| (b g)) (forall (lam (|h2| (a b)) (forall (lam (|s1| (o g)) (forall (lam (|f1| (g g g)) (forall (lam (|s2| (o b)) (forall (lam (|f2| (b b b)) (forall (lam (|s3| (o a)) (forall (lam (|f3| (a a a)) (implies (not (HOM2 (COMPOSE |h2| |h1|) |s1| |f1| |s3| |f3|)) (not (and (HOM2 |h1| |s1| |f1| |s2| |f2|) (HOM2 |h2| |s2| |f2| |s3| |f3|))))))))))))))))))))))
 (help ""))

(th~defproblem THM523
 (in tps)
 (conclusion THM523-conc
  (all-types a (forall (lam (|r| (o a a)) (forall (lam (|x| a) (forall (lam (|y| a) (equiv (RCLOSURE |r| |x| |y|) (forall (lam (|p| (o a a)) (implies (and (SUBRELATION |r| |p|) (REFLEXIVE |p|)) (|p| |x| |y|)))))))))))))
 (help "Theorem about Reflexive closure of relations,
compare to THM146 and THM152 (for Transitive closure)."))

(th~defproblem THM126
 (in tps)
 (conclusion THM126-conc
  (all-types a g b (forall (lam (|h1| (b g)) (forall (lam (|h2| (a b)) (forall (lam (|s1| (o g)) (forall (lam (|f1| (g g g)) (forall (lam (|s2| (o b)) (forall (lam (|f2| (b b b)) (forall (lam (|s3| (o a)) (forall (lam (|f3| (a a a)) (implies (and (HOM2 |h1| |s1| |f1| |s2| |f2|) (HOM2 |h2| |s2| |f2| |s3| |f3|)) (HOM2 (COMPOSE |h2| |h1|) |s1| |f1| |s3| |f3|))))))))))))))))))))
 (help "The composition of homomorphisms of binary  operators is a homomorphism. Suggested by Boyer et al JAR 2 page 284."))

(th~defproblem THM252
 (in tps)
 (conclusion THM252-conc
  (all-types a (forall (lam (PROP (o (o a a))) (forall (lam (R (o a a)) (forall (lam (S (o a a)) (= (CLOSURE PROP (PAIRUNION R S)) (CLOSURE PROP (PAIRUNION (CLOSURE PROP R) (CLOSURE PROP S))))))))))))
 (help "THM250, generalised to any sort of closure"))

(th~defproblem THM131
 (in tps)
 (conclusion THM131-conc
  (all-types a g b (forall (lam (|h1| (b g)) (forall (lam (|h2| (a b)) (forall (lam (|s1| (o g)) (forall (lam (|f1| (g g)) (forall (lam (|s2| (o b)) (forall (lam (|f2| (b b)) (forall (lam (|s3| (o a)) (forall (lam (|f3| (a a)) (implies (and (HOM |h1| |s1| |f1| |s2| |f2|) (HOM |h2| |s2| |f2| |s3| |f3|)) (HOM (COMPOSE |h2| |h1|) |s1| |f1| |s3| |f3|))))))))))))))))))))
 (help "The composition of homomorphisms of monadic  operators is a homomorphism.
Related to THM126, which was suggested by Boyer et al JAR 2 page 284."))

(th~defproblem THM581
 (in tps)
 (conclusion THM581-conc
 (forall (lam (\|0\| i) (forall (lam (S (i i)) (forall (lam (|0| i) (forall (lam (PLUS-LIBCONST (i i i)) (implies (and (IND-TPS \|0\| S) (PLUS-INDEQS-TPS PLUS-LIBCONST |0| S)) (forall (lam (|m| i) (= (PLUS-LIBCONST |0| |m|) |m|)))))))))))))
 (help "Assume natural number induction on type I, and inductive equations for +, we have
forall m . 0 + m = m"))

(th~defproblem THM145-B
 (in tps)
 (conclusion THM145-B-conc
  (all-types a (forall (lam (|k| a) (forall (lam (R (o a a)) (forall (lam (U (a (o a))) (implies (and (TRANSITIVE R) (forall (lam (|s| (o a)) (and (and (forall (lam (|z| a) (implies (|s| |z|) (R |z| (U |s|))))) (forall (lam (|z| a) (implies (|s| |z|) (R |z| (U |s|)))))) (forall (lam (|j| a) (implies (forall (lam (|Iik| a) (implies (|s| |k|) (R |k| |j|)))) (R (U |s|) |j|)))))))) (forall (lam (|f| (a a)) (implies (and (forall (lam (|x| a) (forall (lam (|y| a) (implies (R |x| |y|) (R (|f| |x|) (|f| |y|))))))) (forall (lam (|x| a) (forall (lam (|y| a) (implies (R |x| |y|) (R (|f| |x|) (|f| |y|)))))))) (exists (lam (|w| a) (and (R |w| (|f| |w|)) (R (|f| |w|) |w|))))))))))))))))
 (help "thm145 with num-of-dups 0; needs MAX-MATES>1"))

(th~defproblem THM250C
 (in tps)
 (conclusion THM250C-conc
  (all-types a (forall (lam (R (o a a)) (forall (lam (S (o a a)) (SUBRELATION (TRCL (PAIRUNION (TRCL R) (TRCL S))) (TRCL (PAIRUNION R S)))))))))
 (help ""))

(th~defproblem THM122
 (in tps)
 (conclusion THM122-conc
 (exists (lam (R (o (o i) (o i))) (and (and (forall (lam (|u| (o i)) (forall (lam (|v| (o i)) (implies (R |u| |v|) (forall (lam (|z| i) (implies (|u| |z|) (|v| |z|))))))))) (REFLEXIVE R)) (TRANSITIVE R)))))
 (help "Related to THM120B"))

(th~defproblem THM3
 (in tps)
 (conclusion THM3-conc
 (exists (lam (|f| (o i i)) (INJECTIVE |f|))))
 (help "Hard theorem. See THM104 for a solution"))

(th~defproblem GRP-COMM2
 (in tps)
 (conclusion GRP-COMM2-conc
 (forall (lam (|e| i) (forall (lam (P (i i i)) (implies (and (and (and (forall (lam (|x| i) (= (P |e| |x|) |x|))) (forall (lam (|y| i) (= (P |y| |e|) |y|)))) (forall (lam (|z| i) (= (P |z| |z|) |e|)))) (forall (lam (|x| i) (forall (lam (|y| i) (forall (lam (|z| i) (= (P (P |x| |y|) |z|) (P |x| (P |y| |z|)))))))))) (forall (lam (|a| i) (forall (lam (|b| i) (= (P |a| |b|) (P |b| |a|))))))))))))
 (help "Group is abelian iff every element has order 2"))

(th~defproblem BLEDSOE4-W-AX
 (in tps)
 (conclusion BLEDSOE4-W-AX-conc
 (forall (lam (|c| i) (forall (lam (|b| i) (forall (lam (|a| i) (forall (lam (< (o i i)) (implies (forall (lam (|x| i) (forall (lam (|y| i) (implies (< |x| |y|) (not (= |x| |y|))))))) (implies (and (< |a| |b|) (< |b| |c|)) (exists (lam (A (o i)) (and (and (not (A |a|)) (A |b|)) (not (A |c|))))))))))))))))
 (help "BLEDSOE4 (p. 296, Example 4 of the Set-Var paper in JAR) is not a theorem without
assuming < implies not =.  This gwff (BLEDSOE4-W-AX) puts this axiom into the hyp."))

(th~defproblem THM560
 (in tps)
 (conclusion THM560-conc
  (all-types a (equiv (forall (lam (|r| (o a (o a))) (exists (lam (|g| (a (o a))) (forall (lam (|x| (o a)) (implies (exists (lam (|y| a) (|r| |x| |y|))) (|r| |x| (|g| |x|))))))))) (forall (lam (|s| (o (o a))) (implies (forall (lam (X (o a)) (implies (|s| X) (exists (lam (|t| a) (X |t|)))))) (exists (lam (|f| (a (o a))) (forall (lam (X (o a)) (implies (|s| X) (X (|f| X)))))))))))))
 (help "AC1(A) equiv AC3(OA,A) from Rubin and Rubin.  Note that AC3 usually
refers to 'relations' where here we are using it for relations on OA x A."))

(th~defproblem THM584
 (in tps)
 (conclusion THM584-conc
 (forall (lam (\|0\| i) (forall (lam (S (i i)) (forall (lam (|0| i) (forall (lam (PLUS-LIBCONST (i i i)) (implies (and (IND-TPS \|0\| S) (PLUS-INDEQS-TPS PLUS-LIBCONST |0| S)) (forall (lam (|n| i) (forall (lam (|m| i) (forall (lam (|k| i) (= (PLUS-LIBCONST (PLUS-LIBCONST |m| |n|) |k|) (PLUS-LIBCONST |m| (PLUS-LIBCONST |n| |k|)))))))))))))))))))
 (help "Associativity of addition, proven by induction on k."))

(th~defproblem THM47B
 (in tps)
 (conclusion THM47B-conc
 (forall (lam (X i) (forall (lam (Y i) (implies (forall (lam (R (o i i)) (implies (forall (lam (Z i) (R Z Z))) (R X Y)))) (= X Y)))))))
 (help ""))

(th~defproblem THM261-B
 (in tps)
 (conclusion THM261-B-conc
  (all-types a (forall (lam (P (o (o a))) (implies (PARTITION-B P) (EQUIVALENCE-REL (lam (X a) (lam (Y a) (exists (lam (S (o a)) (and (and (P S) (S X)) (S Y))))))))))))
 (help "A partition defines an equivalence relation"))

(th~defproblem THM143
 (in tps)
 (conclusion THM143-conc
 (forall (lam (|d| (o i)) (forall (lam (|h| (i (o i))) (implies (and (forall (lam (|p| (o i)) (forall (lam (|q| (o i)) (implies (= (|h| |p|) (|h| |q|)) (= |p| |q|)))))) (= |d| (IN-IMAGE |h| (lam (|s| (o i)) (not (|s| (|h| |s|))))))) (not (|d| (|h| |d|)))))))))
 (help "Essentially a lemma for the Injective Cantor Theorem X5309.
Also see X5309a, THM118, and THM143A."))

(th~defproblem BLEDSOE-FENG-SV-I2
 (in tps)
 (conclusion BLEDSOE-FENG-SV-I2-conc
 (forall (lam (P (o i)) (forall (lam (|n| i) (forall (lam (|m| i) (forall (lam (|s| (i i)) (forall (lam (|xO^0| i) (implies (and (forall (lam (A (o i i)) (implies (and (A |xO^0| |xO^0|) (forall (lam (|x| i) (forall (lam (|y| i) (implies (A |x| |y|) (A (|s| |x|) (|s| |y|)))))))) (A |n| |m|)))) (P |n|)) (P |m|)))))))))))))
 (help "Example I2 from Bledsoe-Feng SET-VAR paper"))

(th~defproblem THM126-CORRECTED
 (in tps)
 (conclusion THM126-CORRECTED-conc
  (all-types a g b (forall (lam (|h1| (b g)) (forall (lam (|h2| (a b)) (forall (lam (|s1| (o g)) (forall (lam (|f1| (g g g)) (forall (lam (|s2| (o b)) (forall (lam (|f2| (b b b)) (forall (lam (|h1^0| (b g)) (forall (lam (|h2^0| (a b)) (forall (lam (|s1^0| (o g)) (forall (lam (|f1^0| (g g g)) (forall (lam (|s2^0| (o b)) (forall (lam (|f2^0| (b b b)) (forall (lam (|s3| (o a)) (forall (lam (|f3| (a a a)) (implies (and (and (and (forall (lam (|x| g) (forall (lam (|y| g) (implies (and (|s1^0| |x|) (|s1^0| |y|)) (|s1^0| (|f1^0| |x| |y|))))))) (and (and (forall (lam (|x| g) (implies (|s1^0| |x|) (|s2^0| (|h1^0| |x|))))) (forall (lam (|x| g) (implies (|s1^0| |x|) (|s2^0| (|h1^0| |x|)))))) (forall (lam (|x| g) (implies (|s1^0| |x|) (|s2^0| (|h1^0| |x|))))))) (forall (lam (|x| g) (forall (lam (|y| g) (implies (and (|s1^0| |x|) (|s1^0| |y|)) (= (|h1^0| (|f1^0| |x| |y|)) (|f2^0| (|h1^0| |x|) (|h1^0| |y|))))))))) (and (and (forall (lam (|x| a) (forall (lam (|y| a) (implies (and (|s3| |x|) (|s3| |y|)) (|s3| (|f3| |x| |y|))))))) (forall (lam (|x| b) (implies (|s2^0| |x|) (|s3| (|h2^0| |x|)))))) (forall (lam (|x| b) (forall (lam (|y| b) (implies (and (|s2^0| |x|) (|s2^0| |y|)) (= (|h2^0| (|f2^0| |x| |y|)) (|f3| (|h2^0| |x|) (|h2^0| |y|)))))))))) (and (and (and (forall (lam (|x| g) (forall (lam (|y| g) (implies (and (|s1^0| |x|) (|s1^0| |y|)) (|s1^0| (|f1^0| |x| |y|))))))) (forall (lam (|x| a) (forall (lam (|y| a) (implies (and (|s3| |x|) (|s3| |y|)) (|s3| (|f3| |x| |y|)))))))) (forall (lam (|x| g) (implies (|s1^0| |x|) (|s3| ((lam (|x^4| g) (|h2^0| (|h1^0| |x^4|))) |x|)))))) (forall (lam (|x| g) (forall (lam (|y| g) (implies (and (and (|s1^0| |x|) (|s1^0| |y|)) (and (|s1^0| |x|) (|s1^0| |y|))) (= ((lam (|x^5| g) (|h2^0| (|h1^0| |x^5|))) (|f1^0| |x| |y|)) (|f3| ((lam (|x^6| g) (|h2^0| (|h1^0| |x^6|))) |x|) ((lam (|x^7| g) (|h2^0| (|h1^0| |x^7|))) |y|))))))))))))))))))))))))))))))))))))))))
 (help "thm126 with all duplications done, and needing max-mates 1"))

(th~defproblem THM120I-1
 (in tps)
 (conclusion THM120I-1-conc
 (exists (lam (R (o (o i) (o i))) (and (and (REFLEXIVE R) (TRANSITIVE R)) (forall (lam (X (o i)) (forall (lam (Y (o i)) (implies (and (R X Y) (R Y X)) (EQUIVS1 X Y))))))))))
 (help "thm120i using EQUIVS1 instead of EQUIVS"))

(th~defproblem S-JOINFN-MONOTONE
 (in tps)
 (conclusion S-JOINFN-MONOTONE-conc
  (all-types a (forall (lam (P (a a a)) (forall (lam (|0| a) (MONOTONE3 (lam (R (o a a a)) true) (lam (R (o a a a)) (lam (|x| a) (lam (|y| a) (lam (|z| a) (or (or (and (= |x| |0|) (= |y| |z|)) (and (= |y| |0|) (= |x| |z|))) (exists (lam (|x1| a) (exists (lam (|x2| a) (exists (lam (|y1| a) (exists (lam (|y2| a) (exists (lam (|z1| a) (exists (lam (|z2| a) (and (and (and (and (= |x| (P |x1| |x2|)) (= |y| (P |y1| |y2|))) (= |z| (P |z1| |z2|))) (R |x1| |y1| |z1|)) (R |x2| |y2| |z2|))))))))))))))))))))))))))
 (help "The function used to define JOIN as a LFP is monotone."))

(th~defproblem THM522
 (in tps)
 (conclusion THM522-conc
  (all-types a (forall (lam (|r| (o a a)) (forall (lam (|x| a) (forall (lam (|y| a) (equiv (SCLOSURE |r| |x| |y|) (forall (lam (|p| (o a a)) (implies (and (SUBRELATION |r| |p|) (SYMMETRIC |p|)) (|p| |x| |y|)))))))))))))
 (help "Theorem about Symmetric closure of relations,
compare to THM146 and THM152 (for Transitive closure)."))

(th~defproblem THM250B
 (in tps)
 (conclusion THM250B-conc
  (all-types a (forall (lam (R (o a a)) (forall (lam (S (o a a)) (SUBRELATION (TRCL (PAIRUNION R S)) (TRCL (PAIRUNION (TRCL R) (TRCL S))))))))))
 (help ""))

(th~defproblem LING1
 (in tps)
 (conclusion LING1-conc
 (forall (lam (P i) (forall (lam (S i) (forall (lam (LIKE (o i i)) (forall (lam (J i) (forall (lam (WRH (o i)) (forall (lam (W (o i)) (forall (lam (UNIQUE (o i)) (implies (and (and (and (forall (lam (X i) (implies (UNIQUE X) (forall (lam (Z i) (implies (and (WRH Z) (W Z)) (= X Z))))))) (UNIQUE S)) (W S)) (WRH S)) (exists (lam (|an| (o i)) (and (= (|an| P) (LIKE P S)) (= (|an| J) (exists (lam (X i) (and (and (and (UNIQUE X) (W X)) (WRH X)) (LIKE J X))))))))))))))))))))))))
 (help ""))

(th~defproblem THM145-A
 (in tps)
 (conclusion THM145-A-conc
  (all-types a (forall (lam (|k| a) (forall (lam (R (o a a)) (forall (lam (U (a (o a))) (implies (and (TRANSITIVE R) (forall (lam (|s| (o a)) (and (and (forall (lam (|z| a) (implies (|s| |z|) (R |z| (U |s|))))) (forall (lam (|z| a) (implies (|s| |z|) (R |z| (U |s|)))))) (forall (lam (|j| a) (implies (forall (lam (|Iik| a) (implies (and (|s| |k|) (|s| |k|)) (R |k| |j|)))) (and (R (U |s|) |j|) (R (U |s|) |j|))))))))) (forall (lam (|f| (a a)) (implies (and (forall (lam (|x| a) (forall (lam (|y| a) (implies (R |x| |y|) (R (|f| |x|) (|f| |y|))))))) (forall (lam (|x| a) (forall (lam (|y| a) (implies (R |x| |y|) (R (|f| |x|) (|f| |y|)))))))) (exists (lam (|w| a) (and (R |w| (|f| |w|)) (R (|f| |w|) |w|))))))))))))))))
 (help "edited version of thm145, requiring max-mates 1 and num-of-dups 0"))

(th~defproblem THM571
 (in tps)
 (conclusion THM571-conc
  (all-types b (forall (lam (LESSEQ (o b b)) (forall (lam (S (o (o b))) (implies (and (and (SUBSET S (DDEDEKIND-CUT LESSEQ)) (exists (lam (|x| (o b)) (S |x|)))) (exists (lam (|c| (o b)) (and (DDEDEKIND-CUT LESSEQ |c|) (forall (lam (|x| (o b)) (implies (S |x|) (SUBSET |c| |x|)))))))) (GLB (DDEDEKIND-CUT LESSEQ) (lam (|x| (o b)) (lam (|y| (o b)) (SUBSET |x| |y|))) S (SETINTERSECT S)))))))))
 (help "The idea of this theorem is that if a collection of real numbers (given by
Dedekind Cuts) is bounded from below, then the inf of the collection is the glb."))

(th~defproblem THM530
 (in tps)
 (conclusion THM530-conc
  (all-types a (forall (lam (PROP (o (o a a))) (forall (lam (F (o (o a a))) (= (CLOSURE PROP (PAIRSETUNION F)) (CLOSURE PROP (PAIRSETUNION (lam (S (o a a)) (exists (lam (R (o a a)) (and (F R) (= S (CLOSURE PROP R)))))))))))))))
 (help "Generalization of THM252 to arbitrary unions"))

(th~defproblem THM150
 (in tps)
 (conclusion THM150-conc
  (all-types a (forall (lam (|r| (o a a)) (TRANSITIVE (TC2 |r|))))))
 (help "The transitive closure TC2 of a relation is transitive."))

(th~defproblem TTTP6102
 (in tps)
 (conclusion TTTP6102-conc
 (forall (lam (|p| (o (o (o i)))) (implies (and (|p| ZERO) (forall (lam (|x| (o (o i))) (implies (NAT |x|) (implies (|p| |x|) (|p| (SUCC |x|))))))) (forall (lam (|m| (o (o i))) (implies (NAT |m|) (|p| |m|))))))))
 (help "The Induction Theorem. Theorem 6102 from TTTP"))

(th~defproblem THM67
 (in tps)
 (conclusion THM67-conc
  (all-types a (forall (lam (G (o a (o a))) (forall (lam (F (o a (o a))) (implies (and (forall (lam (S (o a)) (forall (lam (T (o a)) (implies (SUBSET S T) (SUBSET (F T) (F S))))))) (and (forall (lam (S (o a)) (SUBSET S (F (G S))))) (forall (lam (S (o a)) (SUBSET S (G (F S))))))) (forall (lam (S (o a)) (EQUIVS (F (G (F S))) (F S)))))))))))
 (help ""))

(th~defproblem THM532
 (in tps)
 (conclusion THM532-conc
  (all-types a b (implies (forall (lam (|s| (o (o b))) (implies (forall (lam (X (o b)) (implies (|s| X) (exists (lam (|y| b) (X |y|)))))) (exists (lam (|f| (b (o b))) (forall (lam (X (o b)) (implies (|s| X) (X (|f| X)))))))))) (forall (lam (|r| (o b a)) (exists (lam (|g| (b a)) (forall (lam (|x| a) (implies (exists (lam (|y| b) (|r| |x| |y|))) (|r| |x| (|g| |x|))))))))))))
 (help "AC1  =>  AC3 from Rubin and Rubin.
TPS can prove this automatically using Matt's Procedure"))

(th~defproblem THM261
 (in tps)
 (conclusion THM261-conc
  (all-types a (forall (lam (P (o (o a))) (implies (PARTITION P) (EQUIVALENCE-REL (lam (X a) (lam (Y a) (exists (lam (S (o a)) (and (and (P S) (S X)) (S Y))))))))))))
 (help "A partition defines an equivalence relation"))

(th~defproblem THM108
 (in tps)
 (conclusion THM108-conc
  (all-types a (forall (lam (|w| (o a)) (forall (lam (|y| (o a)) (forall (lam (|z| (o a)) (implies (and (SUBSET (INTERSECT |w| (COMPLEMENT-TPS |z|)) |y|) (= (SETDIFF |z| |y|) NULLSET)) (SUBSET |w| |y|))))))))))
 (help "21-127 exam problem requiring cse analysis"))

(th~defproblem THM565
 (in tps)
 (conclusion THM565-conc
  (all-types b (forall (lam (P (o (o b) (o b))) (CLOS-SYS1 (lam (W (o b)) (forall (lam (X (o b)) (forall (lam (Y (o b)) (implies (and (SUBSET X W) (P X Y)) (SUBSET Y W))))))))))))
 (help "A generalization of THM564 (and THM563) suggested by Hongwei Xi."))

(th~defproblem THM53
 (in tps)
 (conclusion THM53-conc
 (forall (lam (P (o i)) (equiv (forall (lam (X i) (equiv (P X) (exists (lam (Y i) (P Y)))))) (equiv (forall (lam (X i) (P X))) (exists (lam (Y i) (P Y))))))))
 (help "DISTRIBUTES QUANTIFIER OVER EQUIVALENCE  
GENERATES 30 CLAUSES  "))

(th~defproblem THM350
 (in tps)
 (conclusion THM350-conc
 (forall (lam (NUMBER (o i)) (forall (lam (ODD (o i)) (forall (lam (S (i i)) (forall (lam (|0| i) (forall (lam (EVEN (o i)) (implies (and (and (and (and (EVEN |0|) (forall (lam (|n| i) (implies (EVEN |n|) (EVEN (S (S |n|))))))) (and (ODD (S |0|)) (forall (lam (|n| i) (implies (ODD |n|) (ODD (S (S |n|)))))))) (implies (and (and (NUMBER |0|) (NUMBER (S |0|))) (forall (lam (|x| i) (implies (and (NUMBER |x|) (NUMBER (S |x|))) (and (NUMBER (S |x|)) (NUMBER (S (S |x|)))))))) (forall (lam (|x| i) (and (NUMBER |x|) (NUMBER (S |x|))))))) (forall (lam (|n| i) (equiv (NUMBER |n|) (or (EVEN |n|) (ODD |n|)))))) (forall (lam (|n| i) (NUMBER |n|)))))))))))))))
 (help ""))

(th~defproblem THM300A
 (in tps)
 (conclusion THM300A-conc
 (forall (lam (DOUBLE (o i i)) (forall (lam (HALF (o i i)) (forall (lam (|0| i) (forall (lam (S (i i)) (implies (and (and (forall (lam (|u| i) (forall (lam (|v| i) (equiv (HALF |u| |v|) (forall (lam (Q (o i i)) (implies (and (and (Q |0| |0|) (Q (S |0|) |0|)) (forall (lam (|x| i) (forall (lam (|y| i) (implies (Q |x| |y|) (Q (S (S |x|)) (S |y|)))))))) (Q |u| |v|))))))))) (DOUBLE |0| |0|)) (forall (lam (|x| i) (forall (lam (|y| i) (implies (DOUBLE |x| |y|) (DOUBLE (S |x|) (S (S |y|))))))))) (forall (lam (|u| i) (forall (lam (|v| i) (implies (HALF |u| |v|) (or (DOUBLE |v| |u|) (DOUBLE (S |v|) (S |u|))))))))))))))))))
 (help "DOUBLEuv means that 2u = v; HALFuv means that the greatest integer in u/2 is v."))

(th~defproblem THM577
 (in tps)
 (conclusion THM577-conc
 (forall (lam (S (i i)) (TRANSITIVE (lam (|x| i) (lam (|y| i) (forall (lam (|p| (o i)) (implies (and (|p| |x|) (forall (lam (|n| i) (implies (|p| |n|) (|p| (S |n|)))))) (|p| |y|))))))))))
 (help "Inductive Defn of <= on naturals is Transitive"))

(th~defproblem THM48-EXPD
 (in tps)
 (conclusion THM48-EXPD-conc
  (all-types a c b (forall (lam (G (b c)) (forall (lam (F (a b)) (implies (and (forall (lam (|x| b) (forall (lam (|y| b) (implies (forall (lam (|q| (o a)) (implies (|q| (F |x|)) (|q| (F |y|))))) (forall (lam (|q| (o b)) (implies (|q| |x|) (|q| |y|))))))))) (forall (lam (|x| c) (forall (lam (|y| c) (implies (forall (lam (|q| (o b)) (implies (|q| (G |x|)) (|q| (G |y|))))) (forall (lam (|q| (o c)) (implies (|q| |x|) (|q| |y|)))))))))) (forall (lam (|x| c) (forall (lam (|y| c) (implies (forall (lam (|q| (o a)) (implies (|q| (F (G |x|))) (|q| (F (G |y|)))))) (forall (lam (|q| (o c)) (implies (|q| |x|) (|q| |y|))))))))))))))))
 (help "expanded form of thm48"))

(th~defproblem THM2D
 (in tps)
 (conclusion THM2D-conc
 (forall (lam (K (o i (o i))) (implies (and (forall (lam (|x| (o i)) (implies (SUBSET |x| (SETUNION (lam (|x^0| (o i)) (SUBSET |x^0| (K |x^0|))))) (SUBSET (K |x|) (K (SETUNION (lam (|x^1| (o i)) (SUBSET |x^1| (K |x^1|))))))))) (implies (SUBSET (SETUNION (lam (|x| (o i)) (SUBSET |x| (K |x|)))) (K (SETUNION (lam (|x| (o i)) (SUBSET |x| (K |x|)))))) (SUBSET (K (SETUNION (lam (|x| (o i)) (SUBSET |x| (K |x|))))) (K (K (SETUNION (lam (|x| (o i)) (SUBSET |x| (K |x|))))))))) (EQUIVS (K (SETUNION (lam (|x| (o i)) (SUBSET |x| (K |x|))))) (lam (|x| i) (and ((lam (|x^2| (o i)) (SUBSET |x^2| (K |x^2|))) (K (SETUNION (lam (|x^3| (o i)) (SUBSET |x^3| (K |x^3|)))))) (K (SETUNION (lam (|x^4| (o i)) (SUBSET |x^4| (K |x^4|)))) |x|))))))))
 (help "thm2a with all the primsubs done..."))

(th~defproblem THM131D
 (in tps)
 (conclusion THM131D-conc
  (all-types a g b (forall (lam (|h1| (b g)) (forall (lam (|h2| (a b)) (forall (lam (|s1| (o g)) (forall (lam (|f1| (g g)) (forall (lam (|s2| (o b)) (forall (lam (|f2| (b b)) (forall (lam (|s3| (o a)) (forall (lam (|f3| (a a)) (implies (and (HOM |h1| |s1| |f1| |s2| |f2|) (HOM |h2| |s2| |f2| |s3| |f3|)) (forall (lam (|x| g) (implies (|s1| |x|) (= (COMPOSE |h2| |h1| (|f1| |x|)) (|f3| (COMPOSE |h2| |h1| |x|)))))))))))))))))))))))))
 (help "THM131 with only part d of the conclusion"))

(th~defproblem TTTP5306A
 (in tps)
 (conclusion TTTP5306A-conc
  (all-types a (forall (lam (|p| (o a)) (equiv (SIGMA1 |p|) (SIGMA1A |p|))))))
 (help "Closely related to theorem 5306 in TTTP."))

(th~defproblem THM580
 (in tps)
 (conclusion THM580-conc
  (all-types $ (forall (lam (JOIN (o $ $ $)) (forall (lam (P ($ $ $)) (forall (lam (|0| $) (implies (and (S-ALG |0| P) (S-JOIN-CLOS |0| P JOIN)) (forall (lam (|x| $) (JOIN |x| |x| |x|))))))))))))
 (help "The join (in the initial pairing algebra $) of x and x is x.
S-ALG 0 P in the hypothesis forces the type $ to be the initial pairing algebra.
S-JOIN-CLOS 0 P JOIN in the hypothesis states the JOIN satisfies the proper closure conditions.
(We do not need that JOIN is the smallest such relation for this theorem.)
The proof is by induction on x using S-ALG 0 P."))

(th~defproblem THM55
 (in tps)
 (conclusion THM55-conc
 (forall (lam (P (o i)) (equiv (exists (lam (X i) (forall (lam (Y i) (equiv (P X) (P Y)))))) (equiv (exists (lam (X i) (P X))) (forall (lam (Y i) (P Y))))))))
 (help "DISTRIBUTES QUANTIFIERS OVER AN EQUIVALENCE  
YIELDS 24 CLAUSES  
YIELDS 8 CLAUSES AND 64 PATHS WHEN NO. CLAUSES IS MINIMIZED  
YIELDS 8 PATHS AND 64 CLAUSES WHEN NO. PATHS IS MINIMIZED  
REQUIRES QUANTIFIER DUPLICATION YIELDING 16 PATHS  "))

(th~defproblem THM300
 (in tps)
 (conclusion THM300-conc
 (forall (lam (HALF (o i i)) (forall (lam (S (i i)) (forall (lam (|0| i) (forall (lam (DOUBLE (o i i)) (implies (and (and (DOUBLE |0| |0|) (forall (lam (|x| i) (forall (lam (|y| i) (implies (DOUBLE |x| |y|) (DOUBLE (S |x|) (S (S |y|))))))))) (forall (lam (Q (o i i)) (forall (lam (|u| i) (forall (lam (|v| i) (implies (and (HALF |u| |v|) (and (and (Q |0| |0|) (Q (S |0|) |0|)) (forall (lam (|x| i) (forall (lam (|y| i) (implies (Q |x| |y|) (Q (S (S |x|)) (S |y|))))))))) (Q |u| |v|))))))))) (forall (lam (|u| i) (forall (lam (|v| i) (implies (HALF |u| |v|) (or (DOUBLE |v| |u|) (DOUBLE (S |v|) (S |u|))))))))))))))))))
 (help ""))

(th~defproblem THM204
 (in tps)
 (conclusion THM204-conc
  (all-types a (forall (lam (|r| (o a a)) (TRANSITIVE (TRCL-BBP |r|))))))
 (help ""))

(th~defproblem THM130B
 (in tps)
 (conclusion THM130B-conc
 (forall (lam (|r| (o (o (o i)) (o (o i)))) (implies (and (|r| ZERO ZERO) (forall (lam (|x| (o (o i))) (forall (lam (|y| (o (o i))) (implies (|r| |x| |y|) (|r| (SUCC |x|) (SUCC |y|)))))))) (forall (lam (|x| (o (o i))) (implies (NAT |x|) (exists (lam (|y| (o (o i))) (|r| |x| |y|))))))))))
 (help "A simple theorem to be proved by induction. NAT, ZERO, and SUCC
 are built into TPS, so they don't have to be declared as needed objects."))

(th~defproblem CLOSED-THM1
 (in tps)
 (conclusion CLOSED-THM1-conc
  (all-types b a (forall (lam (T (o (o a))) (forall (lam (S (o (o b))) (forall (lam (|f| (b a)) (implies (and (and (TOPOLOGY T) (TOPOLOGY S)) (CONTINUOUS S T |f|)) (forall (lam (X (o b)) (implies (CLOSED S X) (forall (lam (Y (o a)) (implies (= Y (lam (|b| a) (X (|f| |b|)))) (CLOSED T Y))))))))))))))))
 (help "The inverse image of a closed set under a continuous function is closed. Easily proven 
with MM 1, NOD 0, MSV 3, MSD 4, when TOPOLOGY is not instantiated. Much harder otherwise"))

(th~defproblem THM91
 (in tps)
 (conclusion THM91-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (K (o a (o a))) (implies (forall (lam (X (o a)) (forall (lam (Y (o a)) (implies (SUBSET X Y) (SUBSET (K X) (K Y))))))) (SUBSET (K S) (K (SETUNION (lam (P (o a)) (SUBSET P S))))))))))))
 (help "Easy theorem TPS3 may not be able to prove;
example related to the completeness question."))

(th~defproblem THM80
 (in tps)
 (conclusion THM80-conc
 (forall (lam (N (o i)) (forall (lam (G (o i)) (forall (lam (M (o i)) (implies (or (or (or (forall (lam (R i) (M R))) (exists (lam (X i) (not (G X))))) (not (or (forall (lam (Y i) (M Y))) (exists (lam (S i) (N S)))))) (not (or (forall (lam (Z i) (not (N Z)))) (not (forall (lam (T i) (G T))))))) (or (forall (lam (R i) (M R))) (or (exists (lam (X i) (not (G X)))) (or (not (forall (lam (Y i) (or (M Y) (exists (lam (S i) (N S))))))) (not (forall (lam (Z i) (or (not (N Z)) (not (forall (lam (T i) (G T)))))))))))))))))))
 (help "THIS IS [S IMPLIES T] IN FALSE LEMMAS IN HERBRAND "))

(th~defproblem THM60
 (in tps)
 (conclusion THM60-conc
  (all-types a (forall (lam (Z (o a)) (forall (lam (W (o (o a))) (EQUIVS (UNION (SETINTERSECT W) Z) (SETINTERSECT (IN-IMAGE (lam (X (o a)) (UNION Z X)) W)))))))))
 (help ""))

(th~defproblem THM56
 (in tps)
 (conclusion THM56-conc
 (forall (lam (P (o i)) (equiv (forall (lam (X i) (equiv (P X) (forall (lam (Y i) (P Y)))))) (equiv (exists (lam (X i) (P X))) (forall (lam (Y i) (P Y))))))))
 (help "DISTRIBUTES QUANTIFIER OVER AN EQUIVALENCE  
REQUIRES NO QUANTIFIER DUPLICATION "))

(th~defproblem THM541
 (in tps)
 (conclusion THM541-conc
  (all-types a (equiv (exists (lam (|f| (a (o a))) (forall (lam (X (o a)) (implies (exists (lam (|t| a) (X |t|))) (X (|f| X))))))) (forall (lam (|s| (o (o a))) (implies (forall (lam (X (o a)) (implies (|s| X) (exists (lam (|t| a) (X |t|)))))) (exists (lam (|f| (a (o a))) (forall (lam (X (o a)) (implies (|s| X) (X (|f| X)))))))))))))
 (help "Equivalence of global choice at type A (usual way of expressing AC in type theory)
and AC1 from Rubin and Rubin."))

(th~defproblem THM182
 (in tps)
 (conclusion THM182-conc
  (all-types a (forall (lam (|x| a) (forall (lam (|y| a) (forall (lam (|u| a) (forall (lam (|v| a) (equiv (= (PAIR1 |x| |y|) (PAIR1 |u| |v|)) (and (= |x| |u|) (= |y| |v|)))))))))))))
 (help "Basic theorem about pairing"))

(th~defproblem THM564
 (in tps)
 (conclusion THM564-conc
  (all-types b (forall (lam (P (o b (o b))) (CLOS-SYS1 (lam (W (o b)) (forall (lam (X (o b)) (forall (lam (|y| b) (implies (and (SUBSET X W) (P X |y|)) (W |y|))))))))))))
 (help "Generalization of THM563 suggested by Hongwei"))

(th~defproblem THM578
 (in tps)
 (conclusion THM578-conc
 (forall (lam (|0| i) (forall (lam (S (i i)) (forall (lam (\|0\| i) (implies (IND-TPS \|0\| S) (forall (lam (|n| i) (or (= |n| |0|) (exists (lam (|m| i) (= |n| (S |m|)))))))))))))))
 (help "Variant of thm 6104 in TTTP, including induction in the hypothesis.
The set variable can be instantiated by unification."))

(th~defproblem THM47C
 (in tps)
 (conclusion THM47C-conc
 (= (lam (|u| i) (lam (|v| i) (= |u| |v|))) (SETPAIRSINTERSECT (lam (|r| (o i i)) (REFLEXIVE |r|)))))
 (help "THM47 stated using SETPAIRSINTERSECT and REFLEXIVE"))

(th~defproblem THM70
 (in tps)
 (conclusion THM70-conc
  (all-types a (forall (lam (S (o (o a a))) (TRANSITIVE (SETPAIRSINTERSECT (lam (R (o a a)) (and (TRANSITIVE R) (S R)))))))))
 (help "THE INTERSECTION OF ANY CLASS OF TRANSITIVE RELATIONS IS TRANSITIVE"))

(th~defproblem THM553
 (in tps)
 (conclusion THM553-conc
  (all-types a (forall (lam (T (o a)) (forall (lam (S (o a)) (forall (lam (R (o a a)) (implies (and (and (LIN-ORD R) (DOWN-CLOSED R S)) (DOWN-CLOSED R T)) (or (SUBSET S T) (SUBSET T S)))))))))))
 (help "Downward closed subsets of a linear order are comparable -- actually
this only requires the relation to satisfy comparability, not the partial order properties."))

(th~defproblem THM172
 (in tps)
 (conclusion THM172-conc
 (forall (lam (|g| (i i)) (forall (lam (|f| (i i)) (implies (ITERATE+ |f| |g|) (implies (exists (lam (|x| i) (and (= (|g| |x|) |x|) (forall (lam (|z| i) (implies (= (|g| |z|) |z|) (= |z| |x|))))))) (exists (lam (|y| i) (= (|f| |y|) |y|))))))))))
 (help "Part of the series THM170-THM172, which provides a nice example of cut. If g is an iterate of f, and g has a unique fixed point, then f has a fixed point.
Closely related to THM15B."))

(th~defproblem THM15C
 (in tps)
 (conclusion THM15C-conc
 (forall (lam (|f| (i i)) (implies (exists (lam (|g| (i i)) (and (ITERATE |f| |g|) (exists (lam (|x| i) (and (= (|g| |x|) |x|) (forall (lam (|z| i) (implies (= (|g| |z|) |z|) (= |z| |x|)))))))))) (exists (lam (|y| i) (= (|f| |y|) |y|)))))))
 (help "Like THM15B but with ITERATE instead of ITERATE+"))

(th~defproblem THM15BA
 (in tps)
 (conclusion THM15BA-conc
 (forall (lam (|f| (i i)) (implies (exists (lam (|g| (i i)) (and (ITERATE+ |f| |g|) (exists (lam (|u| i) (and (= (|g| |u|) |u|) (forall (lam (|z| i) (implies (= (|g| |z|) |z|) (= |z| |u|)))))))))) (exists (lam (|y| i) (= (|f| |y|) |y|)))))))
 (help ""))

(th~defproblem THM143A
 (in tps)
 (conclusion THM143A-conc
 (forall (lam (|h| (i (o i))) (implies (forall (lam (|p| (o i)) (forall (lam (|q| (o i)) (implies (= (|h| |p|) (|h| |q|)) (= |p| |q|)))))) (not (IN-IMAGE |h| (lam (|s| (o i)) (not (|s| (|h| |s|)))) (|h| (IN-IMAGE |h| (lam (|s| (o i)) (not (|s| (|h| |s|))))))))))))
 (help "Essentially a lemma for the Injective Cantor Theorem X5309.
Also see X5309a, THM118, and THM143."))

(th~defproblem THM120D
 (in tps)
 (conclusion THM120D-conc
 (exists (lam (|r^28| (o i (o i) (o i))) (exists (lam (|r^27| (o i (o i) (o i))) (and (and (REFLEXIVE (lam (|w^1| (o i)) (lam (|w^2| (o i)) (forall (lam (|w^3| i) ((lam (|w^5| (o i)) (lam (|w^4| (o i)) (lam (|w^6| i) (or (|r^27| |w^5| |w^4| |w^6|) (|r^28| |w^5| |w^4| |w^6|))))) |w^1| |w^2| |w^3|)))))) (TRANSITIVE (lam (|w^1| (o i)) (lam (|w^2| (o i)) (forall (lam (|w^4| i) ((lam (|w^3| (o i)) (lam (|w^6| (o i)) (lam (|w^5| i) (or (|r^27| |w^3| |w^6| |w^5|) (|r^28| |w^3| |w^6| |w^5|))))) |w^1| |w^2| |w^4|))))))) (not ((lam (|w^1| (o i)) (lam (|w^2| (o i)) (forall (lam (|w^7| i) ((lam (|w^3| (o i)) (lam (|w^4| (o i)) (lam (|w^5| i) (or (|r^27| |w^3| |w^4| |w^5|) (|r^28| |w^3| |w^4| |w^5|))))) |w^1| |w^2| |w^7|))))) (lam (|x| i) true) (lam (|x| i) false)))))))))
 (help "Variant of thm120c with matrix rearranged"))

(th~defproblem DOMLEMMA2-1
 (in tps)
 (conclusion DOMLEMMA2-1-conc
  (all-types a (forall (lam (|t| a) (forall (lam (H (o a)) (implies (FINITESET H) (FINITESET (UNION H (UNITSET |t|))))))))))
 (help "Another Lemma about Finite sets.  If H is finite, then H U {t} is Finite."))

(th~defproblem THM500
 (in tps)
 (conclusion THM500-conc
  (all-types b a (forall (lam (|g| (a b)) (forall (lam (|y| b) (forall (lam (|x| b) (= (IN-IMAGE |g| (SETPAIR |x| |y|)) (SETPAIR (|g| |x|) (|g| |y|)))))))))))
 (help "Theorem relating SETPAIR to %"))

(th~defproblem THM47
 (in tps)
 (conclusion THM47-conc
 (forall (lam (X i) (forall (lam (Y i) (equiv (= X Y) (forall (lam (R (o i i)) (implies (forall (lam (Z i) (R Z Z))) (R X Y))))))))))
 (help "Shows equivalence of two definitions of ="))

(th~defproblem THM40
 (in tps)
 (conclusion THM40-conc
  (all-types b c (forall (lam (G (c b)) (forall (lam (F (b c)) (implies (and (and FINITE1B (INJECTIVE F)) (INJECTIVE G)) FINITE1C)))))))
 (help "04-JUN-78 00:33:16 GOT THE 11 CLAUSES  
 
DATES  "))

(th~defproblem THM30
 (in tps)
 (conclusion THM30-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (R (o a)) (equiv (SUBSET R S) (forall (lam (F (a a)) (SUBSET (IN-IMAGE F R) (IN-IMAGE F S)))))))))))
 (help ""))

(th~defproblem THM271
 (in tps)
 (conclusion THM271-conc
 (forall (lam (P (o i)) (forall (lam (N (o i i)) (forall (lam (M (o i i)) (implies (and (and (forall (lam (|x| i) (implies (exists (lam (|y| i) (or (M |x| |y|) (N |x| |y|)))) (P |x|)))) (forall (lam (|w| i) (exists (lam (|u| i) (or (forall (lam (|v| i) (M |u| |v|))) (N |u| |w|))))))) (forall (lam (|w| i) (forall (lam (|z| i) (implies (or (M |w| |z|) (N |w| |z|)) (or (or (M |z| |w|) (N |z| |w|)) (or (M |z| |z|) (N |z| |z|))))))))) (forall (lam (|z| i) (P |z|)))))))))))
 (help "Combination of X2133 and X2134"))

(th~defproblem THM203
 (in tps)
 (conclusion THM203-conc
  (all-types a (forall (lam (|r| (o a a)) (forall (lam (T (o a a (o a a))) (implies (and (REFLEXIVE (T |r|)) (and (TRANSITIVE (T |r|)) (forall (lam (|x| a) (forall (lam (|y| a) (implies (|r| |x| |y|) (T |r| |x| |y|)))))))) (forall (lam (|x| a) (forall (lam (|y| a) (implies (TRCL-BBP |r| |x| |y|) (T |r| |x| |y|)))))))))))))
 (help "B&B-P's defn of TRCL is the minimal transitive reflexive relation containing r"))

(th~defproblem THM177
 (in tps)
 (conclusion THM177-conc
  (all-types a b (forall (lam (|x| b) (forall (lam (|y| a) (forall (lam (|k| (o a b)) (implies (|k| |x| |y|) (= (ADDPAIR (SUBTRACTPAIR |k| |x| |y|) |x| |y|) |k|))))))))))
 (help "Proof needs extensionality for relations (THM174)."))

(th~defproblem THM132
 (in tps)
 (conclusion THM132-conc
  (all-types a (forall (lam (|h| (a a)) (forall (lam (|s| (o a)) (forall (lam (|f| (a a)) (implies (HOM |h| |s| |f| |s| |f|) (forall (lam (|x| a) (implies (|s| |x|) (= (COMPOSE |h| |h| (|f| |x|)) (|f| (COMPOSE |h| |h| |x|)))))))))))))))
 (help "Special case of THM131D"))

(th~defproblem THM104-TPS2
 (in tps)
 (conclusion THM104-TPS2-conc
  (all-types a (forall (lam (X a) (forall (lam (U a) (implies (= (UNITSET X) (UNITSET U)) (= X U))))))))
 (help "NOT PROVABLE AUTOMATICALLY BY TPS  
ALTHOUGH CRUCIAL INSTANTIATION TERM CONTAINS NO CONNECTIVES OR QUANTIFIERS  
 
REMARK  "))

(th~defproblem FN-THM-1
 (in tps)
 (conclusion FN-THM-1-conc
  (all-types c a b (forall (lam (F (b a)) (forall (lam (G (c b)) (implies (and (INJECTIVE F) (INJECTIVE G)) (INJECTIVE (COMPOSE G F)))))))))
 (help ""))

(th~defproblem X6007A
 (in tps)
 (conclusion X6007A-conc
 (= (lam (N (o (o i))) (forall (lam (P (o (o (o i)))) (implies (and (P ZERO) (forall (lam (X (o (o i))) (implies (P X) (P (SUCC X)))))) (P N))))) (SETINTERSECT (lam (P (o (o (o i)))) (and (P ZERO) (forall (lam (X (o (o i))) (implies (P X) (P (SUCC X))))))))))
 (help ""))

(th~defproblem TTTP2129
 (in tps)
 (conclusion TTTP2129-conc
 (forall (lam (Q (o i i)) (forall (lam (|y| i) (forall (lam (P (o i i)) (equiv (forall (lam (|x| i) (and (P |x| |y|) (Q |x| |y|)))) (and (forall (lam (|x| i) (P |x| |y|))) (forall (lam (|x| i) (Q |x| |y|))))))))))))
 (help ""))

(th~defproblem THM82A
 (in tps)
 (conclusion THM82A-conc
 (forall (lam (F (o i i)) (forall (lam (D (o i i i)) (forall (lam (S (o i i)) (implies (and (and (forall (lam (X i) (exists (lam (Y i) (F X Y))))) (exists (lam (X i) (forall (lam (E i) (exists (lam (N i) (forall (lam (W i) (implies (S N W) (D W X E))))))))))) (forall (lam (G i) (exists (lam (H i) (forall (lam (X1 i) (forall (lam (X2 i) (implies (D X1 X2 H) (forall (lam (Y1 i) (forall (lam (Y2 i) (implies (and (F X1 Y1) (F X2 Y2)) (D Y1 Y2 G)))))))))))))))) (exists (lam (Y i) (forall (lam (E i) (exists (lam (M i) (forall (lam (W i) (implies (S M W) (forall (lam (Z i) (implies (F W Z) (D Z Y E)))))))))))))))))))))
 (help ""))

(th~defproblem THM82
 (in tps)
 (conclusion THM82-conc
 (forall (lam (F (o i i)) (forall (lam (D (o i i i)) (forall (lam (S (o i i)) (implies (and (and (forall (lam (|x| i) (exists (lam (|y| i) (F |x| |y|))))) (exists (lam (|x| i) (forall (lam (|e| i) (exists (lam (|n| i) (forall (lam (|w| i) (implies (S |n| |w|) (D |w| |x| |e|))))))))))) (forall (lam (EPSILON i) (exists (lam (DELTA i) (forall (lam (|x1| i) (forall (lam (|x2| i) (implies (D |x1| |x2| DELTA) (forall (lam (|y1| i) (forall (lam (|y2| i) (implies (and (F |x1| |y1|) (F |x2| |y2|)) (D |y1| |y2| EPSILON)))))))))))))))) (exists (lam (|y| i) (forall (lam (|e| i) (exists (lam (|m| i) (forall (lam (|w| i) (implies (S |m| |w|) (forall (lam (|z| i) (implies (F |w| |z|) (D |z| |y| |e|)))))))))))))))))))))
 (help "1981-NOV-07 GOT MATING AUTOMATICALLY  
RUNTIME 9.006 CONS COUNT 46029  
SYSTEM System III  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
1981-NOV-16 PROOF CONSTRUCTED AUTOMATICALLY FROM PLAN  
RUNTIME 75.622635 CONS COUNT 133134  
 
1981-DEC-14 GOT MATING AUTOMATICALLY ON NEW RUN WITH DIFFERENT VARIABLE NAMES  
RUNTIME 9.09983 CONS COUNT 46047  
SYSTEM System III  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
1982-FEB-04 RUNTIME 109.87219 CONSCOUNT 615453  
SYSTEM TPS1  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
*** COMPLETELY AUTOMATIC TPS PROOF  
***  
1982 FEB 27 COMPLETE PROOF IN TPS1  
OUTLINE BUILD AUTOMATICALLY  
DAM  
 
DATES  "))

(th~defproblem THM61
 (in tps)
 (conclusion THM61-conc
  (all-types a (forall (lam (Z (o a)) (forall (lam (W (o (o a))) (EQUIVS (INTERSECT (SETUNION W) Z) (SETUNION (IN-IMAGE (lam (X (o a)) (INTERSECT Z X)) W)))))))))
 (help ""))

(th~defproblem THM301A
 (in tps)
 (conclusion THM301A-conc
 (forall (lam (HALF (o i i)) (forall (lam (DOUBLE (o i i)) (forall (lam (S (i i)) (forall (lam (|0| i) (implies (and (and (and (forall (lam (|u| i) (forall (lam (|v| i) (equiv (DOUBLE |u| |v|) (forall (lam (Q (o i i)) (implies (and (Q |0| |0|) (forall (lam (|x| i) (forall (lam (|y| i) (implies (Q |x| |y|) (Q (S |x|) (S (S |y|))))))))) (Q |u| |v|))))))))) (HALF |0| |0|)) (HALF (S |0|) |0|)) (forall (lam (|x| i) (forall (lam (|y| i) (implies (HALF |x| |y|) (HALF (S (S |x|)) (S |y|)))))))) (forall (lam (|u| i) (forall (lam (|v| i) (implies (DOUBLE |u| |v|) (HALF |v| |u|))))))))))))))))
 (help "DOUBLEuv means that 2u = v; HALFuv means that the greatest integer in u/2 is v."))

(th~defproblem THM270
 (in tps)
 (conclusion THM270-conc
  (all-types a b c (forall (lam (* (c c c)) (forall (lam (*^0 (b b b)) (forall (lam (*^1 (a a a)) (forall (lam (|f| (b a)) (forall (lam (|g| (c a)) (forall (lam (|h| (c b)) (implies (and (and (and (forall (lam (|x| a) (= (|h| (|f| |x|)) (|g| |x|)))) (forall (lam (|y| b) (exists (lam (|x| a) (= (|f| |x|) |y|)))))) (GEN-HOM-TPS *^1 *^0 |f|)) (GEN-HOM-TPS *^1 * |g|)) (GEN-HOM-TPS *^0 * |h|))))))))))))))))
 (help ""))

(th~defproblem THM140
 (in tps)
 (conclusion THM140-conc
 (forall (lam (DOUBLE (o i i)) (forall (lam (|0| i) (forall (lam (S (i i)) (forall (lam (\|0\| i) (implies (and (and (IND-TPS \|0\| S) (DOUBLE |0| |0|)) (forall (lam (|x| i) (forall (lam (|y| i) (implies (DOUBLE |x| |y|) (DOUBLE (S |x|) (S (S |y|))))))))) (forall (lam (|x| i) (exists (lam (|y| i) (DOUBLE |x| |y|)))))))))))))))
 (help "DOUBLE x y means 2x = y. DOUBLE is a total function.
Suggested by Pfenning."))

(th~defproblem THM129A
 (in tps)
 (conclusion THM129A-conc
 (forall (lam (S ((o (o i)) (o (o i)))) (forall (lam (\\\|0\\\| (o (o i))) (implies (and (and INDUCTION (forall (lam (|x| (o (o i))) (= (PLUS-TPS \\\|0\\\| S ZERO |x|) |x|)))) (forall (lam (|x| (o (o i))) (forall (lam (|y| (o (o i))) (forall (lam (|z| (o (o i))) (implies (= (PLUS-TPS \\\|0\\\| S |y| |x|) |z|) (= (PLUS-TPS \\\|0\\\| S (SUCC |y|) |x|) (SUCC |z|)))))))))) (forall (lam (|y| (o (o i))) (forall (lam (|x| (o (o i))) (exists (lam (|z| (o (o i))) (= (PLUS-TPS \\\|0\\\| S |y| |x|) |z|)))))))))))))
 (help "THM129 with the real +, ZERO and SUCC"))

(th~defproblem THM120C
 (in tps)
 (conclusion THM120C-conc
 (exists (lam (|r^28| (o i (o i) (o i))) (exists (lam (|r^27| (o i (o i) (o i))) (and (and (not ((lam (|w^1| (o i)) (lam (|w^2| (o i)) (forall (lam (|w^8| i) ((lam (|w^3| (o i)) (lam (|w^4| (o i)) (lam (|w^5| i) (or (|r^27| |w^3| |w^4| |w^5|) (|r^28| |w^3| |w^4| |w^5|))))) |w^1| |w^2| |w^8|))))) (lam (|x| i) true) (lam (|x| i) false))) (REFLEXIVE (lam (|w^1| (o i)) (lam (|w^2| (o i)) (forall (lam (|w^9| i) ((lam (|w^3| (o i)) (lam (|w^4| (o i)) (lam (|w^5| i) (or (|r^27| |w^3| |w^4| |w^5|) (|r^28| |w^3| |w^4| |w^5|))))) |w^1| |w^2| |w^9|))))))) (TRANSITIVE (lam (|w^1| (o i)) (lam (|w^2| (o i)) (forall (lam (|w^10| i) ((lam (|w^3| (o i)) (lam (|w^4| (o i)) (lam (|w^5| i) (or (|r^27| |w^3| |w^4| |w^5|) (|r^28| |w^3| |w^4| |w^5|))))) |w^1| |w^2| |w^10|))))))))))))
 (help "variant of thm120b in which the proper primitive substitutions have already been made."))

(th~defproblem THM112A
 (in tps)
 (conclusion THM112A-conc
 (forall (lam (P (o i)) (exists (lam (M (o (i i))) (and (M (lam (|x| i) |x|)) (forall (lam (G (i i)) (forall (lam (H (i i)) (implies (and (M G) (M H)) (and (M (COMPOSE G H)) (forall (lam (Y i) (implies (P Y) (P (G Y)))))))))))))))))
 (help "Like THM112, but excludes a trivial proof"))

(th~defproblem THM11
 (in tps)
 (conclusion THM11-conc
  (all-types b a (forall (lam (|f| (a b)) (forall (lam (|y| (o b)) (forall (lam (|x| (o b)) (SUBSET (IN-IMAGE |f| (INTERSECT |x| |y|)) (INTERSECT (IN-IMAGE |f| |x|) (IN-IMAGE |f| |y|)))))))))))
 (help ""))

(th~defproblem PA-THM2
 (in tps)
 (conclusion PA-THM2-conc
  (all-types n (forall (lam (|0| n) (forall (lam (S (n n)) (forall (lam (\|0\| n) (forall (lam (+ (n n n)) (implies (and (and (PA-1-TPS + \|0\|) (PA-2-TPS + S)) (PA-IND-EQ-TPS \|0\| S)) (forall (lam (|x| n) (= (+ |x| |0|) (+ |0| |x|)))))))))))))))
 (help ""))

(th~defproblem CT29
 (in tps)
 (conclusion CT29-conc
  (all-types a (not (exists (lam (|g| (o a a)) (forall (lam (|f| (o a)) (exists (lam (|j| a) (forall (lam (|p| (o (o a))) (implies (|p| (|g| |j|)) (|p| |f|)))))))))))))
 (help "cantorsatz[D[D[D[D[D[D[D[D[C[C[C[C[C[C[C[C[C[C, Leibnizdefinition"))

(th~defproblem X6007
 (in tps)
 (conclusion X6007-conc
 (forall (lam (S ((o (o i)) (o (o i)))) (forall (lam (|0| (o (o i))) (= (lam (N (o (o i))) (forall (lam (P (o (o (o i)))) (implies (and (P |0|) (forall (lam (X (o (o i))) (implies (P X) (P (S X)))))) (P N))))) (SETINTERSECT (lam (P (o (o (o i)))) (and (P |0|) (forall (lam (X (o (o i))) (implies (P X) (P (S X))))))))))))))
 (help ""))

(th~defproblem THM89A
 (in tps)
 (conclusion THM89A-conc
  (all-types a b (forall (lam (G (b b)) (forall (lam (F (b b)) (forall (lam (A^0 (a b)) (forall (lam (< (o a a)) (implies (and (TRANSITIVE (LESS-INFIX-TPS <)) (and (forall (lam (X b) (LESS-INFIX-TPS < (A^0 X) (A^0 (F X))))) (= G (lam (Z b) (F (F Z)))))) (forall (lam (Y b) (LESS-INFIX-TPS < (A^0 Y) (A^0 (G Y))))))))))))))))
 (help "(THE AGE OF A PERSON IS LTHE PATERNAL GRANDFATHER OF THE PERSON)"))

(th~defproblem THM83
 (in tps)
 (conclusion THM83-conc
 (forall (lam (P (o i)) (forall (lam (W i) (forall (lam (R (o i i)) (implies (forall (lam (X i) (exists (lam (Y i) (implies (P X) (forall (lam (Z i) (and (R X Y) (P Z))))))))) (exists (lam (U i) (forall (lam (V i) (implies (P V) (R W U))))))))))))))
 (help "1982-JAN-11 COMPLETE AUTOMATIC PROOF  
TIMING SPOILED BY BUG  
 
 
RUNTIME 178.5886 CONS COUNT 405955  
SYSTEM System III  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
1982 FEB 27 COMPLETE PROOF IN TPS1  
OUTLINE BUILT AUTOMATICALLY  
DAM  
 
DATES  "))

(th~defproblem THM71
 (in tps)
 (conclusion THM71-conc
 (forall (lam (P (o i)) (forall (lam (G (i i)) (forall (lam (LT (o i i)) (implies (and (forall (lam (X i) (exists (lam (Y i) (and (LT X (G Y)) (and (or (not (P X)) (not (P (G Y)))) (or (P X) (P (G Y))))))))) (forall (lam (U i) (forall (lam (V i) (forall (lam (W i) (implies (and (LT U V) (LT V W)) (LT U W))))))))) (forall (lam (X i) (implies (P X) (exists (lam (Z i) (and (LT X (G Z)) (P (G Z))))))))))))))))
 (help "1980-MAY-19 RUNTIME 34.481586 CONS COUNT 209273  
SYSTEM System III  
GENVPATHMATES GENVPATHMATES1  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
*** COMPLETE MATING PROOF WITH INTERACTIVE REPLICATION  
***  
1982 FEB 27 WRONG PROOF IN TPS1  
QUANTIFIERS WERE DUPLICATED  
SEARCH GOT LOST  
DAM  
 
DATES  "))

(th~defproblem THM579
 (in tps)
 (conclusion THM579-conc
 (forall (lam (\|0\| i) (forall (lam (S (i i)) (forall (lam (|0| i) (forall (lam (D (o i i)) (implies (and (IND-TPS \|0\| S) (and (D |0| |0|) (forall (lam (|x| i) (forall (lam (|y| i) (implies (D |x| |y|) (D (S |x|) (S (S |y|)))))))))) (forall (lam (|x| i) (exists (lam (|y| i) (D |x| |y|)))))))))))))))
 (help "Existence of doubles of naturals.  Proof by induction (setsub found by unification)"))

(th~defproblem THM533
 (in tps)
 (conclusion THM533-conc
  (all-types a (implies (forall (lam (|r| (o a (o a))) (exists (lam (|g| (a (o a))) (forall (lam (|x| (o a)) (implies (exists (lam (|y| a) (|r| |x| |y|))) (|r| |x| (|g| |x|))))))))) (forall (lam (|s| (o (o a))) (implies (forall (lam (X (o a)) (implies (|s| X) (exists (lam (|t| a) (X |t|)))))) (exists (lam (|f| (a (o a))) (forall (lam (X (o a)) (implies (|s| X) (X (|f| X)))))))))))))
 (help "AC3 => AC1 from Rubin and Rubin."))

(th~defproblem THM178
 (in tps)
 (conclusion THM178-conc
 (forall (lam (INFIX-LESS (o i i)) (equiv (forall (lam (P (o i)) (implies (forall (lam (|n| i) (implies (forall (lam (|m| i) (implies (INFIX-LESS |m| |n|) (P |m|)))) (P |n|)))) (forall (lam (|n| i) (P |n|)))))) (forall (lam (P (o i)) (implies (exists (lam (|n| i) (P |n|))) (exists (lam (|n| i) (and (P |n|) (forall (lam (|m| i) (implies (INFIX-LESS |m| |n|) (not (P |m|)))))))))))))))
 (help "Every nonempty set has a minimal element
is equivalent to the assertion that complete induction holds.
Suggested by John Harrison 1997 Jan 5."))

(th~defproblem THM15
 (in tps)
 (conclusion THM15-conc
 (forall (lam (F (i i)) (exists (lam (G (i i)) (implies (and (ITERATES G F) (EXISTS1B (lam (X i) (= (G X) X)))) (exists (lam (Y i) (= (F Y) Y)))))))))
 (help ""))

(th~defproblem THM144
 (in tps)
 (conclusion THM144-conc
 (forall (lam (|h| (i (o i))) (forall (lam (|d| (o i)) (implies (= |d| (IN-IMAGE |h| (lam (|s| (o i)) (not (|s| (|h| |s|)))))) (|d| (|h| |d|))))))))
 (help "Essentially a lemma for the Injective Cantor Theorem X5309.
Also see X5309a, THM118, THM143, THM143A, and THM144A."))

(th~defproblem THM143B
 (in tps)
 (conclusion THM143B-conc
 (forall (lam (|h| (i (o i))) (implies (forall (lam (|p| (o i)) (forall (lam (|q| (o i)) (implies (= (|h| |p|) (|h| |q|)) (= |p| |q|)))))) (not (IDIAG |h| (|h| (IDIAG |h|))))))))
 (help "Essentially a lemma for the Injective Cantor Theorem X5309.
Also see X5309a, THM118, THM143, and THM143A"))

(th~defproblem THM136
 (in tps)
 (conclusion THM136-conc
  (all-types a (forall (lam (|r| (o a a)) (TRANSITIVE (TRANSITIVE-CLOSURE |r|))))))
 (help "The transitive closure of a relation is transitive."))

(th~defproblem THM133
 (in tps)
 (conclusion THM133-conc
  (all-types a g b (forall (lam (|h1| (b g)) (forall (lam (|h2| (a b)) (forall (lam (|f1| (g g g)) (forall (lam (|f2| (b b b)) (forall (lam (|f3| (a a a)) (implies (and (HOMOM2 |h1| |f1| |f2|) (HOMOM2 |h2| |f2| |f3|)) (HOMOM2 (COMPOSE |h2| |h1|) |f1| |f3|))))))))))))))
 (help "The composition of homomorphisms of binary  operators is a 
homomorphism. Suggested by Boyer et al JAR 2 page 284. 
Simplified version of THM126"))

(th~defproblem THM129
 (in tps)
 (conclusion THM129-conc
 (forall (lam (+ (o i i i)) (forall (lam (|0| i) (forall (lam (S (i i)) (forall (lam (\|0\| i) (implies (and (and (IND-TPS \|0\| S) (forall (lam (|x| i) (+ |0| |x| |x|)))) (forall (lam (|x| i) (forall (lam (|y| i) (forall (lam (|z| i) (implies (+ |y| |x| |z|) (+ (S |y|) |x| (S |z|)))))))))) (forall (lam (|y| i) (forall (lam (|x| i) (exists (lam (|z| i) (+ |y| |x| |z|)))))))))))))))))
 (help "Induction theorem for addition. +xyz means [x+y=z]. Previously called PRED-EX1."))

(th~defproblem THM112C
 (in tps)
 (conclusion THM112C-conc
 (forall (lam (P (o i)) (exists (lam (|m^9| (o i (i i))) (exists (lam (|m^10| (o i (i i))) (and (forall (lam (|w^1| i) (or (|m^9| (lam (|x| i) |x|) |w^1|) (|m^10| (lam (|x| i) |x|) |w^1|)))) (forall (lam (G (i i)) (forall (lam (H (i i)) (implies (and (and (forall (lam (|w^1| i) (or (|m^9| G |w^1|) (|m^10| G |w^1|)))) (forall (lam (|w^1| i) (or (|m^9| G |w^1|) (|m^10| G |w^1|))))) (forall (lam (|w^1| i) (or (|m^9| H |w^1|) (|m^10| H |w^1|))))) (and (forall (lam (|w^1| i) (or (|m^9| (COMPOSE G H) |w^1|) (|m^10| (COMPOSE G H) |w^1|)))) (forall (lam (Y i) (implies (P Y) (P (G Y)))))))))))))))))))
 (help ""))

(th~defproblem THM112B
 (in tps)
 (conclusion THM112B-conc
 (forall (lam (P (o i)) (exists (lam (|m^9| (o i (i i))) (exists (lam (|m^10| (o i (i i))) (and ((lam (|w^1| (i i)) (forall (lam (|w^11| i) (or (|m^9| |w^1| |w^11|) (|m^10| |w^1| |w^11|))))) (lam (|x| i) |x|)) (forall (lam (G (i i)) (forall (lam (H (i i)) (implies (and ((lam (|w^1| (i i)) (forall (lam (|w^12| i) (or (|m^9| |w^1| |w^12|) (|m^10| |w^1| |w^12|))))) G) ((lam (|w^1| (i i)) (forall (lam (|w^13| i) (or (|m^9| |w^1| |w^13|) (|m^10| |w^1| |w^13|))))) H)) (and ((lam (|w^1| (i i)) (forall (lam (|w^14| i) (or (|m^9| |w^1| |w^14|) (|m^10| |w^1| |w^14|))))) (COMPOSE G H)) (forall (lam (Y i) (implies (P Y) (P (G Y)))))))))))))))))))
 (help "Variant of THM112a with primsubs applied."))

(th~defproblem THM104
 (in tps)
 (conclusion THM104-conc
  (all-types a (forall (lam (X a) (forall (lam (Z a) (implies (= (UNITSET X) (UNITSET Z)) (= X Z))))))))
 (help "Example from 1989 JAR paper CHOL. Unitset is injective. Like THM7."))

(th~defproblem MODULAR-THM
 (in tps)
 (conclusion MODULAR-THM-conc
  (all-types a (forall (lam (JOIN (a a a)) (forall (lam (MEET (a a a)) (implies (and (LATTICE JOIN MEET) (and (DISTRIBUTIVE JOIN MEET) (DISTRIBUTIVE MEET JOIN))) (MODULAR MEET JOIN))))))))
 (help "Every distributive lattice is modular."))

(th~defproblem LING2
 (in tps)
 (conclusion LING2-conc
 (forall (lam (J i) (forall (lam (UNIQUE (o i)) (forall (lam (S i) (forall (lam (P i) (forall (lam (LIKE (o i i)) (implies (and (forall (lam (X i) (implies (UNIQUE X) (forall (lam (Z i) (= X Z)))))) (UNIQUE S)) (exists (lam (|an| (o i)) (and (= (|an| P) (LIKE P S)) (= (|an| J) (exists (lam (X i) (and (UNIQUE X) (LIKE J X))))))))))))))))))))
 (help ""))

(th~defproblem BLEDSOE7A
 (in tps)
 (conclusion BLEDSOE7A-conc
 (forall (lam (P (o (o (o i)))) (implies (P ONE) (exists (lam (|x| (o (o i))) (and (and (LESS= ZERO |x|) (LESS= |x| (SUCC ONE))) (P |x|))))))))
 (help "corrected version of BLEDSOE7"))

(th~defproblem X5305B
 (in tps)
 (conclusion X5305B-conc
  (all-types a (forall (lam (|s| (o a)) (not (exists (lam (|g| (o a a)) (SUBSET (POWERSET |s|) (IN-IMAGE |g| |s|)))))))))
 (help "Cantor's Theorem for Sets stated in terms of powerset and image;
for any set s, the image of s cannot include the power set of s"))

(th~defproblem TRANS-IND
 (in tps)
 (conclusion TRANS-IND-conc
  (all-types a (forall (lam (|r| (o a a)) (forall (lam (P (o a)) (implies (and (forall (lam (|s| (o a)) (implies (exists (lam (|z| a) (|s| |z|))) (exists (lam (|y| a) (and (|s| |y|) (forall (lam (|w| a) (implies (|r| |w| |y|) (not (|s| |w|))))))))))) (forall (lam (|x| a) (implies (forall (lam (|y| a) (implies (|r| |y| |x|) (P |y|)))) (P |x|))))) (forall (lam (|x| a) (P |x|))))))))))
 (help "transfinite induction theorem, from Bailin and Barker-Plummer"))

(th~defproblem THM89
 (in tps)
 (conclusion THM89-conc
  (all-types a b (forall (lam (G (b b)) (forall (lam (F (b b)) (forall (lam (A^1 (a b)) (forall (lam (L (o a a)) (implies (and (TRANSITIVE L) (and (forall (lam (X b) (L (A^1 X) (A^1 (F X))))) (= G (lam (Z b) (F (F Z)))))) (forall (lam (Y b) (L (A^1 Y) (A^1 (G Y))))))))))))))))
 (help "THE AGE OF A PERSON IS LESS THAN THE AGE OF THE PARENTAL GRANDFATHER OF THE PERSON  
PROBLEM SUGGESTED BY DOUG SKUCE"))

(th~defproblem THM88
 (in tps)
 (conclusion THM88-conc
  (all-types a (forall (lam (U (o a)) (forall (lam (V (o a)) (implies (SUBSET U V) (SUBSET (POWERSET (POWERSET U)) (POWERSET (POWERSET V))))))))))
 (help ""))

(th~defproblem THM85
 (in tps)
 (conclusion THM85-conc
 (forall (lam (P (o i i)) (forall (lam (|h| (i i)) (forall (lam (|g| (i i)) (exists (lam (|x| i) (exists (lam (|f| (i i)) (forall (lam (|y| i) (implies (and (forall (lam (|z| i) (P |z| (|f| |x|)))) (P |x| |y|)) (P |y| (|g| (|h| |y|)))))))))))))))))
 (help ""))

(th~defproblem THM73
 (in tps)
 (conclusion THM73-conc
 (forall (lam (P (o i)) (forall (lam (L (o i i)) (forall (lam (H (i i)) (forall (lam (G (i i)) (implies (and (forall (lam (X i) (exists (lam (Y i) (implies (P X) (and (L X (G (H Y))) (P Y))))))) (forall (lam (W i) (implies (P W) (and (P (G W)) (P (H W))))))) (forall (lam (X i) (implies (P X) (exists (lam (Z i) (and (L X Z) (P Z)))))))))))))))))
 (help "DIFFERS FROM THM72 ONLY BY A VARIABLE CHANGE  
SIMPLE THEOREM BUT REQUIRES QUANTIFIER DUPLICATION"))

(th~defproblem THM500C-WFF
 (in tps)
 (conclusion THM500C-WFF-conc
  (all-types b a (forall (lam (|z| a) (forall (lam (|y| b) (forall (lam (|x| b) (forall (lam (|g| (a b)) (implies (SETPAIR (|g| |x|) (|g| |y|) |z|) (IN-IMAGE |g| (SETPAIR |x| |y|) |z|))))))))))))
 (help "This caused a problem translating to nat ded proof"))

(th~defproblem THM48
 (in tps)
 (conclusion THM48-conc
  (all-types c b a (forall (lam (F (a b)) (forall (lam (G (b c)) (implies (and (INJECTIVE F) (INJECTIVE G)) (INJECTIVE (COMPOSE F G)))))))))
 (help "The composition of injective functions is injective"))

(th~defproblem THM41
 (in tps)
 (conclusion THM41-conc
  (all-types c b (forall (lam (D (b c)) (forall (lam (F (c b)) (implies (and (and FINITE1B (INJECTIVE F)) (INJECTIVE D)) (SURJECTIVE D))))))))
 (help "THIS IS ESSENTIALLY HUET'S  
FORMULATION OF THE CHESSBOARD PROBLEM "))

(th~defproblem THM275
 (in tps)
 (conclusion THM275-conc
  (all-types a (forall (lam (|r| (o a a)) (exists (lam (|p| (o a a)) (and (SUBRELATION |r| |p|) (TRANSITIVE |p|))))))))
 (help ""))

(th~defproblem THM269
 (in tps)
 (conclusion THM269-conc
  (all-types a (forall (lam (|w| (o a)) (forall (lam (|y| (o a)) (forall (lam (|z| (o a)) (implies (and (SUBSET (INTERSECT |w| (COMPLEMENT-TPS |z|)) |y|) (= (SETDIFF |z| |y|) (lam (|x| a) false))) (SUBSET |w| |y|))))))))))
 (help "Example for CADE-15. Variant of THM108"))

(th~defproblem THM210
 (in tps)
 (conclusion THM210-conc
 (forall (lam (OPEN-WINDOW o) (forall (lam (HEADACHE o) (forall (lam (GUSTY o) (forall (lam (SMOKES o) (forall (lam (RHEUMATIC o) (forall (lam (FIRE o) (forall (lam (SMOKE o) (forall (lam (OPEN-DOOR o) (forall (lam (EAST o) (forall (lam (SUNSHINE o) (forall (lam (FLUTE o) (forall (lam (FOGGY o) (forall (lam (COLD o) (implies (and (and (and (and (and (and (and (and (and (and (implies EAST SUNSHINE) (implies (and COLD FOGGY) FLUTE)) (implies (and FIRE SMOKE) OPEN-DOOR)) (implies (and COLD RHEUMATIC) FIRE)) (implies (and EAST GUSTY) SMOKES)) (implies OPEN-DOOR (not HEADACHE))) (implies FOGGY (not OPEN-WINDOW))) (implies (and (and (not GUSTY) FIRE) (not OPEN-DOOR)) (not RHEUMATIC))) (implies SUNSHINE FOGGY)) (implies FLUTE (not OPEN-DOOR))) (implies (and FOGGY EAST) RHEUMATIC)) (implies EAST (not OPEN-WINDOW))))))))))))))))))))))))))))))
 (help "Lewis Carroll's problem of the winds and windows; from the Ninth Paper on Logic, November 1892."))

(th~defproblem THM196
 (in tps)
 (conclusion THM196-conc
 (forall (lam (|a| i) (forall (lam (|b| i) (forall (lam (|h| (i i)) (implies (and (and (= (|h| |a|) |a|) (not (= (|h| |b|) |a|))) (forall (lam (|f| (i i)) (forall (lam (|g| (i i)) (implies (forall (lam (|x| i) (= (|f| |x|) (|g| |x|)))) (= |f| |g|))))))) (not (forall (lam (|j| (i i)) (forall (lam (|k| (i i)) (implies (ITERATE+ |j| (COMPOSE |k| |j|)) (ITERATE+ |j| |k|)))))))))))))))
 (help "It is not true that if [k COMPOSE j] is an iterate of j, 
then k must be an iterate of j, 
provided we assume extensionality and
the existence of the described function h
(which can be proved if we have distinct elements a and b
and descriptions)."))

(th~defproblem THM176
 (in tps)
 (conclusion THM176-conc
  (all-types a b (forall (lam (|x| b) (forall (lam (|y| a) (forall (lam (|s| (o a b)) (forall (lam (|k| (o a b)) (implies (SUBRELATION |k| (ADDPAIR |s| |x| |y|)) (SUBRELATION (SUBTRACTPAIR |k| |x| |y|) |s|))))))))))))
 (help ""))

(th~defproblem THM173
 (in tps)
 (conclusion THM173-conc
  (all-types a b (forall (lam (|x| b) (forall (lam (|y| a) (forall (lam (|s| (o a b)) (forall (lam (|k| (o a b)) (implies (and (SUBRELATION |k| (ADDPAIR |s| |x| |y|)) (not (|k| |x| |y|))) (SUBRELATION |k| |s|))))))))))))
 (help ""))

(th~defproblem THM164
 (in tps)
 (conclusion THM164-conc
  (all-types a (forall (lam (|r| (o a)) (forall (lam (|x| a) (implies (FINITE1 |r|) (FINITE1 (ADD1 |r| |x|)))))))))
 (help "Direct consequence of the definition of FINITE1"))

(th~defproblem THM14
 (in tps)
 (conclusion THM14-conc
 (INJECTIVE (lam (X i) (lam (Y i) (= Y X)))))
 (help "15-APR-77 23:26:50 22-APR-77 16:40:39 26-APR-77 20:59:33  
DATES  "))

(th~defproblem THM130-B
 (in tps)
 (conclusion THM130-B-conc
 (forall (lam (|r| (o (o (o i)) (o (o i)))) (implies (and (|r| ZERO ZERO) (forall (lam (|x| (o (o i))) (forall (lam (|y| (o (o i))) (implies (|r| |x| |y|) (|r| (SUCC |x|) (SUCC |y|)))))))) (forall (lam (|x| (o (o i))) (implies (NAT |x|) (exists (lam (|y| (o (o i))) (|r| |x| |y|))))))))))
 (help ""))

(th~defproblem T146A
 (in tps)
 (conclusion T146A-conc
 (forall (lam (|r| (o i i)) (forall (lam (|x| i) (forall (lam (|y| i) (implies (forall (lam (|p| (o i i)) (implies (and (SUBRELATION |r| |p|) (forall (lam (|u| i) (forall (lam (|v| i) (forall (lam (|w| i) (implies (and (|p| |u| |v|) (|r| |v| |w|)) (|p| |u| |w|))))))))) (|p| |x| |y|)))) (forall (lam (|p| (o i i)) (implies (and (SUBRELATION |r| |p|) (TRANSITIVE |p|)) (|p| |x| |y|))))))))))))
 (help "Half of T146"))

(th~defproblem FN-THM-3
 (in tps)
 (conclusion FN-THM-3-conc
  (all-types c a b (forall (lam (F (b a)) (forall (lam (G (c b)) (implies (INJECTIVE (COMPOSE G F)) (INJECTIVE F))))))))
 (help ""))

(th~defproblem FN-THM-2
 (in tps)
 (conclusion FN-THM-2-conc
  (all-types c a b (forall (lam (F (b a)) (forall (lam (G (c b)) (implies (and (SURJECTIVE F) (SURJECTIVE G)) (SURJECTIVE (COMPOSE G F)))))))))
 (help ""))

(th~defproblem EQP1-1A
 (in tps)
 (conclusion EQP1-1A-conc
  (all-types a (REFLEXIVE (lam (|x| (o a)) (lam (|y| (o a)) (EQP1 |x| |y|))))))
 (help ""))

(th~defproblem DISCRETE-TOPOLOGY
 (in tps)
 (conclusion DISCRETE-TOPOLOGY-conc
  (all-types a (TOPOLOGY (lam (X (o a)) true))))
 (help "The discrete topology really is a topology."))

(th~defproblem CLOSURE-THM0
 (in tps)
 (conclusion CLOSURE-THM0-conc
  (all-types b (forall (lam (S (o (o b))) (implies (TOPOLOGY S) (forall (lam (W (o b)) (SUBSET W (TOP-CLOSURE S W)))))))))
 (help ""))

(th~defproblem X5305A
 (in tps)
 (conclusion X5305A-conc
  (all-types a (forall (lam (|s| (o a)) (not (exists (lam (|g| (o a a)) (forall (lam (|f| (o a)) (implies (IN |f| (POWERSET |s|)) (exists (lam (|j| a) (and (IN |j| |s|) (= (|g| |j|) |f|))))))))))))))
 (help ""))

(th~defproblem TTTP6101
 (in tps)
 (conclusion TTTP6101-conc
 (forall (lam (|x| (o (o i))) (implies (NAT |x|) (NAT (SUCC |x|))))))
 (help ""))

(th~defproblem THM91A
 (in tps)
 (conclusion THM91A-conc
  (all-types a (forall (lam (S (o a)) (SUBSET S (SETUNION (lam (P (o a)) (SUBSET P S))))))))
 (help "Lemma for THM91."))

(th~defproblem THM89B
 (in tps)
 (conclusion THM89B-conc
  (all-types a b (forall (lam (F (b b)) (forall (lam (A^2 (a b)) (forall (lam (L (o a a)) (implies (and (TRANSITIVE L) (forall (lam (X b) (L (A^2 X) (A^2 (F X)))))) (forall (lam (Y b) (L (A^2 Y) (A^2 (F (F Y)))))))))))))))
 (help "1982-SEP-21 COMPLETE AUTOMATIC TPS PROOF  
RUNCOUNT 87.20327 CONSCOUNT 471394  
SYSTEM TPS1  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
1982-SEP-30 COMPLETE AUTOMATIC PROOF IN TPS  
RUNCOUNT 48.5164 CONSCOUNT 247325  
SYSTEM TPS1  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
1982-OCT-04 COMPLETE TPS PROOF WITH PRINTING EXCLUDED FROM RUNCOUNT  
RUNCOUNT 26.570633 CONSCOUNT 103482  
SYSTEM TPS1  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
 
DATES  "))

(th~defproblem THM87
 (in tps)
 (conclusion THM87-conc
 (forall (lam (|k| (i i)) (forall (lam (|h| (i i)) (forall (lam (|a| i) (forall (lam (P (o i i i)) (exists (lam (|v| i) (forall (lam (|j| i) (exists (lam (|q| i) (implies (IN |j| (UNION (P |a| (|h| |j|)) (P |v| (|k| |j|)))) (IN |j| (P |v| |q|))))))))))))))))))
 (help "1985-AUG-22 COMPLETE AUTOMATIC PROOF  
 
RUNCOUNT 18.079021 CONSCOUNT 98741  
SYSTEM TPS1  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
 
DATES  "))

(th~defproblem THM81
 (in tps)
 (conclusion THM81-conc
 (forall (lam (R (o i i)) (implies (and (forall (lam (S i) (forall (lam (T i) (equiv (R S S) (R S T)))))) (forall (lam (W i) (forall (lam (Z i) (equiv (R W W) (R Z W))))))) (implies (exists (lam (X i) (R X X))) (forall (lam (Y i) (R Y Y))))))))
 (help ""))

(th~defproblem THM7-TPS2
 (in tps)
 (conclusion THM7-TPS2-conc
 (INJECTIVE (lam (|x| i) (lam (|y| i) (= |x| |y|)))))
 (help "Unitset is injective. Like THM104"))

(th~defproblem THM7
 (in tps)
 (conclusion THM7-conc
 (INJECTIVE (lam (|x| i) (lam (|y| i) (= |x| |y|)))))
 (help "Unitset is injective. Like THM104"))

(th~defproblem THM68B
 (in tps)
 (conclusion THM68B-conc
 (forall (lam (LYLE i) (forall (lam (BRUCE i) (forall (lam (LIKES (o i i)) (implies (and (forall (lam (X i) (LIKES-INFIX-TPS LIKES X BRUCE))) (forall (lam (Y i) (implies (exists (lam (Z i) (LIKES-INFIX-TPS LIKES Y Z))) (LIKES-INFIX-TPS LIKES LYLE Y))))) (exists (lam (U i) (forall (lam (V i) (LIKES-INFIX-TPS LIKES U V)))))))))))))
 (help "IF EVERYONE LIKES BRUCE AND LYLE LIKES EVERYONE WHO LIKES SOMEONE, THEN SOMEONE LIKES EVERYONE"))

(th~defproblem THM55A
 (in tps)
 (conclusion THM55A-conc
 (forall (lam (R (o i i)) (implies (implies (exists (lam (X i) (R X X))) (forall (lam (Y i) (R Y Y)))) (exists (lam (U i) (forall (lam (V i) (implies (R U U) (R V V))))))))))
 (help ""))

(th~defproblem THM557
 (in tps)
 (conclusion THM557-conc
  (all-types a (LC-RELN =)))
 (help "equality is an LC-RELN

This is an example in which TRUTHVALUES-HACK must be set to T."))

(th~defproblem THM50Q
 (in tps)
 (conclusion THM50Q-conc
 (forall (lam (P o) (forall (lam (Q o) (forall (lam (R o) (equiv (equiv (equiv P Q) R) (equiv P (equiv Q R))))))))))
 (help "quantified version of thm50"))

(th~defproblem THM48A
 (in tps)
 (conclusion THM48A-conc
 (forall (lam (F (i i)) (implies (INJECTIVE F) (INJECTIVE (COMPOSE F F))))))
 (help ""))

(th~defproblem THM47A
 (in tps)
 (conclusion THM47A-conc
 (forall (lam (X i) (forall (lam (Y i) (implies (= X Y) (forall (lam (R (o i i)) (implies (forall (lam (Z i) (R Z Z))) (R X Y))))))))))
 (help ""))

(th~defproblem THM30B
 (in tps)
 (conclusion THM30B-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (R (o a)) (implies (SUBSET (IN-IMAGE (lam (|x| a) |x|) R) (IN-IMAGE (lam (|x| a) |x|) S)) (SUBSET R S))))))))
 (help "Special case of THM30"))

(th~defproblem THM30A
 (in tps)
 (conclusion THM30A-conc
  (all-types a b (forall (lam (V (o b)) (forall (lam (U (o b)) (forall (lam (F (a b)) (implies (SUBSET U V) (SUBSET (IN-IMAGE F U) (IN-IMAGE F V)))))))))))
 (help ""))

(th~defproblem THM301
 (in tps)
 (conclusion THM301-conc
 (forall (lam (HALF (o i i)) (forall (lam (DOUBLE (o i i)) (forall (lam (S (i i)) (forall (lam (|0| i) (implies (and (forall (lam (Q (o i i)) (forall (lam (|u| i) (forall (lam (|v| i) (implies (and (DOUBLE |u| |v|) (and (Q |0| |0|) (forall (lam (|x| i) (forall (lam (|y| i) (implies (Q |x| |y|) (Q (S |x|) (S (S |y|)))))))))) (Q |u| |v|)))))))) (and (and (HALF |0| |0|) (HALF (S |0|) |0|)) (forall (lam (|x| i) (forall (lam (|y| i) (implies (HALF |x| |y|) (HALF (S (S |x|)) (S |y|))))))))) (forall (lam (|u| i) (forall (lam (|v| i) (implies (DOUBLE |u| |v|) (HALF |v| |u|))))))))))))))))
 (help ""))

(th~defproblem THM24
 (in tps)
 (conclusion THM24-conc
 (forall (lam (V i) (forall (lam (U i) (implies (not (= U V)) (exists (lam (G (i i (i i) (i i))) (not (COMMUTATIVE G))))))))))
 (help ""))

(th~defproblem THM23
 (in tps)
 (conclusion THM23-conc
  (all-types a (forall (lam (* (a a a)) (implies (ASSOCIATIVE *) (forall (lam (W a) (forall (lam (X a) (forall (lam (Y a) (forall (lam (Z a) (= (* (* (* W X) Y) Z) (* W (* X (* Y Z)))))))))))))))))
 (help "CONCLUSION IS THAT W * X  
* Y  
* Z = W * X * Y * Z  "))

(th~defproblem THM202
 (in tps)
 (conclusion THM202-conc
  (all-types a (forall (lam (|r| (o a a)) (forall (lam (|x| a) (forall (lam (|y| a) (implies (|r| |x| |y|) (TRCL-BBP |r| |x| |y|))))))))))
 (help ""))

(th~defproblem THM196B
 (in tps)
 (conclusion THM196B-conc
 (forall (lam (|b| i) (forall (lam (|a| i) (implies (not (= |a| |b|)) (not (forall (lam (|j| (i i)) (forall (lam (|k| (i i)) (implies (ITERATE+ |j| (COMPOSE |k| |j|)) (ITERATE+ |j| |k|)))))))))))))
 (help " Improved version of THM196 with 
unnecessary conditions deleted.
It is not true that if [k COMPOSE j] is an iterate of j, 
then k must be an iterate of j,
provided  we have distinct elements a and b."))

(th~defproblem THM185
 (in tps)
 (conclusion THM185-conc
  (all-types a (forall (lam (|r| (o a a)) (equiv (exists (lam (|x| a) (exists (lam (|y| a) (|r| |x| |y|))))) (exists (lam (|p| (a (a a a))) (APR1 |r| |p|))))))))
 (help "Basic theorem about representing relations in terms of ordered pairs.
  Essentially the same as THM149"))

(th~defproblem THM171A
 (in tps)
 (conclusion THM171A-conc
 (forall (lam (|g| (i i)) (forall (lam (|f| (i i)) (implies (= (COMPOSE |f| |g|) (COMPOSE |g| |f|)) (forall (lam (|x| i) (implies (and (= (|g| |x|) |x|) (forall (lam (|z| i) (implies (= (|g| |z|) |z|) (= |z| |x|))))) (= (|f| |x|) |x|))))))))))
 (help "If g commutes with f, any unique fixed point of g
 is a fixed point of f."))

(th~defproblem THM171
 (in tps)
 (conclusion THM171-conc
 (forall (lam (|g| (i i)) (forall (lam (|f| (i i)) (implies (= (COMPOSE |f| |g|) (COMPOSE |g| |f|)) (implies (exists (lam (|x| i) (and (= (|g| |x|) |x|) (forall (lam (|z| i) (implies (= (|g| |z|) |z|) (= |z| |x|))))))) (exists (lam (|y| i) (= (|f| |y|) |y|))))))))))
 (help "Part of the series THM170-THM172, which provides a nice example of cut. If g commutes with f, and g has a unique fixed point, then f has a fixed point. 
Similar to THM141."))

(th~defproblem THM170
 (in tps)
 (conclusion THM170-conc
 (forall (lam (|g| (i i)) (forall (lam (|f| (i i)) (implies (ITERATE+ |f| |g|) (= (COMPOSE |f| |g|) (COMPOSE |g| |f|))))))))
 (help "Part of the series THM170-THM172, which provides a nice example of cut. If g is an iterate of f, then g commutes with f."))

(th~defproblem THM149
 (in tps)
 (conclusion THM149-conc
  (all-types a (forall (lam (|r| (o a a)) (equiv (exists (lam (|x| a) (exists (lam (|y| a) (|r| |x| |y|))))) (exists (lam (|p| (a (a a a))) (INREL1 |p| |r|))))))))
 (help "Basic theorem about representing relations in terms of ordered pairs.
  Essentially the same as THM185"))

(th~defproblem THM148
 (in tps)
 (conclusion THM148-conc
  (all-types a (forall (lam (|r| (o a a)) (equiv (forall (lam (|x| a) (forall (lam (|y| a) (|r| |x| |y|))))) (forall (lam (|p| (a (a a a))) (INREL1 |p| |r|))))))))
 (help "Basic theorem about representing relations in terms of ordered pairs.
     Essentially the same as THM184."))

(th~defproblem THM144A
 (in tps)
 (conclusion THM144A-conc
 (forall (lam (|h| (i (o i))) (IN-IMAGE |h| (lam (|s| (o i)) (not (|s| (|h| |s|)))) (|h| (IN-IMAGE |h| (lam (|s| (o i)) (not (|s| (|h| |s|))))))))))
 (help "Essentially a lemma for the Injective Cantor Theorem X5309.
Also see X5309a, THM118, THM143, THM143A, and THM144"))

(th~defproblem THM141
 (in tps)
 (conclusion THM141-conc
 (forall (lam (|f| (i i)) (implies (exists (lam (|g| (i i)) (and (= (COMPOSE |f| |g|) (COMPOSE |g| |f|)) (exists (lam (|x| i) (and (= (|g| |x|) |x|) (forall (lam (|z| i) (implies (= (|g| |z|) |z|) (= |z| |x|)))))))))) (exists (lam (|y| i) (= (|f| |y|) |y|)))))))
 (help ""))

(th~defproblem THM130A
 (in tps)
 (conclusion THM130A-conc
 (forall (lam (|r| (o i i)) (forall (lam (|0| i) (forall (lam (S (i i)) (forall (lam (\|0\| i) (implies (and (and (IND-TPS \|0\| S) (|r| |0| |0|)) (forall (lam (|x| i) (implies (|r| |x| |x|) (|r| (S |x|) (S |x|)))))) (forall (lam (|x| i) (exists (lam (|y| i) (|r| |x| |y|)))))))))))))))
 (help "TPS must figure out what to do induction on."))

(th~defproblem THM130
 (in tps)
 (conclusion THM130-conc
 (forall (lam (|r| (o i i)) (forall (lam (|0| i) (forall (lam (S (i i)) (forall (lam (\|0\| i) (implies (and (and (IND-TPS \|0\| S) (|r| |0| |0|)) (forall (lam (|x| i) (forall (lam (|y| i) (implies (|r| |x| |y|) (|r| (S |x|) (S |y|)))))))) (forall (lam (|x| i) (exists (lam (|y| i) (|r| |x| |y|)))))))))))))))
 (help "Induction theorem in which the conclusion is weaker than the statement which must be proved by induction"))

(th~defproblem THM120B
 (in tps)
 (conclusion THM120B-conc
 (exists (lam (R (o (o i) (o i))) (and (and (not (R (lam (|x| i) true) (lam (|x| i) false))) (REFLEXIVE R)) (TRANSITIVE R)))))
 (help "Variant of THM120a designed to eliminate trivial proof. Subset is one such relation."))

(th~defproblem THM117C
 (in tps)
 (conclusion THM117C-conc
 (forall (lam (P (o i)) (forall (lam (R (o i i)) (forall (lam (|s| (o i)) (implies (and (forall (lam (|x| (o i)) (forall (lam (|z| i) (implies (|x| |z|) (exists (lam (|y| i) (and (|x| |y|) (forall (lam (|w| i) (implies (R |y| |w|) (not (|x| |w|))))))))))))) (forall (lam (|x1| i) (implies (forall (lam (|y1| i) (implies (and (|s| |y1|) (R |x1| |y1|)) (P |y1|)))) (P |x1|))))) (forall (lam (|x2| i) (implies (|s| |x2|) (P |x2|))))))))))))
 (help "TRANSFINITE INDUCTION theorem of Bailin and Barker-Plummer;
see Z-Match paper JAR 11 (1993) page 396;
if R is a well-founded relation and P is an inductive property over R restricted to s, 
then everything in s has property P; here R y w means y > w."))

(th~defproblem THM117B
 (in tps)
 (conclusion THM117B-conc
 (forall (lam (P (o i)) (forall (lam (R (o i i)) (forall (lam (|s| (o i)) (implies (and (forall (lam (|x| (o i)) (forall (lam (|z| i) (implies (|x| |z|) (exists (lam (|y| i) (and (|x| |y|) (forall (lam (|w| i) (implies (R |y| |w|) (not (|x| |w|))))))))))))) (forall (lam (|x1| i) (implies (forall (lam (|y1| i) (implies (and (|s| |y1|) (R |x1| |y1|)) (P |y1|)))) (P |x1|))))) (forall (lam (|x2| i) (implies (|s| |x2|) (P |x2|))))))))))))
 (help "TRANSFINITE INDUCTION theorem of Bailin and Barker-Plummer;
see Z-Match paper JAR 11 (1993) page 396;
if R is a well-founded relation and P is an inductive property over R restricted to s, 
then everything in s has property P; here R y w means y > w."))

(th~defproblem THM113
 (in tps)
 (conclusion THM113-conc
 (forall (lam (P (o i)) (exists (lam (M (o (i i))) (forall (lam (G (i i)) (implies (M G) (and (M (lam (Z i) (G (G Z)))) (forall (lam (Y i) (implies (P Y) (P (G Y))))))))))))))
 (help "(WEAKER FORM OF THM111)
(THERE IS A SET OF FUNCTIONS ON P CLOSED UNDER COMPOSITION)
(EXPANSION TERM
 (SUBM111 FOR M - LAMBDA G(II) FORALL X(I)[[P X] IMPLIES [P [G X])"))

(th~defproblem THM112
 (in tps)
 (conclusion THM112-conc
 (forall (lam (P (o i)) (exists (lam (M (o (i i))) (forall (lam (G (i i)) (forall (lam (H (i i)) (implies (and (M G) (M H)) (and (M (COMPOSE G H)) (forall (lam (Y i) (implies (P Y) (P (G Y))))))))))))))))
 (help "On every set there exists a set of functions which is closed under composition."))

(th~defproblem THM111
 (in tps)
 (conclusion THM111-conc
 (forall (lam (P (o i)) (exists (lam (M (o (i i))) (forall (lam (G (i i)) (forall (lam (H (i i)) (implies (and (M G) (M H)) (and (M (lam (Z i) (G (H Z)))) (forall (lam (Y i) (implies (P Y) (P (G Y))))))))))))))))
 (help "(THERE IS A SET OF FUNCTIONS ON P CLOSED UNDER COMPOSITION)
(EXPANSION TERM
 (SUBM111 FOR M - LAMBDA G(II) FORALL X(I)[[P X] IMPLIES [P [G X])"))

(th~defproblem EXT1
 (in tps)
 (conclusion EXT1-conc
  (all-types g b (forall (lam (|g| (o b)) (forall (lam (|f| (o b)) (forall (lam (|h| (g (o b))) (implies (implies (forall (lam (|x| b) (equiv (|f| |x|) (|g| |x|)))) (forall (lam (|q| (o (o b))) (implies (|q| |f|) (|q| |g|))))) (implies (forall (lam (|x| b) (equiv (|f| |x|) (|g| |x|)))) (= (|h| |f|) (|h| |g|))))))))))))
 (help "Theorem about extensionality"))

(th~defproblem BLEDSOE6
 (in tps)
 (conclusion BLEDSOE6-conc
 (forall (lam (|b| i) (forall (lam (|a| i) (forall (lam (P (o i)) (exists (lam (A (o i)) (implies (and (P |a|) (not (= |a| |b|))) (and (and (forall (lam (|x| i) (implies (A |x|) (P |x|)))) (exists (lam (|y| i) (A |y|)))) (not (A |b|)))))))))))))
 (help ""))

(th~defproblem TTTP6100
 (in tps)
 (conclusion TTTP6100-conc
 (NAT ZERO))
 (help "Theorem 6100 in TTTP"))

(th~defproblem TTTP5243
 (in tps)
 (conclusion TTTP5243-conc
  (all-types a b (forall (lam (A^0 b) (exists (lam (U (b a)) (forall (lam (V a) (= (U V) A^0)))))))))
 (help "Comprehension Theorem"))

(th~defproblem TRIV0
 (in tps)
 (conclusion TRIV0-conc
 (forall (lam (|p| o) (or |p| (not |p|)))))
 (help ""))

(th~defproblem THM84
 (in tps)
 (conclusion THM84-conc
 (forall (lam (P (o i i)) (forall (lam (|h| (i i)) (forall (lam (|g| (i i)) (exists (lam (|x| i) (forall (lam (|y| i) (implies (forall (lam (|f| (i i)) (forall (lam (|z| i) (and (P |z| (|f| |x|)) (P |x| |y|)))))) (P |y| (|g| (|h| |y|)))))))))))))))
 (help "1982 -FEB-22 EASY THEOREM  
BOTH MATING AND OUTLINE FOUND AUTOMATICALLY  
 
 
DATES  "))

(th~defproblem THM8
 (in tps)
 (conclusion THM8-conc
 (exists (lam (F (i i)) (INJECTIVE F))))
 (help ""))

(th~defproblem THM7B
 (in tps)
 (conclusion THM7B-conc
 (forall (lam (|x| i) (forall (lam (|y| i) (implies (forall (lam (|q| (i (o i))) (implies ((lam (|w| (o i)) (|w| (|q| |w|))) (lam (|z| i) (= |x| |z|))) ((lam (|w| (o i)) (|w| (|q| |w|))) (lam (|z| i) (= |y| |z|)))))) (= |x| |y|)))))))
 (help "half-proved version of thm7 for test purposes"))

(th~defproblem THM76
 (in tps)
 (conclusion THM76-conc
 (forall (lam (X i) (forall (lam (Y i) (implies (= Y X) (= X Y)))))))
 (help "Symmetry of equality"))

(th~defproblem THM54
 (in tps)
 (conclusion THM54-conc
  (all-types b a (forall (lam (F (a b)) (implies (INJECTIVE F) (INJECTIVE F))))))
 (help "1979 -JAN-8 LONG ATTEMPT MET BUG AT 56 SECONDS  
NOWHERE NEAR CORRECT MATING  
 
1980-FEB-12 COMPLETE AUTOMATIC PROOF ON SYSTEM 3 IN 22 SECONDS  
 
 
DATES  "))

(th~defproblem THM534
 (in tps)
 (conclusion THM534-conc
  (all-types a (implies (forall (lam (|s| (o (o a))) (implies (forall (lam (X (o a)) (implies (|s| X) (exists (lam (|t| a) (X |t|)))))) (exists (lam (|f| (a (o a))) (forall (lam (X (o a)) (implies (|s| X) (X (|f| X)))))))))) (forall (lam (|g| (o a (a (o a)))) (implies (forall (lam (|h| (a (o a))) (exists (lam (|u| a) (|g| |h| |u|))))) (exists (lam (|f| (a (o a))) (|g| |f| (|f| (|g| |f|)))))))))))
 (help "AC1 => AC17 from Rubin and Rubin"))

(th~defproblem THM52
 (in tps)
 (conclusion THM52-conc
 (forall (lam (R o) (forall (lam (Q o) (forall (lam (P o) (implies (equiv (equiv P Q) R) (equiv P (equiv Q R))))))))))
 (help "Asociativity of equivalence"))

(th~defproblem THM50-HALF
 (in tps)
 (conclusion THM50-HALF-conc
 (forall (lam (R o) (forall (lam (Q o) (forall (lam (P o) (implies (equiv (equiv P Q) R) (equiv P (equiv Q R))))))))))
 (help ""))

(th~defproblem THM50-A
 (in tps)
 (conclusion THM50-A-conc
 (forall (lam (R o) (forall (lam (Q o) (forall (lam (P o) (implies (equiv (equiv P Q) R) (equiv P (equiv Q R))))))))))
 (help "one direction of thm50"))

(th~defproblem THM50-13
 (in tps)
 (conclusion THM50-13-conc
 (forall (lam (P o) (forall (lam (R o) (forall (lam (Q o) (exists (lam (|x| i) (implies (and (not P) (or (and Q (not R)) (and (not Q) R))) (implies (or (and P Q) (and (not P) (not Q))) R)))))))))))
 (help "Simple formula for debugging"))

(th~defproblem THM50-11
 (in tps)
 (conclusion THM50-11-conc
 (forall (lam (P o) (forall (lam (R o) (forall (lam (Q o) (implies (and (not P) (or (and Q (not R)) (and (not Q) R))) (implies (or (and P Q) (and (not P) (not Q))) R)))))))))
 (help "Simplified bug sample for thm50"))

(th~defproblem THM50
 (in tps)
 (conclusion THM50-conc
 (forall (lam (R o) (forall (lam (Q o) (forall (lam (P o) (equiv (equiv (equiv P Q) R) (equiv P (equiv Q R))))))))))
 (help "Associativity of equivalence"))

(th~defproblem THM201
 (in tps)
 (conclusion THM201-conc
  (all-types a (forall (lam (|r| (o a a)) (REFLEXIVE (TRCL-BBP |r|))))))
 (help ""))

(th~defproblem THM200
 (in tps)
 (conclusion THM200-conc
 (forall (lam (B o) (forall (lam (A o) (implies (implies (implies (implies (implies A B) A) A) B) B))))))
 (help "Nepejvoda's problem; supposedly a difficult ND problem."))

(th~defproblem THM19SK1
 (in tps)
 (conclusion THM19SK1-conc
 (forall (lam (P (o i i)) (forall (lam (E (i (o i))) (not (and (forall (lam (|x| i) (P |x| (E (lam (|y| i) (P |x| |y|)))))) (forall (lam (|f| (i i)) (not (P (E (lam (|x| i) (not (P |x| (|f| |x|))))) (|f| (E (lam (|x| i) (not (P |x| (|f| |x|)))))))))))))))))
 (help "Related to THM19; result of Skolemizing naively."))

(th~defproblem THM197
 (in tps)
 (conclusion THM197-conc
 (forall (lam (|b| i) (forall (lam (|a| i) (implies (not (= |a| |b|)) (not (forall (lam (|f| (i i)) (forall (lam (|g| (i i)) (= (COMPOSE |f| |g|) (COMPOSE |g| |f|)))))))))))))
 (help "If there are at least two individuals,

not all functions commute with each other."))

(th~defproblem THM192
 (in tps)
 (conclusion THM192-conc
 (forall (lam (|h| (i (o i))) (implies (IDIAG |h| (|h| (IDIAG |h|))) (IDIAG |h| (|h| (IDIAG |h|)))))))
 (help "Part of THM193"))

(th~defproblem THM187
 (in tps)
 (conclusion THM187-conc
  (all-types a (forall (lam (|p| (a (a a a))) (implies (ISPAIR1 |p|) (= (PAIR1 (LEFT1 |p|) (RIGHT1 |p|)) |p|))))))
 (help "Basic theorem about pairing"))

(th~defproblem THM186
 (in tps)
 (conclusion THM186-conc
  (all-types a (forall (lam (|y| a) (forall (lam (|x| a) (ISPAIR1 (PAIR1 |x| |y|))))))))
 (help "Basic theorem about pairing"))

(th~defproblem THM184
 (in tps)
 (conclusion THM184-conc
  (all-types a (forall (lam (|r| (o a a)) (equiv (forall (lam (|x| a) (forall (lam (|y| a) (|r| |x| |y|))))) (forall (lam (|p| (a (a a a))) (APR1 |r| |p|))))))))
 (help "Basic theorem about representing relations in terms of ordered pairs.
  Essentially the same as THM148"))

(th~defproblem THM183
 (in tps)
 (conclusion THM183-conc
  (all-types a (forall (lam (|r| (o a a)) (forall (lam (|x| a) (forall (lam (|y| a) (equiv (APR1 |r| (PAIR1 |x| |y|)) (|r| |x| |y|))))))))))
 (help "Basic theorem about pairing"))

(th~defproblem THM181
 (in tps)
 (conclusion THM181-conc
  (all-types a (forall (lam (|x| a) (forall (lam (|y| a) (= (RIGHT1 (PAIR1 |x| |y|)) |y|)))))))
 (help "Basic theorem about pairing"))

(th~defproblem THM180
 (in tps)
 (conclusion THM180-conc
  (all-types a (forall (lam (|x| a) (forall (lam (|y| a) (= (LEFT1 (PAIR1 |x| |y|)) |x|)))))))
 (help "Basic theorem about pairing"))

(th~defproblem THM175
 (in tps)
 (conclusion THM175-conc
 (forall (lam (|s| (o i i)) (SUBRELATION |s| |s|))))
 (help "Reflexivity of subrelation"))

(th~defproblem THM174
 (in tps)
 (conclusion THM174-conc
  (all-types b a (forall (lam (|r| (o a b)) (forall (lam (|s| (o a b)) (implies (forall (lam (|x| b) (forall (lam (|y| a) (equiv (|r| |x| |y|) (|s| |x| |y|)))))) (= |r| |s|))))))))
 (help "Principle of extensionality for binary relations"))

(th~defproblem THM163
 (in tps)
 (conclusion THM163-conc
  (all-types a (FINITE1 (lam (|x| a) false))))
 (help "Direct consequence of the definition ofFINITE1"))

(th~defproblem THM151
 (in tps)
 (conclusion THM151-conc
  (all-types a (forall (lam (|r| (o a a)) (SUBRELATION |r| (TC2 |r|))))))
 (help ""))

(th~defproblem THM144B
 (in tps)
 (conclusion THM144B-conc
 (forall (lam (|h| (i (o i))) (IDIAG |h| (|h| (IDIAG |h|))))))
 (help "Essentially a lemma for the Injective Cantor Theorem X5309.
Also see X5309a, THM118, THM143, THM143A, THM143B, THM144, THM143A"))

(th~defproblem THM139
 (in tps)
 (conclusion THM139-conc
 (forall (lam (|y| i) (exists (lam (|f| (i i)) (IN |y| (RANGE |f|)))))))
 (help "Every object is in the range of some function."))

(th~defproblem THM134
 (in tps)
 (conclusion THM134-conc
 (forall (lam (|z| i) (forall (lam (|g| (i i)) (implies (ITERATE+ (lam (|x| i) |z|) |g|) (forall (lam (|x| i) (= (|g| |x|) |z|)))))))))
 (help "Every positive iterate of a constant function is a constant function. To prove this add (OI) to PRIM-BDTYPES; can remove I."))

(th~defproblem THM125D
 (in tps)
 (conclusion THM125D-conc
 (forall (lam (|a| i) (forall (lam (|b| i) (forall (lam (|c| i) (forall (lam (P (o i)) (exists (lam (|m| (o i)) (exists (lam (|n| (o i)) (and (and (and (or (|n| |a|) (|m| |a|)) (or (P |b|) (|n| |b|))) (or (|m| |c|) (not (P |c|)))) (forall (lam (|x| i) (equiv (|n| |x|) (not (P |x|)))))))))))))))))))
 (help "Trivial theorem to test how TPS mates flexible-flexible pairs; like THM125B; compare with THM125C"))

(th~defproblem THM125C
 (in tps)
 (conclusion THM125C-conc
 (forall (lam (|a| i) (forall (lam (|b| i) (forall (lam (|c| i) (forall (lam (P (o i)) (exists (lam (|m| (o i)) (exists (lam (|n| (o i)) (and (and (and (or (|m| |a|) (|n| |a|)) (or (P |b|) (|n| |b|))) (or (|m| |c|) (not (P |c|)))) (forall (lam (|x| i) (equiv (|n| |x|) (not (P |x|)))))))))))))))))))
 (help "Trivial theorem to test how TPS mates flexible-flexible pairs; like THM125A; compare with THM125D, which is very similar but much harder for TPS."))

(th~defproblem THM125B
 (in tps)
 (conclusion THM125B-conc
 (forall (lam (|a| i) (forall (lam (|b| i) (forall (lam (|c| i) (forall (lam (P (o i)) (exists (lam (|m| (o i)) (exists (lam (|n| (o i)) (and (and (or (|n| |a|) (|m| |a|)) (or (P |b|) (|n| |b|))) (or (|m| |c|) (not (P |c|)))))))))))))))))
 (help "Trivial theorem to test how TPS mates flexible-flexible pairs; also see THM125A"))

(th~defproblem THM125A
 (in tps)
 (conclusion THM125A-conc
 (forall (lam (|a| i) (forall (lam (|b| i) (forall (lam (|c| i) (forall (lam (P (o i)) (exists (lam (|m| (o i)) (exists (lam (|n| (o i)) (and (and (or (|m| |a|) (|n| |a|)) (or (P |b|) (|n| |b|))) (or (|m| |c|) (not (P |c|)))))))))))))))))
 (help "Trivial theorem to test how TPS mates flexible-flexible pairs; also see THM125B."))

(th~defproblem THM121
 (in tps)
 (conclusion THM121-conc
  (all-types a (forall (lam (|r| (o o)) (forall (lam (|p| (o a)) (forall (lam (|q| (o a)) (exists (lam (|h| o) (equiv (|r| |h|) (|r| (forall (lam (|x| a) (or (|p| |x|) (|q| |x|))))))))))))))))
 (help "Test theorem for nested primitive substitutions"))

(th~defproblem THM120
 (in tps)
 (conclusion THM120-conc
 (exists (lam (R (o (o i) (o i))) (TRANSITIVE R))))
 (help "There exists a transitive relation on sets"))

(th~defproblem THM114
 (in tps)
 (conclusion THM114-conc
 (forall (lam (Q o) (forall (lam (P o) (implies (and P (implies P Q)) Q))))))
 (help ""))

(th~defproblem THM107
 (in tps)
 (conclusion THM107-conc
  (all-types b ((lam (P (b b (b b) (b b))) (forall (lam (X (b b)) (forall (lam (Y (b b)) (forall (lam (Z (b b)) (= (P (P X Y) Z) (P X (P Y Z)))))))))) (lam (F (b b)) (lam (G (b b)) (lam (W b) (F (G W))))))))
 (help "1985-AUG-18 COMPLETE AUTOMATIC PROOF  
 
RUNCOUNT 3.7816093 CONSCOUNT 24273  
SYSTEM TPS1  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC T  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
 
DATES  "))

(th~defproblem STRANGE-HO-EXAMPLE
 (in tps)
 (conclusion STRANGE-HO-EXAMPLE-conc
 (forall (lam (|y| i) (forall (lam (|x| i) (forall (lam (S (o (o i i))) (implies (and (S (STRANGE-HO-ABBR S)) (STRANGE-HO-ABBR S |x| |y|)) (STRANGE-HO-ABBR S |y| |x|)))))))))
 (help ""))

(th~defproblem FN-THM-4
 (in tps)
 (conclusion FN-THM-4-conc
  (all-types c a b (forall (lam (F (b a)) (forall (lam (G (c b)) (implies (SURJECTIVE (COMPOSE G F)) (SURJECTIVE G))))))))
 (help ""))

(th~defproblem BLEDSOE-FENG-SV-I1
 (in tps)
 (conclusion BLEDSOE-FENG-SV-I1-conc
 (forall (lam (P (o i)) (forall (lam (|n| i) (forall (lam (1+ (i i)) (forall (lam (|xO^1| i) (implies (and (and (forall (lam (A (o i)) (implies (and (A |xO^1|) (forall (lam (|x| i) (implies (A |x|) (A (1+ |x|)))))) (A |n|)))) (P |xO^1|)) (forall (lam (|x| i) (implies (P |x|) (P (1+ |x|)))))) (P |n|)))))))))))
 (help "Example I1 from the Bledsoe-Feng SET-VAR paper."))

(th~defproblem THM15B
 (in tps)
 (conclusion THM15B-conc
 (forall (lam (|f| (i i)) (implies (exists (lam (|g| (i i)) (and (ITERATE+ |f| |g|) (exists (lam (|x| i) (and (= (|g| |x|) |x|) (forall (lam (|z| i) (implies (= (|g| |z|) |z|) (= |z| |x|)))))))))) (exists (lam (|y| i) (= (|f| |y|) |y|)))))))
 (help "If some iterate of a function f has a unique fixed point,
then f has a fixed point.
Example discussed in Resolution in Type Theory, JSL 36 (1971)."))

(th~defproblem THM46A
 (in tps)
 (conclusion THM46A-conc
 (forall (lam (E (o i)) (forall (lam (D (o i)) (EQUIVS (POWERSET (INTERSECT D E)) (INTERSECT (POWERSET D) (POWERSET E))))))))
 (help "12 CLAUSES 24 PATHS WHEN MINIMIZE CLAUSES  
12 PATHS 24 CLAUSES WHEN MINIMIZE PATHS  
REQUIRES QUANTIFIER DUPLICATION  "))

(th~defproblem THM12
 (in tps)
 (conclusion THM12-conc
 (forall (lam (R (o i)) (forall (lam (S (o i)) (implies (= R S) (forall (lam (X i) (implies (S X) (R X))))))))))
 (help ""))

(th~defproblem THM44-90
 (in tps)
 (conclusion THM44-90-conc
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (equiv (exists (lam (S (o i)) (forall (lam (X i) (and (or (S X) (P X)) (or (not (S X)) (Q X))))))) (forall (lam (Y i) (or (P Y) (Q Y))))))))))
 (help ""))

(th~defproblem BOOL-PROP-54
 (in tps)
 (conclusion BOOL-PROP-54-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (implies (SUBSET X Y) (= Y (UNION X (SETDIFF Y X))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem CT23B
 (in tps)
 (conclusion CT23B-conc
 (forall (lam (|x| o) (forall (lam (|y| o) (= (= |x| |y|) (= |y| |x|)))))))
 (help ""))

(th~defproblem GAZING-THM27
 (in tps)
 (conclusion GAZING-THM27-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (= (INTERSECT (INTERSECT S T) U) (INTERSECT S (INTERSECT T U)))))))))))
 (help "Example 27 from Barker-Plummer's paper about Gazing"))

(th~defproblem GAZING-THM19
 (in tps)
 (conclusion GAZING-THM19-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (SUBSET S (UNION T S))))))))
 (help "Example 19 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-81
 (in tps)
 (conclusion BOOL-PROP-81-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (SETDIFF X (SETDIFF Y Z)) (UNION (SETDIFF X Y) (INTERSECT X Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM46
 (in tps)
 (conclusion THM46-conc
 (forall (lam (E (o i)) (forall (lam (D (o i)) (SUBSET (POWERSET (INTERSECT D E)) (INTERSECT (POWERSET D) (POWERSET E))))))))
 (help ""))

(th~defproblem CT23A
 (in tps)
 (conclusion CT23A-conc
 (exists (lam (|x| o) (forall (lam (|y| o) (= (= |x| |y|) (= |y| |x|)))))))
 (help "Simple propositional theorem"))

(th~defproblem CT26
 (in tps)
 (conclusion CT26-conc
 (forall (lam (|x| o) (forall (lam (|y| o) (= (= |x| |y|) (= |y| |x|)))))))
 (help ""))

(th~defproblem X2150
 (in tps)
 (conclusion X2150-conc
 (forall (lam (P (o i i i)) (exists (lam (W i) (implies (forall (lam (X i) (exists (lam (Y i) (P W X Y))))) (exists (lam (Z i) (P Z Z W)))))))))
 (help ""))

(th~defproblem BOOL-PROP-37
 (in tps)
 (conclusion BOOL-PROP-37-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (SUBSET (INTERSECT X Y) X)))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-99
 (in tps)
 (conclusion BOOL-PROP-99-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (SYMDIFF X (SYMDIFF Y Z)) (SYMDIFF (SYMDIFF X Y) Z))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM45
 (in tps)
 (conclusion THM45-conc
 (forall (lam (Q (o i i)) (forall (lam (E i) (forall (lam (D i) (forall (lam (P (o i i)) (implies (and (and (P D E) (forall (lam (X i) (forall (lam (Y i) (implies (P X Y) (and (P Y X) (Q X Y)))))))) (forall (lam (U i) (forall (lam (V i) (implies (Q U V) (Q U U))))))) (and (Q D D) (Q E E))))))))))))
 (help "TRIVIAL THEOREM  
REQUIRES 2 COPIES OF CLAUSES  
YIELDS 32 PATHS 8 CLAUSES AFTER QUANTIFIER DUPLICATION"))

(th~defproblem BOOL-PROP-29
 (in tps)
 (conclusion BOOL-PROP-29-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (and (SUBSET X Y) (SUBSET Y Z)) (SUBSET X Z))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-93
 (in tps)
 (conclusion BOOL-PROP-93-conc
  (all-types a (forall (lam (X (o a)) (= (SYMDIFF X X) NULLSET)))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM44
 (in tps)
 (conclusion THM44-conc
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (equiv (exists (lam (S (o i)) (forall (lam (X i) (and (or (S X) (P X)) (or (not (S X)) (Q X))))))) (forall (lam (Y i) (or (P Y) (Q Y))))))))))
 (help "USEFUL IN ELIMINATING A QUANTIFIER ON A SECOND ORDER VARIABLE"))

(th~defproblem GAZING-THM29
 (in tps)
 (conclusion GAZING-THM29-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (= (INTERSECT S (UNION T U)) (UNION (INTERSECT S T) (INTERSECT S U)))))))))))
 (help "Example 29 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-23
 (in tps)
 (conclusion BOOL-PROP-23-conc
  (all-types a (forall (lam (|x| a) (forall (lam (X (o a)) (forall (lam (Y (o a)) (equiv (equiv (SYMDIFF X Y |x|) (X |x|)) (not (Y |x|)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM79
 (in tps)
 (conclusion THM79-conc
 (forall (lam (G (o i)) (forall (lam (N (o i)) (forall (lam (M (o i)) (or (forall (lam (R i) (M R))) (or (exists (lam (X i) (not (G X)))) (or (not (forall (lam (Y i) (or (M Y) (exists (lam (S i) (N S))))))) (not (forall (lam (Z i) (or (not (N Z)) (not (forall (lam (T i) (G T))))))))))))))))))
 (help "1981-JUL-23 MATING FOUND AUTOMATICALLY  
RUNTIME 3.1420605 CONS COUNT 14239  
SYSTEM System III  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
1981-JUL-26 PLAN FOUND AUTOMATICALLY  
RUNTIME 7.4537096 CONS COUNT 32366  
SYSTEM System III  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
 
DATES  "))

(th~defproblem GAZING-THM25
 (in tps)
 (conclusion GAZING-THM25-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (= (INTERSECT S T) (INTERSECT T S))))))))
 (help "Example 25 from Barker-Plummer's paper about Gazing"))

(th~defproblem THM128
 (in tps)
 (conclusion THM128-conc
 (forall (lam (P (o i)) (forall (lam (|f| (i i)) (forall (lam (|a| i) (forall (lam (|g| (i i)) (implies (= (COMPOSE |f| |g|) (COMPOSE |g| |f|)) (implies (P (|f| (|f| (|g| |a|)))) (P (|g| (|f| (|f| |a|)))))))))))))))
 (help ""))

(th~defproblem THM43
 (in tps)
 (conclusion THM43-conc
 (forall (lam (S (o i)) (not (exists (lam (G (o i i)) (forall (lam (F (o i)) (implies (SUBSET F S) (exists (lam (J i) (and (S J) (= (G J) F)))))))))))))
 (help ""))

(th~defproblem BOOL-PROP-50
 (in tps)
 (conclusion BOOL-PROP-50-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (implies (SUBSET X (SETDIFF Y X)) (= X NULLSET))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-112
 (in tps)
 (conclusion BOOL-PROP-112-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (MISSES (INTERSECT X Y) (SYMDIFF X Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM78
 (in tps)
 (conclusion THM78-conc
 (forall (lam (G (o i)) (forall (lam (N (o i)) (forall (lam (M (o i)) (or (forall (lam (R i) (M R))) (or (exists (lam (X i) (not (G X)))) (or (not (or (forall (lam (Y i) (M Y))) (exists (lam (S i) (N S))))) (not (or (forall (lam (Z i) (not (N Z)))) (not (forall (lam (T i) (G T))))))))))))))))
 (help "WFF S FROM FALSE LEMMAS IN HERBRAND"))

(th~defproblem BOOL-PROP-76
 (in tps)
 (conclusion BOOL-PROP-76-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (SETDIFF X (UNION X Y)) NULLSET)))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-39
 (in tps)
 (conclusion BOOL-PROP-39-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (and (SUBSET Z X) (SUBSET Z Y)) (SUBSET Z (UNION X Y)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem PELL40
 (in tps)
 (conclusion PELL40-conc
 (forall (lam (F (o i i)) (implies (exists (lam (|y| i) (forall (lam (|x| i) (equiv (F |x| |y|) (F |x| |x|)))))) (not (forall (lam (|x| i) (exists (lam (|y| i) (forall (lam (|z| i) (equiv (F |z| |y|) (not (F |z| |x|))))))))))))))
 (help "The existence of an anti-russel set (a set
containing exactly the elements which are members of themself) implies that not 
every set has a complement"))

(th~defproblem BOOL-PROP-33
 (in tps)
 (conclusion BOOL-PROP-33-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (SUBSET X Y) (SUBSET (UNION X Z) (UNION Y Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM101
 (in tps)
 (conclusion THM101-conc
 (forall (lam (P (o i i)) (implies (and (forall (lam (|x| i) (P |x| |x|))) (forall (lam (|x| i) (forall (lam (|y| i) (forall (lam (|z| i) (implies (and (P |x| |y|) (P |z| |y|)) (P |x| |z|))))))))) (forall (lam (|u| i) (forall (lam (|v| i) (forall (lam (|w| i) (implies (and (P |u| |v|) (P |v| |w|)) (P |u| |w|))))))))))))
 (help "1983-MAR-16 COMPLETE PROOF  
RUNCOUNT 35.46545 CONSCOUNT 150901  
SYSTEM TPS1  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
 
DATES  "))

(th~defproblem GAZING-THM35
 (in tps)
 (conclusion GAZING-THM35-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (implies (and (SUBSET S U) (SUBSET T U)) (equiv (SUBSET S T) (SUBSET (INTERSECT S (DIFF U T)) (DIFF U S))))))))))))
 (help "Example 35 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-88
 (in tps)
 (conclusion BOOL-PROP-88-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (SETDIFF (SETDIFF X Y) Z) (SETDIFF X (UNION Y Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem CT19
 (in tps)
 (conclusion CT19-conc
 (exists (lam (|q| (o (o o))) (forall (lam (|p| (o o)) (|q| |p|))))))
 (help ""))

(th~defproblem PELL27
 (in tps)
 (conclusion PELL27-conc
 (forall (lam (J (o i)) (forall (lam (|xI^0| (o i)) (forall (lam (H (o i)) (forall (lam (G (o i)) (forall (lam (F (o i)) (implies (and (and (and (exists (lam (|x| i) (and (F |x|) (not (G |x|))))) (forall (lam (|x| i) (implies (F |x|) (H |x|))))) (forall (lam (|x| i) (implies (and (J |x|) (|xI^0| |x|)) (F |x|))))) (implies (exists (lam (|x| i) (and (H |x|) (not (G |x|))))) (forall (lam (|x| i) (implies (|xI^0| |x|) (not (H |x|))))))) (forall (lam (|x| i) (implies (J |x|) (not (|xI^0| |x|)))))))))))))))))
 (help ""))

(th~defproblem BOOL-PROP-101
 (in tps)
 (conclusion BOOL-PROP-101-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (and (MEETS X Y) (SUBSET Y Z)) (MEETS X Z))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BLEDSOE-FENG-6
 (in tps)
 (conclusion BLEDSOE-FENG-6-conc
 (forall (lam (Q (o (o i) i)) (forall (lam (|b| i) (forall (lam (|a| i) (forall (lam (P (o i i)) (implies (and (P |a| |b|) (forall (lam (E (o i)) (Q |b| E)))) (exists (lam (A (o i)) (forall (lam (|g| i) (implies (A |g|) (and (exists (lam (|x| i) (and (P |g| |x|) (Q |x| A)))) (A |a|)))))))))))))))))
 (help "Set Var, JAR paper, p. 299, Example 6: BLEDSOE-FENG-6 (I created this, to distinguish it from BLEDSOE6, apparently from another source)"))

(th~defproblem THM75
 (in tps)
 (conclusion THM75-conc
 (forall (lam (|k| (i i)) (forall (lam (|h| (i i)) (forall (lam (|a| i) (forall (lam (P (o i i i)) (exists (lam (|v| i) (forall (lam (|j| i) (exists (lam (|q| i) (implies (or (P |a| |j| (|h| |j|)) (P |v| |j| (|k| |j|))) (P |v| |j| |q|)))))))))))))))))
 (help "RELATED TO THM87, WHICH WAS USED FOR CADE-6"))

(th~defproblem CT9
 (in tps)
 (conclusion CT9-conc
 (exists (lam (|x| o) (exists (lam (|y| o) |x|)))))
 (help ""))

(th~defproblem CT8
 (in tps)
 (conclusion CT8-conc
 (forall (lam (|x| o) (forall (lam (|y| o) (exists (lam (|p| (o o)) (and (|p| |x|) (|p| |y|)))))))))
 (help ""))

(th~defproblem GAZING-THM21
 (in tps)
 (conclusion GAZING-THM21-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (SUBSET (INTERSECT S T) T)))))))
 (help "Example 21 from Barker-Plummer's paper about Gazing"))

(th~defproblem CT7
 (in tps)
 (conclusion CT7-conc
 (exists (lam (|p| (o o)) (forall (lam (|x| o) (forall (lam (|y| o) (and (|p| |x|) (|p| |y|)))))))))
 (help ""))

(th~defproblem BOOL-PROP-45
 (in tps)
 (conclusion BOOL-PROP-45-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (equiv (= (SETDIFF X Y) NULLSET) (SUBSET X Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem CT5
 (in tps)
 (conclusion CT5-conc
 (exists (lam (|x| o) (forall (lam (|y| o) (implies |x| |y|))))))
 (help ""))

(th~defproblem CT4
 (in tps)
 (conclusion CT4-conc
 (forall (lam (|x| o) (forall (lam (|y| o) (exists (lam (|p| (o o)) (implies (|p| |x|) (|p| |y|)))))))))
 (help ""))

(th~defproblem CT3
 (in tps)
 (conclusion CT3-conc
 (forall (lam (|a| o) (exists (lam (|p| (o o)) (|p| |a|))))))
 (help ""))

(th~defproblem CT2
 (in tps)
 (conclusion CT2-conc
 (forall (lam (|a| o) (exists (lam (|p| (o o)) (|p| |a|))))))
 (help ""))

(th~defproblem CT1
 (in tps)
 (conclusion CT1-conc
 (exists (lam (|x| o) |x|)))
 (help "chris.lib"))

(th~defproblem BOOL-PROP-117
 (in tps)
 (conclusion BOOL-PROP-117-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (INTERSECT X (SETDIFF Y Z)) (SETDIFF (INTERSECT X Y) (INTERSECT X Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-72
 (in tps)
 (conclusion BOOL-PROP-72-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (UNION (INTERSECT X Y) (UNION (INTERSECT Y Z) (INTERSECT Z X))) (INTERSECT (UNION X Y) (INTERSECT (UNION Y Z) (UNION Z X))))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM189
 (in tps)
 (conclusion THM189-conc
  (all-types a (forall (lam (|r| (o a a a a)) (implies (forall (lam (|p| (a (a a a))) (forall (lam (|q| (a (a a a))) (implies (APR1 (APR1 |r| |p|) |q|) (= |p| |q|)))))) (forall (lam (|x1| a) (forall (lam (|y1| a) (forall (lam (|x2| a) (forall (lam (|y2| a) (implies (|r| |x1| |y1| |x2| |y2|) (and (= |x1| |x2|) (= |y1| |y2|))))))))))))))))
 (help "Basic theorem about pairing"))

(th~defproblem BLEDSOE1
 (in tps)
 (conclusion BLEDSOE1-conc
 (forall (lam (|0| (o (o i))) (exists (lam (A (o (o (o i)))) (forall (lam (|x| (o (o i))) (implies (A |x|) (LESS= |x| |0|)))))))))
 (help ""))

(th~defproblem BOOL-PROP-57
 (in tps)
 (conclusion BOOL-PROP-57-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (equiv (= X (INTERSECT Y Z)) (and (and (SUBSET X Y) (SUBSET X Z)) (forall (lam (V (o a)) (implies (and (SUBSET V Y) (SUBSET V Z)) (SUBSET V X))))))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM72
 (in tps)
 (conclusion THM72-conc
 (forall (lam (P (o i)) (forall (lam (L (o i i)) (forall (lam (H (i i)) (forall (lam (G (i i)) (implies (and (forall (lam (X i) (exists (lam (Y i) (implies (P X) (and (L X (G (H Y))) (P Y))))))) (forall (lam (W i) (implies (P W) (and (P (G W)) (P (H W))))))) (forall (lam (X i) (implies (P X) (exists (lam (Y i) (and (L X Y) (P Y)))))))))))))))))
 (help "Simple theorem used for example in CADE-5 lecture."))

(th~defproblem GAZING-THM31
 (in tps)
 (conclusion GAZING-THM31-conc
  (all-types a (forall (lam (S (o a)) (= (INTERSECT S S) S)))))
 (help "Example 31 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-84
 (in tps)
 (conclusion BOOL-PROP-84-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (equiv (= (INTERSECT X Y) NULLSET) (= (SETDIFF X Y) X))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem CT22
 (in tps)
 (conclusion CT22-conc
 (not (exists (lam (|p| (o o)) (and (|p| (= |p| |p|)) (not (|p| (= |p| |p|))))))))
 (help ""))

(th~defproblem CT12
 (in tps)
 (conclusion CT12-conc
 (exists (lam (|x| o) (exists (lam (|y| o) (= |x| |y|))))))
 (help ""))

(th~defproblem GAZING-THM7
 (in tps)
 (conclusion GAZING-THM7-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (implies (and (= S T) (= T U)) (= S U))))))))))
 (help "Example 7 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-41
 (in tps)
 (conclusion BOOL-PROP-41-conc
  (all-types a (forall (lam (V (o a)) (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (and (SUBSET X Y) (SUBSET Z V)) (SUBSET (INTERSECT X Z) (INTERSECT Y V)))))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem GAZING-THM43
 (in tps)
 (conclusion GAZING-THM43-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (= (SYMDIFF (SYMDIFF S T) U) (SYMDIFF S (SYMDIFF T U)))))))))))
 (help "Example 43 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-96
 (in tps)
 (conclusion BOOL-PROP-96-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (SYMDIFF X Y) (SETDIFF (UNION X Y) (INTERSECT X Y)))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-59
 (in tps)
 (conclusion BOOL-PROP-59-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (equiv (= (UNION X Y) NULLSET) (and (= X NULLSET) (= Y NULLSET)))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM39
 (in tps)
 (conclusion THM39-conc
 (forall (lam (S (o i)) (forall (lam (R (o i)) (implies (= (UNION R S) (INTERSECT R S)) (SUBSET R S)))))))
 (help ""))

(th~defproblem BOOL-PROP-53
 (in tps)
 (conclusion BOOL-PROP-53-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (= (UNION (INTERSECT X Y) (INTERSECT X Z)) X) (SUBSET X (UNION Y Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem GAZING-THM26
 (in tps)
 (conclusion GAZING-THM26-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (= (UNION S T) (UNION T S))))))))
 (help "Example 26 from Barker-Plummer's paper about Gazing"))

(th~defproblem RUSSELL1
 (in tps)
 (conclusion RUSSELL1-conc
 (forall (lam (E (o i i)) (not (exists (lam (U i) (forall (lam (V i) (equiv (E V U) (not (E V V)))))))))))
 (help "One form of Russell's Paradox"))

(th~defproblem THM38
 (in tps)
 (conclusion THM38-conc
 (forall (lam (S (o i)) (forall (lam (R (o i)) (implies (= (INTERSECT R S) (UNION R S)) (SUBSET R S)))))))
 (help "EASY INTERACTIVE PROOF  
20-JUN-77 18:36:36  
DATES  "))

(th~defproblem BOOL-PROP-80
 (in tps)
 (conclusion BOOL-PROP-80-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (UNION (INTERSECT X Y) (SETDIFF X Y)) X)))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM37
 (in tps)
 (conclusion THM37-conc
 (forall (lam (S (o i)) (forall (lam (R (o i)) (implies (= R (INTERSECT R S)) (SUBSET R S)))))))
 (help "COMPLETE / AND EASY INTERACTIVE PROOF  
13-JUN-77 11:36:50  
DATES  "))

(th~defproblem GAZING-THM12
 (in tps)
 (conclusion GAZING-THM12-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (implies (SUBSET S T) (= S (DIFF T (DIFF T S))))))))))
 (help "Example 12 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-113
 (in tps)
 (conclusion BOOL-PROP-113-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (MEETS X (SETDIFF Y Z)) (MEETS X Y))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem Y2141
 (in tps)
 (conclusion Y2141-conc
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (implies (forall (lam (|x| i) (exists (lam (|y| i) (and (P |x|) (or (Q |y|) (Q |x|))))))) (exists (lam (|z| i) (and (P |z|) (Q |z|))))))))))
 (help "Proposed additional exercise"))

(th~defproblem GAZING-THM38
 (in tps)
 (conclusion GAZING-THM38-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (equiv (= (UNION S T) S) (SUBSET T S))))))))
 (help "Example 38 from Barker-Plummer's paper about Gazing"))

(th~defproblem THM588LEM2
 (in tps)
 (conclusion THM588LEM2-conc
 (forall (lam (|g| (i i)) (forall (lam (|f| (i i)) (forall (lam (|h| (i i)) (implies (forall (lam (|x| i) (forall (lam (|y| i) (implies (= (|g| |x|) |y|) (= (|h| |y|) (|f| |x|))))))) (= (COMPOSE |h| |g|) |f|)))))))))
 (help "Another possible lemma for THM588, for manipulating composite functions."))

(th~defproblem THM36
 (in tps)
 (conclusion THM36-conc
 (forall (lam (S (o i)) (forall (lam (R (o i)) (implies (= R S) (SUBSET R S)))))))
 (help "A TRIVIAL THEOREM WITH AN EASY PROOF"))

(th~defproblem CT25
 (in tps)
 (conclusion CT25-conc
 (not (forall (lam (|x| o) (forall (lam (|y| o) (= |x| |y|)))))))
 (help ""))

(th~defproblem CT15
 (in tps)
 (conclusion CT15-conc
 (exists (lam (|x| o) (exists (lam (|y| o) (or |x| |y|))))))
 (help ""))

(th~defproblem BOOL-PROP-92
 (in tps)
 (conclusion BOOL-PROP-92-conc
  (all-types a (forall (lam (X (o a)) (and (= (SYMDIFF X NULLSET) X) (= (SYMDIFF NULLSET X) X))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM34A
 (in tps)
 (conclusion THM34A-conc
  (all-types b a (forall (lam (F (a b)) (forall (lam (S (o b)) (forall (lam (R (o b)) (SUBSET (IN-IMAGE F (UNION R S)) (UNION (IN-IMAGE F R) (IN-IMAGE F S)))))))))))
 (help "THE CONVERSE INCLUSION IS ALSO A THEOREM"))

(th~defproblem BLEDSOE-FENG-7
 (in tps)
 (conclusion BLEDSOE-FENG-7-conc
 (forall (lam (|a| i) (exists (lam (A (o i)) (not (A |a|)))))))
 (help "Set Var paper in JAR, p. 299, Example 7: BLEDSOE-FENG-7 (Chad created this, to distinguish it from BLEDSOE7, apparently from another source)
         (DOES NOT REQUIRE A PRIMSUB)
         Extremely trivial!  Just says there is some set that doesn't contain a given object a -- so the empty set works.
"))

(th~defproblem THM102
 (in tps)
 (conclusion THM102-conc
 (forall (lam (|a| i) (forall (lam (|b| i) (forall (lam (R (o i i)) (implies (and (forall (lam (|x| i) (R |x| |b|))) (forall (lam (|y| i) (implies (exists (lam (|z| i) (R |y| |z|))) (R |a| |y|))))) (exists (lam (|u| i) (forall (lam (|v| i) (R |u| |v|)))))))))))))
 (help "EXERCISE X2114 FROM LOGIC NOTES  
 
REMARK  "))

(th~defproblem THM35
 (in tps)
 (conclusion THM35-conc
 (forall (lam (S (o i)) (forall (lam (R (o i)) (implies (= (INTERSECT R S) R) (SUBSET R S)))))))
 (help "09-JUN-77 01:06:33  
DATES  "))

(th~defproblem GAZING-THM24
 (in tps)
 (conclusion GAZING-THM24-conc
  (all-types a (forall (lam (S (o a)) (= (INTERSECT S (lam (|x| a) false)) (lam (|x| a) false))))))
 (help "Example 24 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-48
 (in tps)
 (conclusion BOOL-PROP-48-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (forall (lam (V (o a)) (implies (and (SUBSET X Y) (SUBSET Z V)) (SUBSET (SETDIFF X V) (SETDIFF Y Z)))))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem PELL28
 (in tps)
 (conclusion PELL28-conc
 (forall (lam (F (o i)) (forall (lam (G (o i)) (forall (lam (P (o i)) (forall (lam (S (o i)) (forall (lam (R (o i)) (forall (lam (Q (o i)) (implies (and (and (forall (lam (|x| i) (implies (P |x|) (forall (lam (|x^0| i) (Q |x^0|)))))) (implies (forall (lam (|x| i) (or (Q |x|) (R |x|)))) (exists (lam (|x| i) (and (Q |x|) (S |x|)))))) (implies (exists (lam (|x| i) (S |x|))) (forall (lam (|x| i) (implies (F |x|) (G |x|)))))) (forall (lam (|x| i) (implies (and (P |x|) (F |x|)) (G |x|))))))))))))))))))
 (help ""))

(th~defproblem PELL21
 (in tps)
 (conclusion PELL21-conc
 (forall (lam (F (o i)) (forall (lam (|p| o) (implies (and (exists (lam (|x| i) (implies |p| (F |x|)))) (exists (lam (|x| i) (implies (F |x|) |p|)))) (exists (lam (|x| i) (and (implies |p| (F |x|)) (implies (F |x|) |p|))))))))))
 (help ""))

(th~defproblem THM68A
 (in tps)
 (conclusion THM68A-conc
 (forall (lam (LYLE i) (forall (lam (BRUCE i) (forall (lam (LIKES (o i i)) (implies (and (forall (lam (X i) (LIKES X BRUCE))) (forall (lam (Y i) (implies (exists (lam (Z i) (LIKES Y Z))) (LIKES LYLE Y))))) (exists (lam (U i) (forall (lam (V i) (LIKES U V)))))))))))))
 (help "IF EVERYONE LIKES BRUCE AND LYLE LIKES EVERYONE WHO LIKES SOMEONE  
THEN SOMEONE LIKES EVERYONE"))

(th~defproblem BOOL-PROP-102
 (in tps)
 (conclusion BOOL-PROP-102-conc
  (all-types a (forall (lam (Z (o a)) (forall (lam (X (o a)) (forall (lam (Y (o a)) (implies (MEETS X (INTERSECT Y Z)) (MEETS X Y))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM34
 (in tps)
 (conclusion THM34-conc
  (all-types b a (forall (lam (F (a b)) (forall (lam (S (o b)) (forall (lam (R (o b)) (EQUIVS (IN-IMAGE F (UNION R S)) (UNION (IN-IMAGE F R) (IN-IMAGE F S)))))))))))
 (help ""))

(th~defproblem BOOL-PROP-75
 (in tps)
 (conclusion BOOL-PROP-75-conc
  (all-types a (forall (lam (X (o a)) (= (SETDIFF NULLSET X) NULLSET)))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM69
 (in tps)
 (conclusion THM69-conc
 (forall (lam (G (i i)) (forall (lam (P (o i i)) (forall (lam (M (o i)) (forall (lam (F (i i i)) (forall (lam (Q (o i)) (implies (and (and (forall (lam (X i) (implies (exists (lam (Y i) (P X Y))) (forall (lam (Z i) (P Z Z)))))) (forall (lam (R i) (exists (lam (S i) (or (P R S) (and (M R) (Q (F R S))))))))) (forall (lam (W i) (implies (Q W) (not (M (G W))))))) (forall (lam (U i) (exists (lam (V i) (and (P (G U) V) (P U U))))))))))))))))))
 (help "PROOF REQUIRES NESTED QUANTIFIER DUPLICATIONS"))

(th~defproblem THM67A
 (in tps)
 (conclusion THM67A-conc
  (all-types a (forall (lam (G (o a (o a))) (forall (lam (F (o a (o a))) (implies (and (forall (lam (S (o a)) (forall (lam (T (o a)) (implies (SUBSET S T) (SUBSET (F T) (F S))))))) (forall (lam (S (o a)) (and (SUBSET S (F (G S))) (SUBSET S (G (F S))))))) (forall (lam (S (o a)) (EQUIVS (F (G (F S))) (F S)))))))))))
 (help "LIKE THM67 BUT MUCH HARDER BECAUSE QUANTIFIER WITH CONJUNCTION IN SCOPE HAS TO BE DUPLICATED"))

(th~defproblem BOOL-PROP-118
 (in tps)
 (conclusion BOOL-PROP-118-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (equiv (MISSES X Y) (= (INTERSECT X Y) NULLSET))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-120
 (in tps)
 (conclusion BOOL-PROP-120-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (and (SUBSET X (UNION Y Z)) (= (INTERSECT X Z) NULLSET)) (SUBSET X Y))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-32
 (in tps)
 (conclusion BOOL-PROP-32-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (and (SUBSET X Z) (SUBSET Y Z)) (SUBSET (UNION X Y) Z))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem GAZING-THM9
 (in tps)
 (conclusion GAZING-THM9-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (implies (and (= S T) (= T U)) (and (and (SUBSET S T) (SUBSET T U)) (SUBSET U S)))))))))))
 (help "Example 9 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-61
 (in tps)
 (conclusion BOOL-PROP-61-conc
  (all-types a (forall (lam (X (o a)) (= (INTERSECT X NULLSET) NULLSET)))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM68
 (in tps)
 (conclusion THM68-conc
 (forall (lam (LYLE i) (forall (lam (BRUCE i) (forall (lam (LIKES (o i i)) (implies (and (forall (lam (X i) (LIKES X BRUCE))) (forall (lam (Y i) (implies (exists (lam (Z i) (LIKES Y Z))) (LIKES LYLE Y))))) (exists (lam (U i) (forall (lam (V i) (LIKES U V)))))))))))))
 (help ""))

(th~defproblem GAZING-THM34
 (in tps)
 (conclusion GAZING-THM34-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (implies (and (SUBSET S U) (SUBSET T U)) (equiv (SUBSET S T) (= (UNION (DIFF U S) T) U)))))))))))
 (help "Example 34 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-87
 (in tps)
 (conclusion BOOL-PROP-87-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (SETDIFF (UNION X Y) (INTERSECT X Y)) (UNION (SETDIFF X Y) (SETDIFF Y X)))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM31
 (in tps)
 (conclusion THM31-conc
 (forall (lam (S (o i)) (forall (lam (R (o i)) (equiv (SUBSET R S) (= (INTERSECT R S) R)))))))
 (help ""))

(th~defproblem CT616
 (in tps)
 (conclusion CT616-conc
 (exists (lam (|x| o) (exists (lam (|y| o) |y|)))))
 (help ""))

(th~defproblem GAZING-THM20
 (in tps)
 (conclusion GAZING-THM20-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (SUBSET (INTERSECT S T) S)))))))
 (help "Example 20 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-44
 (in tps)
 (conclusion BOOL-PROP-44-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (SUBSET X Z) (= (UNION X (INTERSECT Y Z)) (INTERSECT (UNION X Y) Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM66
 (in tps)
 (conclusion THM66-conc
 (forall (lam (P (o i)) (equiv (exists (lam (X i) (forall (lam (Y i) (implies (P X) (P Y)))))) (exists (lam (X i) (implies (P X) (forall (lam (Y i) (P Y))))))))))
 (help ""))

(th~defproblem CT28
 (in tps)
 (conclusion CT28-conc
  (all-types a (not (exists (lam (|g| (o a a)) (forall (lam (|f| (o a)) (exists (lam (|j| a) (= (|g| |j|) |f|))))))))))
 (help "Cantorsatz"))

(th~defproblem CT18
 (in tps)
 (conclusion CT18-conc
 (exists (lam (|y| o) (forall (lam (|z| o) (exists (lam (|x| o) (implies |z| |x|))))))))
 (help ""))

(th~defproblem GAZING-THM46
 (in tps)
 (conclusion GAZING-THM46-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (equiv (= S T) (= (SYMDIFF T S) (lam (|x| a) false)))))))))
 (help "Example 46 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-71
 (in tps)
 (conclusion BOOL-PROP-71-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (UNION X (INTERSECT Y Z)) (INTERSECT (UNION X Y) (UNION X Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM65
 (in tps)
 (conclusion THM65-conc
 (forall (lam (Q (o i i)) (forall (lam (R (o i i)) (implies (forall (lam (W i) (not (R W W)))) (exists (lam (X i) (exists (lam (Y i) (and (not (R X Y)) (implies (Q Y X) (forall (lam (Z i) (Q Z Z)))))))))))))))
 (help "1979-OCT-16 INTERACTIVE MIPAS PROOF  
 
DATES  "))

(th~defproblem GAZING-THM3
 (in tps)
 (conclusion GAZING-THM3-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (implies (and (SUBSET S T) (SUBSET T U)) (SUBSET S U))))))))))
 (help "Example 3 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-30
 (in tps)
 (conclusion BOOL-PROP-30-conc
  (all-types a (forall (lam (X (o a)) (implies (SUBSET X NULLSET) (= X NULLSET))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM44-AGAIN
 (in tps)
 (conclusion THM44-AGAIN-conc
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (equiv (exists (lam (S (o i)) (forall (lam (X i) (and (or (S X) (P X)) (or (not (S X)) (Q X))))))) (forall (lam (Y i) (or (P Y) (Q Y))))))))))
 (help ""))

(th~defproblem DMG8
 (in tps)
 (conclusion DMG8-conc
 (forall (lam (|s| o) (forall (lam (|t| o) (forall (lam (|q| o) (forall (lam (|p| o) (implies (or (or (not (or (not (and (or (not (and (or (not (and |p| |q|)) (not (and |s| |t|))) |q|)) (not (and |s| |t|))) |q|)) (not (and |s| |t|)))) (not |q|)) (or (not |s|) (not |t|))) (or (not (and (or (not (and (or (not (and (or (not (and |p| |q|)) (not (and |s| |t|))) |q|)) (not (and |s| |t|))) |q|)) (not (and |s| |t|))) |q|)) (not (and |s| |t|)))))))))))))
 (help "A propositional problem suggested by Jay Hunschel Kim"))

(th~defproblem BOOL-PROP-89
 (in tps)
 (conclusion BOOL-PROP-89-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (SETDIFF (UNION X Y) Z) (UNION (SETDIFF X Z) (SETDIFF Y Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-56
 (in tps)
 (conclusion BOOL-PROP-56-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (equiv (= X (UNION Y Z)) (and (and (SUBSET Y X) (SUBSET Z X)) (forall (lam (V (o a)) (implies (and (SUBSET Y V) (SUBSET Z V)) (SUBSET X V))))))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM64
 (in tps)
 (conclusion THM64-conc
 (forall (lam (P (o i)) (exists (lam (X i) (implies (P X) (forall (lam (Y i) (P Y)))))))))
 (help "1979 OCT 6 INTERACTIVE MIPAS PROOF  
 
DATES  "))

(th~defproblem GAZING-THM30
 (in tps)
 (conclusion GAZING-THM30-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (= (UNION S (INTERSECT T U)) (INTERSECT (UNION S T) (UNION S U)))))))))))
 (help "Example 30 from Barker-Plummer's paper about Gazing"))

(th~defproblem CT313
 (in tps)
 (conclusion CT313-conc
 (exists (lam (|z| o) (exists (lam (|x| o) (forall (lam (|y| o) (implies |x| |y|))))))))
 (help ""))

(th~defproblem BOOL-PROP-83
 (in tps)
 (conclusion BOOL-PROP-83-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (SETDIFF (UNION X Y) Y) (SETDIFF X Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM63
 (in tps)
 (conclusion THM63-conc
 (forall (lam (P (o i i)) (implies (forall (lam (U i) (forall (lam (V i) (forall (lam (W i) (or (P U V) (P V W)))))))) (exists (lam (X i) (forall (lam (Y i) (P X Y)))))))))
 (help "REALLY EASY BUT REQUIRES QUANTIFIER DUPLICATION"))

(th~defproblem GAZING-THM15
 (in tps)
 (conclusion GAZING-THM15-conc
  (all-types a (forall (lam (S (o a)) (implies (SUBSET S (lam (|x| a) false)) (= S (lam (|x| a) false)))))))
 (help "Example 15 from Barker-Plummer's paper about Gazing"))

(th~defproblem THM62
 (in tps)
 (conclusion THM62-conc
 (forall (lam (B i) (forall (lam (A i) (forall (lam (P (o i i)) (implies (or (forall (lam (U i) (P A U))) (forall (lam (V i) (P V B)))) (exists (lam (X i) (P X X)))))))))))
 (help ""))

(th~defproblem BOOL-PROP-40
 (in tps)
 (conclusion BOOL-PROP-40-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (SUBSET X Y) (SUBSET (INTERSECT X Z) (INTERSECT Y Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-68
 (in tps)
 (conclusion BOOL-PROP-68-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (INTERSECT X (UNION X Y)) X)))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-114
 (in tps)
 (conclusion BOOL-PROP-114-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (and (and (SUBSET X Y) (SUBSET X Z)) (MISSES Y Z)) (= X NULLSET))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM123
 (in tps)
 (conclusion THM123-conc
 (forall (lam (|p| o) (exists (lam (|f| (o o)) (|f| |p|))))))
 (help "Trivial theorem to test TPS"))

(th~defproblem GAZING-THM42
 (in tps)
 (conclusion GAZING-THM42-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (= (SYMDIFF S T) (SYMDIFF T S))))))))
 (help "Example 42 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-95
 (in tps)
 (conclusion BOOL-PROP-95-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (UNION X Y) (UNION (SYMDIFF X Y) (INTERSECT X Y)))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BLEDSOE-FENG-8
 (in tps)
 (conclusion BLEDSOE-FENG-8-conc
 (forall (lam (|-2| i) (forall (lam (|absval| (i i)) (forall (lam (|0| i) (forall (lam (< (o i i)) (implies (< |-2| |0|) (implies (forall (lam (|u| i) (forall (lam (|v| i) (implies (< |u| |0|) (not (= |u| (|absval| |v|)))))))) (exists (lam (A (o i)) (and (forall (lam (|y| i) (not (A (|absval| |y|))))) (A |-2|)))))))))))))))
 (help "Set Var JAR paper, p. 299, Example 8: BLEDSOE-FENG-8 (note that we have to include -2 < 0 in the hyp)
There is a set containing no nonnegative numbers and containing -2.
Bledsoe's solution is the set of negative numbers, by saying the set of z not equal to any [absval y]"))

(th~defproblem CT31
 (in tps)
 (conclusion CT31-conc
  (all-types a (not (exists (lam (|g| (o a a)) (forall (lam (|f| (o a)) (exists (lam (|j| a) (= (= (|g| |j|) |f|) (= |f| |f|)))))))))))
 (help "cantorsatz, kompliziert formuliert"))

(th~defproblem CT21
 (in tps)
 (conclusion CT21-conc
 (exists (lam (|p| (o o)) (|p| (forall (lam (|y| o) (and |y| (not |y|))))))))
 (help ""))

(th~defproblem CT11
 (in tps)
 (conclusion CT11-conc
 (forall (lam (|y| o) (exists (lam (|y^0| o) |y^0|)))))
 (help ""))

(th~defproblem THM6
 (in tps)
 (conclusion THM6-conc
 (forall (lam (S (i i)) (implies (forall (lam (M i) (not (= (S M) M)))) (not (exists (lam (G (i i i)) (forall (lam (F (i i)) (exists (lam (J i) (= (G J) F))))))))))))
 (help "COMPLETE AUTOMATIC PROOF  
CORE 90000  
COST $ 34.0  
29-DEC-77 16:18:32  
DATES  "))

(th~defproblem BOOL-PROP-25
 (in tps)
 (conclusion BOOL-PROP-25-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (forall (lam (|x| a) (equiv (equiv (not (X |x|)) (Y |x|)) (Z |x|)))) (= X (SYMDIFF Y Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem ARR-COM-DMG5
 (in tps)
 (conclusion ARR-COM-DMG5-conc
 (forall (lam (|s| o) (forall (lam (|t| o) (forall (lam (|p| o) (forall (lam (|c| o) (forall (lam (|d| o) (forall (lam (|b| o) (forall (lam (|a| o) (implies (or (and |p| (or (or (not |a|) (not |b|)) (or (not |c|) (not |d|)))) (implies |s| |t|)) (or (and (or (not (and |a| |b|)) (not (and |c| |d|))) |p|) (implies |s| |t|))))))))))))))))))
 (help "A propositional problem suggested by Jay Hunschel Kim"))

(th~defproblem THM29A
 (in tps)
 (conclusion THM29A-conc
  (all-types c b a (forall (lam (F (a b)) (forall (lam (S (o c)) (forall (lam (G (b c)) (SUBSET (IN-IMAGE F (IN-IMAGE G S)) (IN-IMAGE (COMPOSE F G) S))))))))))
 (help ""))

(th~defproblem PELL42
 (in tps)
 (conclusion PELL42-conc
 (forall (lam (F (o i i)) (not (exists (lam (|y| i) (forall (lam (|x| i) (and (implies (F |x| |y|) (not (exists (lam (|z| i) (and (F |x| |z|) (F |z| |x|)))))) (implies (not (exists (lam (|z| i) (and (F |x| |z|) (F |z| |x|))))) (F |x| |y|)))))))))))
 (help "there is no set of non-circular sets [where
a circular set is a set x s.t. there is a set y, s.t. x belongs to y and 
reversely"))

(th~defproblem BOOL-PROP-52
 (in tps)
 (conclusion BOOL-PROP-52-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (SUBSET X (UNION Y Z)) (and (SUBSET (SETDIFF X Y) Z) (SUBSET (SETDIFF X Z) Y)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM34B
 (in tps)
 (conclusion THM34B-conc
  (all-types a b (forall (lam (S (o b)) (forall (lam (R (o b)) (forall (lam (F (a b)) (SUBSET (UNION (IN-IMAGE F R) (IN-IMAGE F S)) (IN-IMAGE F (UNION R S)))))))))))
 (help ""))

(th~defproblem THM120A
 (in tps)
 (conclusion THM120A-conc
 (exists (lam (R (o (o i) (o i))) (and (not (R (lam (|x| i) true) (lam (|x| i) false))) (TRANSITIVE R)))))
 (help "Variant of THM120 designed to eliminate trivial proof. Subset is one such relation."))

(th~defproblem BAFFLER-VARIANT
 (in tps)
 (conclusion BAFFLER-VARIANT-conc
 (forall (lam (|f| (i i i)) (forall (lam (P (o i)) (exists (lam (|y| i) (forall (lam (|x| i) (implies (P |y|) (P (|f| |y| |x|))))))))))))
 (help "A variant of X2119, where the exp var
shows up in the second exp term."))

(th~defproblem BOOL-PROP-78
 (in tps)
 (conclusion BOOL-PROP-78-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (INTERSECT (SETDIFF X Y) Y) NULLSET)))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem X2108B
 (in tps)
 (conclusion X2108B-conc
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (forall (lam (|x| i) (forall (lam (|y| i) (exists (lam (|u| i) (exists (lam (|v| i) (implies (and (P |x|) (Q |y|)) (and (P |u|) (Q |v|))))))))))))))))
 (help "enhanced version of x2108"))

(th~defproblem THM29
 (in tps)
 (conclusion THM29-conc
  (all-types c b a (forall (lam (F (a b)) (forall (lam (S (o c)) (forall (lam (G (b c)) (EQUIVS (IN-IMAGE F (IN-IMAGE G S)) (IN-IMAGE (COMPOSE F G) S))))))))))
 (help "14 CLAUSES 30 VPATHS  
14 PATHS 30 CLAUSES  
REQUIRES NO QUANTIFIER DUPLICATION  
NEXTBADPATH1 SHOULD WORK BETTER THAN NEXTBADPATH3 ON THIS PROBLEM  
EASILY PROVED WIHT THE = HEURISTIC"))

(th~defproblem GAZING-THM11
 (in tps)
 (conclusion GAZING-THM11-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (implies (and (and (SUBSET S U) (SUBSET T U)) (SUBSET S T)) (SUBSET (DIFF U T) (DIFF U S)))))))))))
 (help "Example 11 from Barker-Plummer's paper about Gazing"))

(th~defproblem PELL29
 (in tps)
 (conclusion PELL29-conc
 (forall (lam (G (o i)) (forall (lam (J (o i)) (forall (lam (H (o i)) (forall (lam (F (o i)) (implies (and (exists (lam (|x| i) (F |x|))) (exists (lam (|x| i) (G |x|)))) (and (implies (and (forall (lam (|x| i) (implies (F |x|) (H |x|)))) (forall (lam (|x| i) (implies (G |x|) (J |x|))))) (forall (lam (|x| i) (forall (lam (|y| i) (implies (and (F |x|) (G |y|)) (and (H |x|) (J |y|)))))))) (implies (forall (lam (|x| i) (forall (lam (|y| i) (implies (and (F |x|) (G |y|)) (and (H |x|) (J |y|))))))) (forall (lam (|x| i) (implies (G |x|) (J |x|))))))))))))))))
 (help ""))

(th~defproblem BOOL-PROP-35
 (in tps)
 (conclusion BOOL-PROP-35-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (implies (SUBSET X Y) (SUBSET (UNION X Y) Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-64
 (in tps)
 (conclusion BOOL-PROP-64-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (UNION (UNION X Y) Z) (UNION X (UNION Y Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM28
 (in tps)
 (conclusion THM28-conc
  (all-types c b a (forall (lam (F (a b)) (forall (lam (G (b c)) (forall (lam (S (o c)) (SUBSET (IN-IMAGE (COMPOSE F G) S) (IN-IMAGE F (IN-IMAGE G S)))))))))))
 (help ""))

(th~defproblem THM123B
 (in tps)
 (conclusion THM123B-conc
 (forall (lam (|p| o) (exists (lam (|f| (o o)) (not (|f| |p|)))))))
 (help "Trivial theorem to test TPS"))

(th~defproblem THM100A
 (in tps)
 (conclusion THM100A-conc
  (all-types a b (forall (lam (D (b b)) (forall (lam (G (b b)) (forall (lam (W (b b)) (forall (lam (|xO^0| b) (forall (lam (L (b b)) (forall (lam (S a) (forall (lam (N a) (forall (lam (P (o b a a a a)) (implies (and (and (and (and (and (and (and (and (and (and (and (and (and (and (P S S S S |xO^0|) (forall (lam (T b) (implies (P S N S N T) (P N N S N (L T)))))) (forall (lam (T1 b) (implies (P N N S N T1) (P S N S N (L T1)))))) (forall (lam (T2 b) (implies (P S S N S T2) (P N S N S (L T2)))))) (forall (lam (T3 b) (implies (P N S N S T3) (P S S N S (L T3)))))) (forall (lam (T4 b) (implies (P S S S N T4) (P N N S N (W T4)))))) (forall (lam (T5 b) (implies (P N N S N T5) (P S S S N (W T5)))))) (forall (lam (T6 b) (implies (P S S N S T6) (P N N N S (W T6)))))) (forall (lam (T7 b) (implies (P N N N S T7) (P S S N S (W T7)))))) (forall (lam (X a) (forall (lam (Y a) (forall (lam (U b) (implies (P S X S Y U) (P N X N Y (G U)))))))))) (forall (lam (X1 a) (forall (lam (Y1 a) (forall (lam (V b) (implies (P N X1 N Y1 V) (P S X1 S Y1 (G V)))))))))) (forall (lam (T8 b) (implies (P S N S S T8) (P N N S N (D T8)))))) (forall (lam (T9 b) (implies (P N N S N T9) (P S N S S (D T9)))))) (forall (lam (U1 b) (implies (P S S N S U1) (P N S N N (D U1)))))) (forall (lam (V1 b) (implies (P N S N N V1) (P S S N S (D V1)))))) (exists (lam (Z b) (P N N N N Z))))))))))))))))))))))
 (help "A NAIVE FORMALIZATION OF THE PROBLEM OF MOVING MAN WOLF GOAT CABBAGE FROM SOUTH TO NORTH SIDE OF RIVER  
BOAT HOLDS ONLY MAN AND ONE OTHER ITEM  
P SPECIFIES LOCATIONS OF MAN WOLF GOAT CABBAGE AND STATUS OF STATE VARIABLE  
L MEANS GO ALONE  
W MEANS TAKE WOLF  
G MEANS TAKE GOAT  
D MEANS TAKE CABBAGE  
NEEDS QUANTIFIERS DUPLICATED ONCE  
SEEMS TOO HARD BUT MIGHT BE FEASIBLE WITH A GOOD SET-OF-SUPPORT HEURISTIC  
 
REMARK  "))

(th~defproblem PELL19
 (in tps)
 (conclusion PELL19-conc
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (exists (lam (|x| i) (forall (lam (|y| i) (forall (lam (|z| i) (implies (implies (P |y|) (Q |z|)) (implies (P |x|) (Q |x|))))))))))))))
 (help ""))

(th~defproblem THM92
 (in tps)
 (conclusion THM92-conc
  (all-types a (forall (lam (|f| (a a)) (ITERATE |f| (COMPOSE |f| |f|))))))
 (help "Trivial theorem which gives nice simple example of
 an expansion proof."))

(th~defproblem GAZING-THM23
 (in tps)
 (conclusion GAZING-THM23-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (implies (= (UNION S T) S) (SUBSET T S))))))))
 (help "Example 23 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-47
 (in tps)
 (conclusion BOOL-PROP-47-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (SUBSET X Y) (SUBSET (SETDIFF Z Y) (SETDIFF Z X)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM26
 (in tps)
 (conclusion THM26-conc
 (implies (exists (lam (X i) (exists (lam (Y i) (not (= X Y)))))) (forall (lam (U i) (exists (lam (Z i) (not (= Z U))))))))
 (help "IN THE NATURAL INTUITIVE PROOF  
ONE CONSIDERS TWO CASES CONCERNING U  
WHEN FINDING Z "))

(th~defproblem COM-DMG02
 (in tps)
 (conclusion COM-DMG02-conc
 (forall (lam (|s| o) (forall (lam (|a| o) (forall (lam (|b| o) (forall (lam (|d| o) (forall (lam (|c| o) (implies (or (or (not (and |c| |d|)) (not (and |a| |b|))) |s|) (or |s| (or (not (and |a| |b|)) (not (and |c| |d|))))))))))))))))
 (help "A propositional problem suggested by Jay Hunschel Kim"))

(th~defproblem BLEDSOE3
 (in tps)
 (conclusion BLEDSOE3-conc
 (forall (lam (|a| i) (forall (lam (P (o i)) (implies (P |a|) (exists (lam (A (o i)) (and (forall (lam (|x| i) (implies (A |x|) (P |x|)))) (exists (lam (|y| i) (A |y|))))))))))))
 (help "Example 3 from Bledsoe, MI9"))

(th~defproblem CT14
 (in tps)
 (conclusion CT14-conc
 (exists (lam (|z| o) (exists (lam (|x| o) (= |x| |x|))))))
 (help ""))

(th~defproblem THM9
 (in tps)
 (conclusion THM9-conc
 (forall (lam (TH (i (i i))) (forall (lam (TM (i i i)) (implies (forall (lam (G (i i)) (= (TM (TH G)) G))) (forall (lam (F (i i)) (exists (lam (N i) (= (TM (F N)) (TM N))))))))))))
 (help "THIS IS A VERY NAIVE VERSION OF THE RECURSION THEOREM  
TM AND TH ARE CONSTANTS  
TM X Y  
IS THE OUTPUT OF TURING MACHINE X ON INPUT Y  
TH F  
IS THE NUMBER OF A TURING MACHINE WHICH COMPUTES FUNCTION F"))

(th~defproblem BOOL-PROP-74
 (in tps)
 (conclusion BOOL-PROP-74-conc
  (all-types a (forall (lam (X (o a)) (= (SETDIFF X NULLSET) X)))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM25
 (in tps)
 (conclusion THM25-conc
 (forall (lam (R (o i i)) (not (exists (lam (Y i) (forall (lam (X i) (equiv (R X Y) (not (exists (lam (Z i) (and (R X Z) (R Z X))))))))))))))
 (help "THIS IS QUINE'S MODIFICATION OF RUSSELL'S PARADOX.
    LET (R X Y) MEAN X IS IN Y."))

(th~defproblem GAZING-THM39
 (in tps)
 (conclusion GAZING-THM39-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (equiv (= (INTERSECT S T) S) (SUBSET S T))))))))
 (help "Example 39 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-110
 (in tps)
 (conclusion BOOL-PROP-110-conc
  (all-types a (forall (lam (X (o a)) (equiv (MEETS X X) (not (= X NULLSET)))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-31
 (in tps)
 (conclusion BOOL-PROP-31-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (SUBSET X (UNION X Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-60
 (in tps)
 (conclusion BOOL-PROP-60-conc
  (all-types a (forall (lam (X (o a)) (= (UNION X NULLSET) X)))))
 (help "From Boolean Properties of Sets"))

(th~defproblem GAZING-THM33
 (in tps)
 (conclusion GAZING-THM33-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (implies (and (SUBSET S U) (SUBSET T U)) (equiv (SUBSET S T) (= (INTERSECT S (DIFF U T)) (lam (|x| a) false))))))))))))
 (help "Example 33 from Barker-Plummer's paper about Gazing"))

(th~defproblem THM44-90-AGAIN
 (in tps)
 (conclusion THM44-90-AGAIN-conc
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (equiv (exists (lam (S (o i)) (forall (lam (X i) (and (or (S X) (P X)) (or (not (S X)) (Q X))))))) (forall (lam (Y i) (or (P Y) (Q Y))))))))))
 (help ""))

(th~defproblem BOOL-PROP-86
 (in tps)
 (conclusion BOOL-PROP-86-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (SETDIFF X (INTERSECT Y Z)) (UNION (SETDIFF X Y) (SETDIFF X Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM59
 (in tps)
 (conclusion THM59-conc
  (all-types a (forall (lam (Z (o a)) (forall (lam (Y (o a)) (forall (lam (X (o a)) (EQUIVS (UNION (INTERSECT X Y) Z) (INTERSECT (UNION X Z) (UNION Y Z)))))))))))
 (help ""))

(th~defproblem BOOL-PROP-49
 (in tps)
 (conclusion BOOL-PROP-49-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (SUBSET (SETDIFF X Y) X)))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM44-SLOW
 (in tps)
 (conclusion THM44-SLOW-conc
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (equiv (exists (lam (S (o i)) (forall (lam (X i) (and (or (S X) (P X)) (or (not (S X)) (Q X))))))) (forall (lam (Y i) (or (P Y) (Q Y))))))))))
 (help ""))

(th~defproblem PELL35
 (in tps)
 (conclusion PELL35-conc
 (forall (lam (P (o i i)) (exists (lam (|x| i) (exists (lam (|y| i) (implies (P |x| |y|) (forall (lam (|x^1| i) (forall (lam (|y^0| i) (P |x^1| |y^0|)))))))))))))
 (help ""))

(th~defproblem GAZING-THM18
 (in tps)
 (conclusion GAZING-THM18-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (SUBSET S (UNION S T))))))))
 (help "Example 18 from Barker-Plummer's paper about Gazing"))

(th~defproblem THM58
 (in tps)
 (conclusion THM58-conc
  (all-types a (forall (lam (Z (o a)) (forall (lam (Y (o a)) (forall (lam (X (o a)) (EQUIVS (UNION (INTERSECT X Y) Z) (INTERSECT (UNION X Z) (UNION Y Z)))))))))))
 (help "TRIVIAL THEOREM  
12 PATHS 24 CLAUSES WHEN MINIMIZE NO. PATHS "))

(th~defproblem PELL25
 (in tps)
 (conclusion PELL25-conc
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (forall (lam (F (o i)) (forall (lam (R (o i)) (forall (lam (G (o i)) (implies (and (and (and (exists (lam (|x| i) (P |x|))) (forall (lam (|x| i) (implies (F |x|) (and (not (G |x|)) (R |x|)))))) (forall (lam (|x| i) (implies (P |x|) (and (G |x|) (F |x|)))))) (or (forall (lam (|x| i) (implies (P |x|) (Q |x|)))) (exists (lam (|x| i) (and (P |x|) (R |x|)))))) (exists (lam (|x| i) (and (Q |x|) (P |x|))))))))))))))))
 (help ""))

(th~defproblem GAZING-THM45
 (in tps)
 (conclusion GAZING-THM45-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (SUBSET (DIFF T S) (SYMDIFF T S))))))))
 (help "Example 45 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-70
 (in tps)
 (conclusion BOOL-PROP-70-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (INTERSECT X (UNION Y Z)) (UNION (INTERSECT X Y) (INTERSECT X Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-98
 (in tps)
 (conclusion BOOL-PROP-98-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (SETDIFF X (SYMDIFF Y Z)) (UNION (SETDIFF X (UNION Y Z)) (INTERSECT X (INTERSECT Y Z))))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-115
 (in tps)
 (conclusion BOOL-PROP-115-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (and (SUBSET (SETDIFF X Y) Z) (SUBSET (SETDIFF Y X) Z)) (SUBSET (SYMDIFF X Y) Z))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem CT27
 (in tps)
 (conclusion CT27-conc
 (exists (lam (|x| o) (exists (lam (|y| o) (= (= |x| |y|) (= |y| |x|)))))))
 (help ""))

(th~defproblem CT17
 (in tps)
 (conclusion CT17-conc
 (forall (lam (|y| o) (exists (lam (|x| o) (implies |y| |y|))))))
 (help ""))

(th~defproblem BOOL-PROP-55
 (in tps)
 (conclusion BOOL-PROP-55-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (and (SUBSET X Y) (= (INTERSECT Y Z) NULLSET)) (= (INTERSECT X Z) NULLSET))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem GAZING-THM28
 (in tps)
 (conclusion GAZING-THM28-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (= (UNION (UNION S T) U) (UNION S (UNION T U)))))))))))
 (help "Example 28 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-82
 (in tps)
 (conclusion BOOL-PROP-82-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (SETDIFF X (SETDIFF X Y)) (INTERSECT X Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem DMG7
 (in tps)
 (conclusion DMG7-conc
 (forall (lam (|s| o) (forall (lam (|t| o) (forall (lam (|q| o) (forall (lam (|p| o) (implies (or (or (not (or (not (and (or (not (and |p| |q|)) (not (and |s| |t|))) |q|)) (not (and |s| |t|)))) (not |q|)) (or (not |s|) (not |t|))) (or (not (and (or (not (and (or (not (and |p| |q|)) (not (and |s| |t|))) |q|)) (not (and |s| |t|))) |q|)) (not (and |s| |t|)))))))))))))
 (help "A propositional problem suggested by Jay Hunschel Kim"))

(th~defproblem BOOL-PROP-38
 (in tps)
 (conclusion BOOL-PROP-38-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (SUBSET (INTERSECT X Y) (UNION X Z))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-67
 (in tps)
 (conclusion BOOL-PROP-67-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (INTERSECT (INTERSECT X Y) Z) (INTERSECT X (INTERSECT Y Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-104
 (in tps)
 (conclusion BOOL-PROP-104-conc
  (all-types a (forall (lam (X (o a)) (MISSES X NULLSET)))))
 (help "From Boolean Properties of Sets"))

(th~defproblem GAZING-THM41
 (in tps)
 (conclusion GAZING-THM41-conc
  (all-types a (forall (lam (S (o a)) (= (SYMDIFF S (lam (|x| a) false)) S)))))
 (help "Example 41 from Barker-Plummer's paper about Gazing"))

(th~defproblem GAZING-THM8
 (in tps)
 (conclusion GAZING-THM8-conc
  (all-types a (forall (lam (T (o a)) (forall (lam (S (o a)) (implies (forall (lam (S^0 (o a)) (forall (lam (T^0 (o a)) (= S^0 T^0))))) (and (SUBSET S T) (SUBSET T S)))))))))
 (help "Example 8 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-51
 (in tps)
 (conclusion BOOL-PROP-51-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (and (and (SUBSET X Y) (SUBSET X Z)) (= (INTERSECT Y Z) NULLSET)) (= X NULLSET))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM5
 (in tps)
 (conclusion THM5-conc
 (not (exists (lam (G (o i i)) (forall (lam (F (o i)) (exists (lam (J i) (= (G J) F)))))))))
 (help ""))

(th~defproblem CT20
 (in tps)
 (conclusion CT20-conc
 (not (forall (lam (|q| (o (o o))) (not (exists (lam (|p| (o o)) (|q| |p|))))))))
 (help ""))

(th~defproblem CT10
 (in tps)
 (conclusion CT10-conc
 (exists (lam (|x| o) (forall (lam (|y| o) |x|)))))
 (help ""))

(th~defproblem BOOL-PROP-77
 (in tps)
 (conclusion BOOL-PROP-77-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (SETDIFF X (INTERSECT X Y)) (SETDIFF X Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM147
 (in tps)
 (conclusion THM147-conc
 (forall (lam (T (o i)) (forall (lam (|nt| (i i)) (forall (lam (|imp| (i i i)) (not (and (and (and (and (forall (lam (|p| i) (forall (lam (|q| i) (or (or (not (T (|imp| |p| |q|))) (not (T |p|))) (T |q|)))))) (forall (lam (|p| i) (forall (lam (|q| i) (T (|imp| |p| (|imp| |q| |p|)))))))) (forall (lam (|p| i) (forall (lam (|q| i) (forall (lam (|r| i) (T (|imp| (|imp| |p| (|imp| |q| |r|)) (|imp| (|imp| |p| |q|) (|imp| |p| |r|))))))))))) (forall (lam (|p| i) (forall (lam (|q| i) (T (|imp| (|imp| (|nt| |p|) (|nt| |q|)) (|imp| |q| |p|)))))))) (exists (lam (|a| i) (not (T (|imp| |a| |a|))))))))))))))
 (help "Related to lcl077-1, but simpler;
theorem 211 on page 120 of Church's book"))

(th~defproblem THM86
 (in tps)
 (conclusion THM86-conc
  (all-types a b (forall (lam (|w| (o (o b))) (forall (lam (|f| (a b)) (SUBSET (IN-IMAGE |f| (SETINTERSECT |w|)) (SETINTERSECT (IN-IMAGE (IN-IMAGE |f|) |w|)))))))))
 (help "1982 FEB 27 ATTEMPT IN TPS1 FAILED  
UNIFICATION PROCESS GOT LOST  
DAM  
ATTEMP IN COMPLD WITH REALLOC 10000 1100 5000 5000 85000  
EVENTUALLY RAN OUT OF SPACE  
DAM  "))

(th~defproblem BOOL-PROP-69
 (in tps)
 (conclusion BOOL-PROP-69-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (UNION X (INTERSECT X Y)) X)))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM137
 (in tps)
 (conclusion THM137-conc
 (forall (lam (B i) (forall (lam (A i) (forall (lam (R (o i i)) (implies (forall (lam (X i) (R X A))) (exists (lam (Y i) (R B Y)))))))))))
 (help "trivial theorem for logic lessons"))

(th~defproblem BOOL-PROP-34
 (in tps)
 (conclusion BOOL-PROP-34-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (forall (lam (V (o a)) (implies (and (SUBSET X Y) (SUBSET Z V)) (SUBSET (UNION X Z) (UNION Y V)))))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM44-FAST
 (in tps)
 (conclusion THM44-FAST-conc
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (equiv (exists (lam (S (o i)) (forall (lam (X i) (and (or (S X) (P X)) (or (not (S X)) (Q X))))))) (forall (lam (Y i) (or (P Y) (Q Y))))))))))
 (help ""))

(th~defproblem GAZING-THM36
 (in tps)
 (conclusion GAZING-THM36-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (implies (and (SUBSET S U) (SUBSET T U)) (equiv (SUBSET S T) (SUBSET (INTERSECT S (DIFF U T)) T)))))))))))
 (help "Example 36 from Barker-Plummer's paper about Gazing"))

(th~defproblem THM127
 (in tps)
 (conclusion THM127-conc
 (forall (lam (P (o i)) (forall (lam (|f| (i i)) (forall (lam (|a| i) (forall (lam (|g| (i i)) (implies (= (COMPOSE |f| |g|) (COMPOSE |g| |f|)) (implies (P (|f| (|g| |a|))) (P (|g| (|f| |a|))))))))))))))
 (help ""))

(th~defproblem BOOL-PROP-90
 (in tps)
 (conclusion BOOL-PROP-90-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (implies (= (SETDIFF X Y) (SETDIFF Y X)) (= X Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-111
 (in tps)
 (conclusion BOOL-PROP-111-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (MISSES (INTERSECT X Y) (SETDIFF X Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-79
 (in tps)
 (conclusion BOOL-PROP-79-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (= (UNION X (SETDIFF Y X)) (UNION X Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem GAZING-THM22
 (in tps)
 (conclusion GAZING-THM22-conc
  (all-types a (forall (lam (S (o a)) (= (UNION S (lam (|x| a) false)) S)))))
 (help "Example 22 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-46
 (in tps)
 (conclusion BOOL-PROP-46-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (implies (SUBSET X Y) (SUBSET (SETDIFF X Z) (SETDIFF Y Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM100
 (in tps)
 (conclusion THM100-conc
  (all-types a b (forall (lam (C (b b)) (forall (lam (G (b b)) (forall (lam (W (b b)) (forall (lam (|xO^1| b) (forall (lam (A^0 (b b)) (forall (lam (S a) (forall (lam (N a) (forall (lam (P (o b a a a a)) (implies (and (and (and (and (and (and (and (and (and (and (and (and (and (and (P S S S S |xO^1|) (forall (lam (T b) (implies (P S N S N T) (P N N S N (A^0 T)))))) (forall (lam (T^1 b) (implies (P N N S N T^1) (P S N S N (A^0 T^1)))))) (forall (lam (T^2 b) (implies (P S S N S T^2) (P N S N S (A^0 T^2)))))) (forall (lam (T^3 b) (implies (P N S N S T^3) (P S S N S (A^0 T^3)))))) (forall (lam (T^4 b) (implies (P S S S N T^4) (P N N S N (W T^4)))))) (forall (lam (T^5 b) (implies (P N N S N T^5) (P S S S N (W T^5)))))) (forall (lam (T^6 b) (implies (P S S N S T^6) (P N N N S (W T^6)))))) (forall (lam (T^7 b) (implies (P N N N S T^7) (P S S N S (W T^7)))))) (forall (lam (X a) (forall (lam (Y a) (forall (lam (U b) (implies (P S X S Y U) (P N X N Y (G U)))))))))) (forall (lam (X^1 a) (forall (lam (Y^1 a) (forall (lam (V b) (implies (P N X^1 N Y^1 V) (P S X^1 S Y^1 (G V)))))))))) (forall (lam (T^8 b) (implies (P S N S S T^8) (P N N S N (C T^8)))))) (forall (lam (T^9 b) (implies (P N N S N T^9) (P S N S S (C T^9)))))) (forall (lam (U^1 b) (implies (P S S N S U^1) (P N S N N (C U^1)))))) (forall (lam (V^1 b) (implies (P N S N N V^1) (P S S N S (C V^1)))))) (exists (lam (Z b) (P N N N N Z))))))))))))))))))))))
 (help "A NAIVE FORMALIZATION OF THE PROBLEM OF MOVING MAN WOLF GOAT
     CABBAGE FROM SOUTH TO NORTH SIDE OF RIVER;
  BOAT HOLDS ONLY MAN AND ONE OTHER ITEM;
  P SPECIFIES LOCATIONS OF MAN WOLF GOAT CABBAGE AND STATUS OF STATE
     VARIABLE;
  A MEANS GO ALONE;
  W MEANS TAKE WOLF;
  G MEANS TAKE GOAT;
  C MEANS TAKE CABBAGE.
  NEEDS QUANTIFIERS DUPLICATED ONCE"))

(th~defproblem X3411
 (in tps)
 (conclusion X3411-conc
 (forall (lam (Q (o i i)) (forall (lam (P (o i i)) (implies (forall (lam (|y| i) (implies (forall (lam (|x| i) (P |x| |y|))) (forall (lam (|u| i) (Q |u| |y|)))))) (exists (lam (|z| i) (implies (exists (lam (|v| i) (P |v| |z|))) (exists (lam (|w| i) (or (P |z| |w|) (Q |w| |z|)))))))))))))
 (help ""))

(th~defproblem PELL26
 (in tps)
 (conclusion PELL26-conc
 (forall (lam (R (o i)) (forall (lam (S (o i)) (forall (lam (Q (o i)) (forall (lam (P (o i)) (implies (and (and (implies (exists (lam (|x| i) (P |x|))) (exists (lam (|x| i) (Q |x|)))) (implies (exists (lam (|x| i) (Q |x|))) (exists (lam (|x| i) (P |x|))))) (forall (lam (|x| i) (forall (lam (|y| i) (implies (and (P |x|) (Q |y|)) (and (implies (R |x|) (S |y|)) (implies (S |y|) (R |x|))))))))) (and (implies (forall (lam (|x| i) (implies (P |x|) (R |x|)))) (forall (lam (|x| i) (implies (Q |x|) (S |x|))))) (implies (forall (lam (|x| i) (implies (Q |x|) (S |x|)))) (forall (lam (|x| i) (implies (P |x|) (R |x|))))))))))))))))
 (help ""))

(th~defproblem BOOL-PROP-100
 (in tps)
 (conclusion BOOL-PROP-100-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (equiv (MEETS X (UNION Y Z)) (or (MEETS X Y) (MEETS X Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem CT23
 (in tps)
 (conclusion CT23-conc
 (forall (lam (|x| i) (exists (lam (|f| (i i)) (= (|f| |x|) |x|))))))
 (help ""))

(th~defproblem BOOL-PROP-58
 (in tps)
 (conclusion BOOL-PROP-58-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (SUBSET (SETDIFF X Y) (SYMDIFF X Y))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem GAZING-THM32
 (in tps)
 (conclusion GAZING-THM32-conc
  (all-types a (forall (lam (S (o a)) (= (UNION S S) S)))))
 (help "Example 32 from Barker-Plummer's paper about Gazing"))

(th~defproblem THM100-TPS2
 (in tps)
 (conclusion THM100-TPS2-conc
  (all-types a b (forall (lam (C (b b)) (forall (lam (G (b b)) (forall (lam (W (b b)) (forall (lam (|xO^2| b) (forall (lam (A^1 (b b)) (forall (lam (S a) (forall (lam (N a) (forall (lam (P (o b a a a a)) (implies (and (and (and (and (and (and (and (and (and (and (and (and (and (and (P S S S S |xO^2|) (forall (lam (T b) (implies (P S N S N T) (P N N S N (A^1 T)))))) (forall (lam (T1 b) (implies (P N N S N T1) (P S N S N (A^1 T1)))))) (forall (lam (T2 b) (implies (P S S N S T2) (P N S N S (A^1 T2)))))) (forall (lam (T3 b) (implies (P N S N S T3) (P S S N S (A^1 T3)))))) (forall (lam (T4 b) (implies (P S S S N T4) (P N N S N (W T4)))))) (forall (lam (T5 b) (implies (P N N S N T5) (P S S S N (W T5)))))) (forall (lam (T6 b) (implies (P S S N S T6) (P N N N S (W T6)))))) (forall (lam (T7 b) (implies (P N N N S T7) (P S S N S (W T7)))))) (forall (lam (X a) (forall (lam (Y a) (forall (lam (U b) (implies (P S X S Y U) (P N X N Y (G U)))))))))) (forall (lam (X1 a) (forall (lam (Y1 a) (forall (lam (V b) (implies (P N X1 N Y1 V) (P S X1 S Y1 (G V)))))))))) (forall (lam (T8 b) (implies (P S N S S T8) (P N N S N (C T8)))))) (forall (lam (T9 b) (implies (P N N S N T9) (P S N S S (C T9)))))) (forall (lam (U1 b) (implies (P S S N S U1) (P N S N N (C U1)))))) (forall (lam (V1 b) (implies (P N S N N V1) (P S S N S (C V1)))))) (exists (lam (Z b) (P N N N N Z))))))))))))))))))))))
 (help "1982-SEP-06 RAN OUT OF FREE STORAGE  
RUNCOUNT 1331.4718 CONSCOUNT 2774583  
SYSTEM TPS1  
GENVPATHMATES GENVPATHMATES4  
NEXTBADPATH NEXTBADPATH3  
BESTPAIRS1RST BESTPAIRS1RST3  
PATH-SEARCHLENGTH 5  
EQUIV-CHOICE T  
EQHEURISTIC  
INITDEPTH 2  
UNIDEPTH 3  
PRINTFLAG ARCS  
 
 
DATES  "))

(th~defproblem BOOL-PROP-85
 (in tps)
 (conclusion BOOL-PROP-85-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (SETDIFF X (UNION Y Z)) (INTERSECT (SETDIFF X Y) (SETDIFF X Z)))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-116
 (in tps)
 (conclusion BOOL-PROP-116-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (INTERSECT X (SETDIFF Y Z)) (SETDIFF (INTERSECT X Y) Z))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem CT265
 (in tps)
 (conclusion CT265-conc
 (exists (lam (|x| o) (forall (lam (|y| o) (= (= |x| |y|) (= |y| |x|)))))))
 (help ""))

(th~defproblem X2201TEST
 (in tps)
 (conclusion X2201TEST-conc
 (forall (lam (Q (o i i)) (forall (lam (R (o i)) (forall (lam (|y| i) (forall (lam (P (o i i i)) (equiv (not (implies (exists (lam (|x| i) (Q |x| |y|))) (not (forall (lam (|z| i) (implies (forall (lam (|u| i) (P |u| |y| |z|))) (not (exists (lam (|v| i) (R |v|)))))))))) (exists (lam (|x| i) (forall (lam (|z| i) (exists (lam (|u| i) (forall (lam (|v| i) (not (implies (Q |x| |y|) (not (implies (P |u| |y| |z|) (not (R |v|))))))))))))))))))))))))
 (help "shows x2201 equivalent to its prenex normal form"))

(th~defproblem LX1
 (in tps)
 (conclusion LX1-conc
 (forall (lam (Q (o i i)) (forall (lam (B i) (forall (lam (A i) (forall (lam (R (o i i)) (implies (and (and (R A B) (forall (lam (X i) (implies (exists (lam (Y i) (R X Y))) (Q X X))))) (forall (lam (U i) (forall (lam (V i) (implies (Q U V) (forall (lam (Z i) (R Z V))))))))) (exists (lam (W i) (and (R B W) (Q W A))))))))))))))
 (help ""))

(th~defproblem BOOL-PROP-42
 (in tps)
 (conclusion BOOL-PROP-42-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (implies (SUBSET X Y) (= (INTERSECT X Y) X))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem THM49
 (in tps)
 (conclusion THM49-conc
 (forall (lam (R o) (forall (lam (Q o) (forall (lam (P o) (or (or (equiv P Q) (equiv Q R)) (equiv P R)))))))))
 (help "REALLY TRIVIAL"))

(th~defproblem THM100B
 (in tps)
 (conclusion THM100B-conc
  (all-types a b (forall (lam (D (b b)) (forall (lam (G (b b)) (forall (lam (W (b b)) (forall (lam (|xO^3| b) (forall (lam (L (b b)) (forall (lam (S a) (forall (lam (N a) (forall (lam (P (o b a a a a)) (implies (and (and (and (and (and (and (and (and (and (and (and (and (and (and (P S S S S |xO^3|) (forall (lam (T b) (implies (P S N S N T) (P N N S N (L T)))))) (forall (lam (|t| b) (implies (P N N S N |t|) (P S N S N (L |t|)))))) (forall (lam (T b) (implies (P S S N S T) (P N S N S (L T)))))) (forall (lam (T b) (implies (P N S N S T) (P S S N S (L T)))))) (forall (lam (T b) (implies (P S S S N T) (P N N S N (W T)))))) (forall (lam (T b) (implies (P N N S N T) (P S S S N (W T)))))) (forall (lam (T b) (implies (P S S N S T) (P N N N S (W T)))))) (forall (lam (T b) (implies (P N N N S T) (P S S N S (W T)))))) (forall (lam (X a) (forall (lam (Y a) (forall (lam (U b) (implies (P S X S Y U) (P N X N Y (G U)))))))))) (forall (lam (X1 a) (forall (lam (Y1 a) (forall (lam (V b) (implies (P N X1 N Y1 V) (P S X1 S Y1 (G V)))))))))) (forall (lam (T b) (implies (P S N S S T) (P N N S N (D T)))))) (forall (lam (T b) (implies (P N N S N T) (P S N S S (D T)))))) (forall (lam (U1 b) (implies (P S S N S U1) (P N S N N (D U1)))))) (forall (lam (V1 b) (implies (P N S N N V1) (P S S N S (D V1)))))) (exists (lam (Z b) (P N N N N Z))))))))))))))))))))))
 (help "NIL"))

(th~defproblem GAZING-THM44
 (in tps)
 (conclusion GAZING-THM44-conc
  (all-types a (forall (lam (S (o a)) (forall (lam (T (o a)) (forall (lam (U (o a)) (= (INTERSECT S (SYMDIFF T U)) (SYMDIFF (INTERSECT S T) (INTERSECT S U)))))))))))
 (help "Example 44 from Barker-Plummer's paper about Gazing"))

(th~defproblem BOOL-PROP-97
 (in tps)
 (conclusion BOOL-PROP-97-conc
  (all-types a (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (= (SETDIFF (SYMDIFF X Y) Z) (UNION (SETDIFF X (UNION Y Z)) (SETDIFF Y (UNION X Z))))))))))))
 (help "From Boolean Properties of Sets"))

(th~defproblem BOOL-PROP-27
 (in tps)
 (conclusion BOOL-PROP-27-conc
  (all-types a (forall (lam (X (o a)) (SUBSET NULLSET X)))))
 (help "From Boolean Properties of Sets"))

