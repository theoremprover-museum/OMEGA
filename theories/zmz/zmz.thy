;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
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

(th~deftheory ZMZ
	      (uses group module)
	      (help "Theory for residueclass sets."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition Resclasse + Resclassen Menge              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+old(th~defdef resclass
	   (in zmz)
	   (definition (lam (n num)
			    (lam (m num)
				 (that (lam (s (o num)) 
					    (forall-sort (lam (x num)
							      (equiv (s x) (= (mod x n) m)))
							 int))))))
	   (help "Definition of residue classes."))

(th~defdef resclass
	   (in zmz)
	   (definition (lam (n num)
			    (lam (m num)
				 (lam (x num)
				      (and (int x) (= (mod x n) m))))))
	   (help "Definition of residue classes."))

;; Diese Definition der Resclassen uber Z schliest a priori auch nicht die Definition von (resclass 2 3) aus, allerdings ist
;; diese Resclasse einfach leer, da es bei mod 2 keine Reste von 3 gibt.
;; Desweiteren schliesst sie auch nicht aus folgendes hinzuschreiben: (resclass 0 2), allerdings ist auch diese Resclasse leer
;; da mod 0 nicht funktioniert

#+old(th~defdef resclass-set
	   (in zmz)
	   (definition (lam (n num)
			    (that (lam (s (o (o num)))
				       (forall-sort (lam (m num)
							 (equiv (s (resclass n m))
								(non-empty (resclass n m))))
						    nat)))))
	   (help "A set of residue classes of n is the set of residue classes of n that are not empty."))

(th~defdef resclass-set
	   (in zmz)
	   (definition (lam (n num)
			    (lam (r (o num))
				 (exists-sort (lam (m num)
						   (and (= r (resclass n m))
							(non-empty r)))
					      nat))))
	   (help "A set of residue classes of n is the set of residue classes of n that are not empty."))
;; Die Resclassen Menge ueber n ist die Menge aller Resclassen uber n die nicht leer sind



;; Ueber diese Definitionen muessen wir jetzt folgende Theoreme zeigen:

(th~deftheorem resclass-set-non-empty
	       (in zmz)
	       (conclusion (forall-sort (lam (n num)
					     (equiv (non-empty (resclass-set n))
						    (not (= n zero))))
					int)))
;; Die Resclassenmenge uber n ist genau dann nicht leer, wenn n integer ungleich 0 ist

(th~deftheorem resclass-set-th
	       (in zmz)
	       (conclusion (forall-sort (lam (n num)
					     (forall-sort (lam (m num)
							       (equiv ((resclass-set n) (resclass n m))
								      (less m (absval n))))
							  nat))
					int)))
;; Die Resclassenmenge uber n besteht genau aus den Restklassen von 0 ... |n|-1

(th~deftheorem resclass-element
	       (in zmz)
	       (conclusion (forall-sort (lam (n num)
					     (forall-sort (lam (m num)
							       (implies (non-empty (resclass n m))
									((resclass n m) m)))
							  nat))
					int)))
;; Resclasse n,m enthaelt - wenn sie nicht leer ist - mindestens m



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition plus, times, minus uber resclassen       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(th~defdef plus-resclass
	   (in zmz)

	   (definition
	     (lam (r1 (o num))
		  (lam (r2 (o num))
		       (lam (x3 num)
			    (exists-sort (lam (x1 num)
					      (exists-sort (lam (x2 num)
								(= x3 (plus x1 x2)))
							   r2))
					 r1)))))
	   (help "Plus for residue classes over the integers."))

;; Das Ergebnis von plus auf zwei Resclassen R1 und R2 ist die Resclasse, die fuer je zwei Element aus x1 und x2 aus R1 bzw. R2
;; ein Element x3 enthaelt mit x3 = x1 + x2.
;; Diese Addition ist nur sinnvoll auf Resclassen uber dem gleichen n, das werden wir aber anschliessend in Theoremen beweisen.


(th~defdef times-resclass
	   (in zmz)

	   (definition
	     (lam (r1 (o num))
		  (lam (r2 (o num))
		       (lam (x3 num)
			    (exists-sort (lam (x1 num)
					      (exists-sort (lam (x2 num)
								(= x3 (times x1 x2)))
							   r2))
					 r1)))))
	   (help "Times for residue classes over the integers."))
	   
;; Das Ergebnis von times auf zwei Resclassen R1 und R2 ist die Resclasse, die fuer je zwei Element aus x1 und x2 aus R1 bzw. R2
;; ein Element x3 enthaelt mit x3 = x1 * x2.
;; Diese Multiplication ist nur sinnvoll auf Resclassen uber dem gleichen n, das werden wir aber anschliessend in Theoremen beweisen.

(th~defdef minus-resclass
	   (in zmz)

	   (definition
	     (lam (r1 (o num))
		  (lam (r2 (o num))
		       (lam (x3 num)
			    (exists-sort (lam (x1 num)
					      (exists-sort (lam (x2 num)
								(= x3 (minus x1 x2)))
							   r2))
					 r1)))))
	   (help "Minus for residue classes over the integers."))

;; Das Ergebnis von minus auf zwei Resclassen R1 und R2 ist die Resclasse, die fuer je zwei Element aus x1 und x2 aus R1 bzw. R2
;; ein Element x3 enthaelt mit x3 = x1 - x2.
;; Diese Subtraction ist nur sinnvoll auf Resclassen uber dem gleichen n, das werden wir aber anschliessend in Theoremen beweisen.


;; Hierfuer muessen wir jetzt auch wieder folgende Theorem zeigen:

(th~deftheorem resclass-plus-closed
	       (in zmz)

	       (conclusion 
		(forall-sort (lam (n num)
				  (forall-sort (lam (r1 (o num))
						    (forall-sort (lam (r2 (o num))
								      ((resclass-set n) (plus-resclass r1 r2)))
								 (resclass-set n)))
					       (resclass-set n)))
			     int)))
;; Resclassen addition ist innerhalb einer Resclassenmenge ueber n definiert + abgeschlossen!

(th~deftheorem resclass-minus-closed
	       (in zmz)

	       (conclusion 
		(forall-sort (lam (n num)
				  (forall-sort (lam (r1 (o num))
						    (forall-sort (lam (r2 (o num))
								      ((resclass-set n) (minus-resclass r1 r2)))
								 (resclass-set n)))
					       (resclass-set n)))
			     int)))
;; Resclassen subtraktion ist innerhalb einer Resclassenmenge ueber n definiert + abgeschlossen!

(th~deftheorem resclass-times-closed
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (forall-sort (lam (r1 (o num))
						    (forall-sort (lam (r2 (o num))
								      ((resclass-set n) (times-resclass r1 r2)))
								 (resclass-set n)))
					       (resclass-set n)))
			     int)))
;; Resclassen multiplikation ist innerhalb einer Resclassenmenge ueber n definiert + abgeschlossen!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition des classen-rests einer Resclasse uber n     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defdef modulo-factor
	   (in zmz)

	   (definition
	     (lam (r (o num))
		  (that (lam (n num)
			     (exists (lam (m num)
					  (forall-sort (lam (x num)
							    (= (mod x n) m))
						       r))))))))
;; The modulo-factor of a residue class is that n, such that there exists an m such that for all x of the
;; residue class holds (= (mod x n) m)


(th~defdef class-residue
	   (in zmz)

	   (definition
	     (lam (r (o num))
		  (lam (n num)
		       (that (lam (m num)
				  (forall (lam (x num)
					       (implies (r x)
							(= (mod x n) m))))))))))
;; Der class-residue einer resclasse ist uber n definiert als der rest aller Element dieser Resclasse
;; Diese Definition macht nur Sinn, fuer resclassen uber n! (class-residue (resclass n' m) n) wird fehlschlagen!)
;; Das werden wir gleich ueberpruefen!

;; Note that class-residue is also definied for a (resclass n m) and n' such that n' devides n!
;; For instance, consider the reside class (resclass 6 5), that is 5_6.
;; (class-residue (resclass 6 5) 3) = 2 since (5,11,17, ... mod 3 = 2)!!!!

;; Darueber koennen wir jetzt auch wieder ein paar Saetze zeigen:

(th~deftheorem class-residue-exists
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (forall-sort (lam (r (o num))
						    (exists (lam (x num)
								 (and (= (class-residue r n) x)
								      (and (nat x)
									   (r x))))))
					       (resclass-set n)))
			     int)))
;; Fuer Resclasse existiert class-residue und ist in der Resclasse enthalten


(th~deftheorem class-residue-direct
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (forall-sort (lam (m num)
						    (implies (and (not (= n zero))
								  (less m (absval n)))
							     (= (class-residue (resclass n m) n) m)))
					       nat))
			     int)))
;; Fuer resclasse n,m ist class-residue = m

(th~deftheorem class-residue-properties
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (forall-sort (lam (r (o num))
						    (forall (lam (x num)
								 (implies (= (class-residue r n) x)
									  (and (r x)
									       (nat x))))))
					       (resclass-set n)))
			     int)))
;; Fuer den class-residue einer resclasse gilt, dass er in der Resclasse enthalten ist und in N ist


(th~deftheorem class-residue-main
	       (in zmz)

	       (conclusion
		(forall-sort (lam (n num)
				  (forall-sort (lam (r1 (o num))
						    (forall-sort (lam (r2 (o num))
								      (equiv (= r1 r2)
									     (= (class-residue r1 n) (class-residue r2 n))))
								 (resclass-set n)))
					       (resclass-set n)))
			     int)))
;; Das duerfte der Hauptsatz sein, mit dem sich resclass-plus, ... zurueckfuehren laesst auf plus, ... uber Z



;; Hiermit koennen wir jetzt folgende Theorem ueber die gewuenchten Eigenschaften der Addition, Multiplikation, Subtraktion uber
;; Resclassen zeigen:

(th~deftheorem resclass-plus-th
	       (in zmz)

	       (conclusion
	       (forall-sort (lam (n num)
		  (forall-sort (lam (r1 (o num))
		     (forall-sort (lam (r2 (o num))
			(forall (lam (m1 num)
			   (forall (lam (m2 num)
			      (implies (and (= (class-residue r1 n) m1)
					    (= (class-residue r2 n) m2))
				       (= (class-residue (plus-resclass r1 r2) n) (mod (plus m1 m2) n))))))))
				  (resclass-set n)))
			       (resclass-set n)))
			    int)))
;;; Das Ergebnisses von plus-resclass r1 und r2 mit class-residues m1 und m2 ueber n ist genau die Resclasse mit dem class-residue
;;; (mod (m1 + m2) n)

(th~deftheorem resclass-times-th
	       (in zmz)

	       (conclusion
	       (forall-sort (lam (n num)
		  (forall-sort (lam (r1 (o num))
		     (forall-sort (lam (r2 (o num))
			(forall (lam (m1 num)
			   (forall (lam (m2 num)
			      (implies (and (= (class-residue r1 n) m1)
					    (= (class-residue r2 n) m2))
				       (= (class-residue (times-resclass r1 r2) n) (mod (times m1 m2) n))))))))
				  (resclass-set n)))
			       (resclass-set n)))
			    int)))
;; Das Ergebnisses von times-resclass r1 und r2 mit class-residues m1 und m2 ueber n ist genau die Resclasse mit dem class-residue
;; (mod (m1 * m2) n)


(th~deftheorem resclass-minus-th
	       (in zmz)

	       (conclusion
	       (forall-sort (lam (n num)
		  (forall-sort (lam (r1 (o num))
		     (forall-sort (lam (r2 (o num))
			(forall (lam (m1 num)
			   (forall (lam (m2 num)
			      (implies (and (= (class-residue r1 n) m1)
					    (= (class-residue r2 n) m2))
				       (= (class-residue (minus-resclass r1 r2) n) (mod (minus m1 m2) n))))))))
				  (resclass-set n)))
			       (resclass-set n)))
			    int)))
;; Das Ergebnisses von minus-resclass r1 und r2 mit class-residues m1 und m2 ueber n ist genau die Resclasse mit dem class-residue
;; (mod (m1 - m2) n)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of generalized plus, times, minus  on residue classes       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; The operations plus-resclass, times-resclass, and minus-resclass are defined not on residue classes
;; modulo the same factor. We can generalize these operations as follows:
;; 
;; a_n+b_m=(a+b)_n if n divides m
;; a_n-b_m=(a-b)_n if n divides m
;; a_n*b_m=(a*b)_n if n divides m
;;
;; These operations are well-defined in the sense of: a=b and c=d => a op c = b op d
;;


(th~defdef plusgen-resclass
	   (in zmz)

	   (definition
	     (lam (r1 (o num))
		  (lam (r2 (o num))
		       (lam (x3 num)
			    (exists-sort (lam (x1 num)
					      (exists-sort (lam (x2 num)
								(= x3 (plus (mod x1 (modulo-factor r2)) x2)))
							   r2))
					 r1)))))
	   (help "Generalised Plus for residue classes over the integers."))

;;
;; The result of plusgen for two residue classes R1 and R2 is the set that contains for two elements
;; x1 and x2 in R1 and R2 an element x3 with x2 = (mod x (modulo-factor R2)) + x2.
;; The resulting set is a residue class if (modulo-factor R2) divides (modulo-factor R1)
;; The modulo-factir of the resulting residue class is (modulo-factor R2)
;; If (modulo-factor R2) = (modulo-factor R1), then plusgen-resclass equals plus-resclass

(th~defdef timesgen-resclass
	   (in zmz)

	   (definition
	     (lam (r1 (o num))
		  (lam (r2 (o num))
		       (lam (x3 num)
			    (exists-sort (lam (x1 num)
					      (exists-sort (lam (x2 num)
								(= x3 (times (mod x1 (modulo-factor r2)) x2)))
							   r2))
					 r1)))))
	   (help "Generalised Times for residue classes over the integers."))

;;
;; The result of timesgen for two residue classes R1 and R2 is the set that contains for two elements
;; x1 and x2 in R1 and R2 an element x3 with x2 = (mod x (modulo-factor R2)) * x2.
;; The resulting set is a residue class if (modulo-factor R2) divides (modulo-factor R1)
;; The modulo-factir of the resulting residue class is (modulo-factor R2)
;; If (modulo-factor R2) = (modulo-factor R1), then timesgen-resclass equals times-resclass


(th~defdef minusgen-resclass
	   (in zmz)

	   (definition
	     (lam (r1 (o num))
		  (lam (r2 (o num))
		       (lam (x3 num)
			    (exists-sort (lam (x1 num)
					      (exists-sort (lam (x2 num)
								(= x3 (minus  (mod x1 (modulo-factor r2)) x2)))
							   r2))
					 r1)))))
	   (help "Generalised Minus for residue classes over the integers."))


;;
;; The result of minusgen for two residue classes R1 and R2 is the set that contains for two elements
;; x1 and x2 in R1 and R2 an element x3 with x2 = (mod x (modulo-factor R2)) - x2.
;; The resulting set is a residue class if (modulo-factor R2) divides (modulo-factor R1)
;; The modulo-factir of the resulting residue class is (modulo-factor R2)
;; If (modulo-factor R2) = (modulo-factor R1), then minusgen-resclass equals minus-resclass




;; To use the function plusgen-resclass, minusgen-resclass, and timesgen-resclass we need the following theorems:

(th~deftheorem resclass-plusgen-th
	       (in zmz)

	       (conclusion
	       (forall-sort (lam (n1 num)
		(forall-sort (lam (m1 num)
                 (forall-sort (lam (n2 num)
		  (forall-sort (lam (m2 num)
			(implies (divisor n2 n1)
				 (= (plusgen-resclass (resclass n1 m1) (resclass n2 m2))
				    (plus-resclass (resclass n2 (mod m1 n2)) (resclass n2 m2)))))
			       int))
			      int))
			     int))
			    int)))

(th~deftheorem resclass-minusgen-th
	       (in zmz)

	       (conclusion
	       (forall-sort (lam (n1 num)
		(forall-sort (lam (m1 num)
                 (forall-sort (lam (n2 num)
		  (forall-sort (lam (m2 num)
			(implies (divisor n2 n1)
				 (= (minusgen-resclass (resclass n1 m1) (resclass n2 m2))
				    (minus-resclass (resclass n2 (mod m1 n2)) (resclass n2 m2)))))
			       int))
			      int))
			     int))
			    int)))

(th~deftheorem resclass-timesgen-th
	       (in zmz)

	       (conclusion
	       (forall-sort (lam (n1 num)
		(forall-sort (lam (m1 num)
                 (forall-sort (lam (n2 num)
		  (forall-sort (lam (m2 num)
			(implies (divisor n2 n1)
				 (= (timesgen-resclass (resclass n1 m1) (resclass n2 m2))
				    (times-resclass (resclass n2 (mod m1 n2)) (resclass n2 m2)))))
			       int))
			      int))
			     int))
			    int)))
