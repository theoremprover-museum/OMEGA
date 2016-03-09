;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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

(mod~defmod NATAC 
            :uses (data env keim node omega pds pdsn pos prob tacl term th)
            :documentation "Functions for the expansion of subproofs for arith. operations on Nat, Int, Rat."
            :exports (natac*hashtable
		      natac*hash-list

		      natac~num-p
		      natac~data-equal
		      natac~remove-hashes-of-proof-plan
		      
		      natac~expand-simplify-num
		      natac~expand-expand-num
		      natac~expand-arith-simplify

		      natac~expand-number
		      natac~expand-number*
		      natac~simplify-number
		      natac~simplify-number*

		      natac~compute
		      natac~reduction
		      natac~reduction-cont

		      natac~func2num-line
		      natac~func2num-proof
		      natac~num2func-line
		      natac~num2func-proof
		      ))



;;; --------------------------------------------------------------------------

#{
\section{Introduction}
This module serves the expansion of arithmetic computations carried out by
{\tt simplify-num}, {\tt expand-num} or {\tt arith-simplify}.
#}


#{
\section{Constants and Variables}
#}

#{
\subsection{Hashtables}
#}

(defvar natac*hashtable (make-hash-table :test #'equal)
  "A global hashtable that serves to find the hashtables for arithmetic simplifications of the current proof plan.")

(defconstant natac*hash-list
  (list 'theorems-hash
	'natural-numbers-hash
	'neg-natural-numbers-hash
	'integer-numbers-hash
	'rational-numbers-hash
	'change-sign-hash)
  "The hashtables in this list serve to find already derived proof lines in order to reuse
   obtained results rather than compute them again.
   theorems-hash stores the already imported theorems.
   The key is name of the theorem.
   natural-numbers-hash stores the lines which prove that certain numbers
   are natural.
   The key is the number as value.
   neg-natural-numbers-hash stores the lines which prove that certain numbers
   are negative integers.
   The key is the number as value.
   integer-numers-hash stores the lines which prove that certain numbers
   are integers.
   The key is the number as value.
   rational-numbers-hash stores the lines which prove that certain numbers
   are rationals.
   The key is the number as value.
   change-sign-hash stores the lines which prove that, for some integer x,
   (= (change-sign x) -x).
   For example: (= (change-sign (p (p (p zero)))) (s (s (s zero))))
   The key is the argument of 'change-sign' as value.")


#{
\subsection{Theories}
#}

(defconstant natac*natural 'natural "The theory Natural.")
(defconstant natac*integer 'integer "The theory Integer.")
(defconstant natac*rational 'rational "The theory Rational.")


#{
\subsection{Sorts}
#}

(defconstant natac*nat 'nat "The sort Nat.")
(defconstant natac*nnat 'nnat "The sort NNat.")
(defconstant natac*int 'int "The sort Int.")
(defconstant natac*rat 'rat "The sort Rat.")
(defconstant natac*sort-list (list natac*nat
				   natac*int
				   natac*rat)
  "A list of the currently available sorts.")


#{
\subsection{Theorems}
#}

(defconstant natac*zero-nat 'zero-nat "Zero is a natural number.")
(defconstant natac*succ-nat 'succ-nat "The successor of a natural number is natural.")
(defconstant natac*plus-nat-base 'plus-nat-base "The base case for recursive definition of addition.")
(defconstant natac*plus-nat-base2 'plus-nat-base2 "Another base case for recursive definition of addition.")
(defconstant natac*plus-nat-step 'plus-nat-step "The step case for recursive definition of addition.")
(defconstant natac*plus-nat-step2 'plus-nat-step2 "Another step case for recursive definition of addition.")
(defconstant natac*times-nat-base 'times-nat-base "The base case for recursive definition of multiplication.")
(defconstant natac*times-nat-base2 'times-nat-base2 "Another base case for recursive definition of multiplication.")
(defconstant natac*times-nat-step 'times-nat-step "The step case for recursive definition of multiplication.")
(defconstant natac*times-nat-step2 'times-nat-step2 "Another step case for recursive definition of multiplication.")
(defconstant natac*power-nat-base 'power-nat-base "The base case for recursive definition of exponentiation.")
(defconstant natac*power-nat-step 'power-nat-step "The step case for recursive definition of exponentiation.")
(defconstant natac*power-nat-base-zero 'power-nat-base-zero "Powers with base zero.")
(defconstant natac*power-nat-base-one 'power-nat-base-one "Powers with base one.")
(defconstant natac*less-nat-base 'less-nat-base "The base case for recursive definition of less.")
(defconstant natac*less-nat-step 'less-nat-step "The step case for recursive definition of less.")
(defconstant natac*less-implies-leq-nat 'less-implies-leq-nat "Less implies less or equal.")
(defconstant natac*equal-implies-leq-nat 'equal-implies-leq-nat "Equal implies less or equal.")

(defconstant natac*zero-nnat 'zero-nnat "Zero is a nonpositive integer.")
(defconstant natac*pred-nnat 'pred-nnat "The predecessor of a nonpostive integer is a nonpostive integer.")
(defconstant natac*nat-int 'nat-int "A natural number is whole-numbered.")
(defconstant natac*nnat-int 'nnat-int "A nonpositive integer is whole-numbered.")
(defconstant natac*plus-int-base 'plus-int-base "The base case for recursive definition of addition.")
(defconstant natac*plus-int-base2 'plus-int-base2 "Another base case for recursive definition of addition.")
(defconstant natac*plus-int-step-s 'plus-int-step-s "The step case for recursive definition of addition.")
(defconstant natac*plus-int-step-p 'plus-int-step-p "Another step case for recursive definition of addition.")
(defconstant natac*plus-int-step2-s 'plus-int-step2-s "Another step case for recursive definition of addition.")
(defconstant natac*plus-int-step2-p 'plus-int-step2-p "Another step case for recursive definition of addition.")
(defconstant natac*change-sign-base 'change-sign-base "The base case for recursive definition of unary minus.")
(defconstant natac*change-sign-reverse 'change-sign-reverse "Simplification of unary minus.")
(defconstant natac*change-sign-s 'change-sign-s "The step case for recursive definition of unary minus.")
(defconstant natac*change-sign-p 'change-sign-p "Another step case for recursive definition of unary minus.")
(defconstant natac*sp-int 'sp-int "The successor of the predecessor of a number equals the number itself.")
(defconstant natac*ps-int 'ps-int "The successor of the predecessor of a number equals the number itself.")
(defconstant natac*times-int-base 'times-int-base "The base case for recursive definition of multiplication.")
(defconstant natac*times-int-base2 'times-int-base2 "Another base case for recursive definition of multiplication.")
(defconstant natac*times-int-step-s 'times-int-step-s "The step case for recursive definition of multiplication.")
(defconstant natac*times-int-step-p 'times-int-step-p "Another step case for recursive definition of multiplication.")
(defconstant natac*times-int-step2-s 'times-int-step2-s "Another step case for recursive definition of multiplication.")
(defconstant natac*times-int-step2-p 'times-int-step2-p "Another step case for recursive definition of multiplication.")
(defconstant natac*power-int-base 'power-int-base "The base case for recursive definition of exponentiation.")
(defconstant natac*power-int-step 'power-int-step "The step case for recursive definition of exponentiation.")
(defconstant natac*gcd-left-arg-zero 'gcd-left-arg-zero "A base case of the Euclidian algorithm.")
(defconstant natac*gcd-right-arg-zero 'gcd-right-arg-zero "Another base case of the Euclidian algorithm.")
(defconstant natac*gcd-equal-args 'gcd-equal-args "Another base case of the Euclidian algorithm.")
(defconstant natac*gcd-neg-left-arg 'gcd-neg-left-arg "Greatest common divisor with negative arguments.")
(defconstant natac*gcd-neg-right-arg 'gcd-neg-right-arg "Greatest common divisor with negative arguments.")
(defconstant natac*gcd-diff-1 'gcd-diff-1 "The step case of the Euclidian algorithm.")
(defconstant natac*gcd-diff-2 'gcd-diff-2 "Another step case of the Euclidian algorithm.")
(defconstant natac*lcm-left-arg-zero 'lcm-left-arg-zero "A base case for the least common multiple.")
(defconstant natac*lcm-right-arg-zero 'lcm-right-arg-zero "Another base case for the least common multiple.")
(defconstant natac*lcm-equal-args 'lcm-equal-args "Another base case for the least common multiple.")
(defconstant natac*lcm-neg-left-arg 'lcm-neg-left-arg "Least common multiple with negative arguments.")
(defconstant natac*lcm-neg-right-arg 'lcm-neg-right-arg "Least common multiple with negative arguments.")
(defconstant natac*lcm-by-gcd 'lcm-by-gcd "The least common multiple by the greatest common divisor.")
(defconstant natac*div-int 'div-int "Whole-numbered division.")
(defconstant natac*mod-int 'mod-int "Residue of whole-numbered division.")
(defconstant natac*less-nnat-base 'less-nnat-base "The base case for recursive definition of less.")
(defconstant natac*less-nnat-step 'less-nnat-step "The step case for recursive definition of less.")
(defconstant natac*less-implies-leq-int 'less-implies-leq-int "Less implies less or equal.")
(defconstant natac*equal-implies-leq-int 'equal-implies-leq-int "Equal implies less or equal.")
(defconstant natac*neg-less-pos-int 'neg-less-pos-int "A negative integer is smaller than a positive integer.")

(defconstant natac*int-rat 'int-rat "An integer is rational.")
(defconstant natac*numerator-of-int 'numerator-of-int "The numerator of an whole-numbered number is the number itself.")
(defconstant natac*denominator-of-int 'denominator-of-int "The denominator of an whole-numbered number is one.")
(defconstant natac*rat-crit 'rat-crit "A criterion for a number being rational.")
(defconstant natac*cancel-fraction 'cancel-fraction "A theorem for cancelling fractions.")
(defconstant natac*numerator-equals-zero 'numerator-equals-zero "A rational number with numerator zero is the integer zero.")
(defconstant natac*int-to-rat 'int-to-rat "Conversion of integers to fractions.")
(defconstant natac*rat-to-int 'rat-to-int "Conversion of fractions with denominator one to integers.")
(defconstant natac*plus-rat 'plus-rat "Brute force addition on rational numbers.")
(defconstant natac*plus-rat-equal-denoms 'plus-rat-equal-denoms "Addition on rational numbers with equal denominators")
(defconstant natac*plus-rat-expanded-fracs 'plus-rat-expanded-fracs "Addition of expanded fractions.")
(defconstant natac*times-rat 'times-rat "Multiplication on rational numbers.")
(defconstant natac*power-rat-nat 'power-rat-nat "Natural powers of rational numbers.")
(defconstant natac*power-rat-nnat 'power-rat-nnat "Negative whole-numbered powers of rational numbers.")
(defconstant natac*power-rat-base-one 'power-rat-base-one "Powers with base one.")
(defconstant natac*change-sign-rat 'change-sign-rat "Unary minus on rational numbers.")
(defconstant natac*numerator-of-frac 'numerator-of-frac "Extraction of the numerator of a rational number.")
(defconstant natac*denominator-of-frac 'denominator-of-frac "Extraction of the denominator of a rational number.")
(defconstant natac*less-rat 'less-rat "Less on rational numbers.")
(defconstant natac*less-rat-neg-and-pos 'less-rat-neg-and-pos "A negative rational number is smaller than a positive rational number.")
(defconstant natac*less-implies-leq-rat 'less-implies-leq-rat "Less implies less or equal.")
(defconstant natac*equal-implies-leq-rat 'equal-implies-leq-rat "Equal implies less or equal.")


#{
\subsection{Tactics}
#}

(defconstant natac*simplify-num 'simplify-num "The tactic simplify-num.")
(defconstant natac*expand-num 'expand-num "The tactic expand-num.")
(defconstant natac*arith-simplify 'arith-simplify "The tactic arith-simplify.")
(defconstant natac*foralle-sort 'foralle-sort "The tactic foralle-sort.")
(defconstant natac*=subst '=subst "The tactic =subst.")
(defconstant natac*=ref '=ref "The tactic =ref.")
(defconstant natac*=sym '=sym "The tactic =sym.")
(defconstant natac*andi 'andi "The tactic andi.")
(defconstant natac*oril 'oril "The tactic oril.")
(defconstant natac*orir 'orir "The tactic orir.")
(defconstant natac*impe 'impe "The tactic impe.")
(defconstant natac*defni 'defni "The tactic defni.")
(defconstant natac*defne 'defne "The tactic defne.")
(defconstant natac*weaken 'weaken "The tactic weaken.")


#{
\subsection{Types}
#}

(defconstant natac*number-type :num "The type number.")


#{
\subsection{Setnums}
Setnums are numbers in set theoretical notation.
#}

(defconstant natac*zero :zero "The constant zero.")
(defconstant natac*succ :s "The successor function.")
(defconstant natac*pred :p "The predecessor function.")
(defconstant natac*frac :frac "The fraction function.")
(defconstant natac*set-function-list (list natac*succ
					   natac*pred
					   natac*frac)
  "The functions constituting numbers in set theoretical representations.")


#{
\subsection{Arithmetic Functions and Operators}
#}

(defconstant natac*plus :plus "The addition operator.")
(defconstant natac*minus :minus "The subtraction operator.")
(defconstant natac*times :times "The multiplication operator.")
(defconstant natac*power :power "The exponentiation operator.")
(defconstant natac*div :div "The division operator for integers.")
(defconstant natac*mod :mod "The modulo operator for integers.")
(defconstant natac*gcd :gcd "The greatest common divisor for integers.")
(defconstant natac*lcm :lcm "The least common multiple for integers.")
(defconstant natac*divide :divide "The division operator for rational numbers.")
(defconstant natac*change-sign :change-sign "The unary minus.")
(defconstant natac*one-over :one-over "The reciprocal value.")
(defconstant natac*numerator :numerator "The numerator of a fraction.")
(defconstant natac*denominator :denominator "The denominator of a fraction.")
(defconstant natac*function-list (list natac*plus
				       natac*minus
				       natac*times
				       natac*divide
				       natac*power
				       natac*div
				       natac*mod
				       natac*gcd
				       natac*lcm)
  "The list of arithmetic functions simplify-num, expand-num and arith-simplify can handle.")
(defconstant natac*arith-function-list (list natac*plus
					     natac*minus
					     natac*times
					     natac*divide
					     natac*power
					     natac*div
					     natac*mod
					     natac*gcd
					     natac*lcm)
  "The list of arithmetic functions that can be expanded.")


#{
\subsection{Arithmetic Predicates}
#}

(defconstant natac*= := "The predicate =.")
(defconstant natac*less :less "The predicate less.")
(defconstant natac*leq :leq "The predicate less or equal.")
(defconstant natac*greater :greater "The predicate greater.")
(defconstant natac*geq :geq "The predicate greater or equal.")
(defconstant natac*in :in "The predicate in.")
(defconstant natac*arith-predicate-list (list natac*=
					      natac*less
					      natac*leq
					      natac*greater
					      natac*geq
					      natac*in)
  "The list of arithmetic predicates that can be expanded.")


#{
\subsection{Miscellaneous}
#}

(defconstant natac*theory-array (make-array '(15 3 3)
					    :initial-contents
					    (list (list (list natac*natural natac*integer natac*rational)
							(list natac*integer natac*integer natac*rational)
							(list natac*rational natac*rational natac*rational))
						  (list (list natac*integer natac*integer natac*rational)
							(list natac*integer natac*integer natac*rational)
							(list natac*rational natac*rational natac*rational))
						  (list (list natac*natural natac*integer natac*rational)
							(list natac*integer natac*integer natac*rational)
							(list natac*rational natac*rational natac*rational))
						  (list (list natac*rational natac*rational natac*rational)
							(list natac*rational natac*rational natac*rational)
							(list natac*rational natac*rational natac*rational))
						  (list (list natac*natural natac*rational nil)
							(list natac*integer natac*rational nil)
							(list natac*rational natac*rational nil))
						  (list (list natac*integer nil nil)
							(list natac*integer nil nil)
							(list nil nil nil))
						  (list (list natac*integer nil nil)
							(list natac*integer nil nil)
							(list nil nil nil))
						  (list (list natac*integer natac*integer nil)
							(list natac*integer natac*integer nil)
							(list nil nil nil))
						  (list (list natac*integer natac*integer nil)
							(list natac*integer natac*integer nil)
							(list nil nil nil))
						  (list (list natac*natural natac*integer natac*rational)
							(list natac*integer natac*integer natac*rational)
							(list natac*rational natac*rational natac*rational))
						  (list (list natac*natural natac*integer natac*rational)
							(list natac*integer natac*integer natac*rational)
							(list natac*rational natac*rational natac*rational))
						  (list (list natac*natural natac*integer natac*rational)
							(list natac*integer natac*integer natac*rational)
							(list natac*rational natac*rational natac*rational))
						  (list (list natac*natural natac*integer natac*rational)
							(list natac*integer natac*integer natac*rational)
							(list natac*rational natac*rational natac*rational))
						  (list (list natac*natural nil nil)
							(list nil natac*integer nil)
							(list nil nil natac*rational))
						  (list (list natac*natural natac*integer natac*rational)
							(list nil natac*integer natac*rational)
							(list nil nil natac*rational))))
  "This array serves to find the theory necessary for computation,
    depending on the operator, the left argument and the right argument.
    See functions natac=array-index-of-sort and natac=array-index-of-operator,
    which perform the corresponding mappings:
       The operator plus is mapped to 0.
       The operator minus is mapped to 1.
       The operator times is mapped to 2.
       The operator divide is mapped to 3.
       The operator power is mapped to 4.
       The operator div is mapped to 5.
       The operator mod is mapped to 6.
       The operator gcd is mapped to 7.
       The operator lcm is mapped to 8.
       The operator less is mapped to 9.
       The operator leq is mapped to 10.
       The operator greater is mapped to 11.
       The operator geq is mapped to 12.
       The operator = is mapped to 13.
       The operator in is mapped to 14.
       If the smallest sort (ordered by inclusion) an argument belongs to is nat it is mapped to 0.
       If the smallest sort (ordered by inclusion) an argument belongs to is nnat it is mapped to 1.
       If the smallest sort (ordered by inclusion) an argument belongs to is rat it is mapped to 2.
    nil as array entry means there is (currently) no theory this computation can be performed in.
    example: (div -6 3)
             As can be seen in function natac=array-index-of-operator the operator div
             is mapped to 5. Furthermore, as -6 is whole-numbered but not natural, -6 is mapped
             to 1 in function natac=array-index-of-sort. Similarly 3 is mapped to 0.
             So we 've got to access array entry (5 1 0) which shows that in order to
             perform this computation at least theory integer is necessary.")





(defconstant natac*expansion nil
  "If this constant is set to nil, computations will be expanded to a lower level.
   However, usually some of the resulting open nodes will be closed by further applications.
   of simplify-num, expand-num or arith-simplify.
   If this constant is set to t, computations will be expanded down to a level no further
   simplify-num, expand-num or arith-simplify is required at.")




#{
\section{Auxiliary Functions}
#}


#{
\subsection{Values}
Values are simply LISP numbers (integers or rational numbers).
#}

(defun natac=value-p (term)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt term} is a value."))
  (rationalp term))

(defun natac=value-naturalp (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt value} is a natural value."))
  (and (natac=value-integerp value)
       (not (natac=value-minusp value))))

(defun natac=value-integerp (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "T iff value is an integer value."))
  (integerp value))

(defun natac=value-rationalp (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt value} is a rational value."))
  (rationalp value))

(defun natac=value-zerop (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt value} = 0."))
  (zerop value))

(defun natac=value-plusp (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt value} is a positive value."))
  (plusp value))

(defun natac=value-minusp (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt value} is a negative value."))
  (minusp value))

(defun natac=values-eql (value-1 value-2)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "Two values.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt value-1} = {\\tt value-2}."))
  (= value-1 value-2))

(defun natac=values-< (value-1 value-2)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "Two values.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt value-1} smaller than {\\tt value-2}."))
  (< value-1 value-2))

(defun natac=value-abs-value (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "The absolute value of value as value."))
  (abs value))

(defun natac=values-abs-values-eql (value-1 value-2)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "Two values.")
	   (effect  "No effects.")
	   (value   "T iff absolute value of {\\tt value-1} = absolute value of {\\tt value-2}."))
  (let ((abs-value-1 (natac=value-abs-value value-1))
	(abs-value-2 (natac=value-abs-value value-2)))
    (natac=values-eql abs-value-1 abs-value-2)))

(defun natac=values-plus (value-1 value-2)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "Two values.")
	   (effect  "No effects.")
	   (value   "{\\tt value-1} + {\\tt value-2} as value."))
  (+ value-1 value-2))

(defun natac=values-minus (value-1 value-2)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "Two values.")
	   (effect  "No effects.")
	   (value   "{\\tt value-1} - {\\tt value-2} as value."))
  (- value-1 value-2))

(defun natac=value-change-sign (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "- {\\tt value} as value."))
  (- value))

(defun natac=value-decrement (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "{\\tt value} - 1 as value."))
  (1- value))

(defun natac=value-increment (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "{\\tt value} + 1 as value."))
  (1+ value))

(defun natac=values-div (value-1 value-2)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "Two values.")
	   (effect  "No effects.")
	   (value   "{\\tt value-1} / {\\tt value-2} as value."))
  (/ value-1 value-2))

(defun natac=values-gcd (value-1 value-2)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "Two integer values.")
	   (effect  "No effects.")
	   (value   "The greatest common divisor of {\\tt value-1} and {\\tt value-2} as value."))
  (gcd value-1 value-2))

(defun natac=values-lcm (value-1 value-2)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "Two integer values.")
	   (effect  "No effects.")
	   (value   "The least common multiple of {\\tt value-1} and {\\tt value-2} as value."))
  (lcm value-1 value-2))

(defun natac=value-numerator (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "The numerator of {\\tt value} as value."))
  (numerator value))

(defun natac=value-denominator (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "The denominator of {\\tt value} as value."))
  (denominator value))

(defun natac=create-one-as-value ()
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "No input.")
	   (effect  "No effects.")
	   (value   "1 as value."))
  1)

(defun natac=int-value-eql-one-p (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt value} = 1."))
  (let ((one-as-value (natac=create-one-as-value)))
    (natac=values-eql one-as-value value)))

(defun natac=int-values-no-common-divisor-p (value-1 value-2)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "Two values.")
	   (effect  "No effects.")
	   (value   "T iff there is no common divisor of {\\tt value-1} and {\\tt value-2}."))
  (natac=int-value-eql-one-p (natac=values-gcd value-1 value-2)))

(defun natac=compute-value-from-symnum (symnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A symnum.")
	   (effect  "No effects.")
	   (value   "{\\tt symnum} as value."))
  (keim~name symnum))

(defun natac=compute-value-from-setnum (setnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A setnum.")
	   (effect  "No effects.")
	   (value   "{\\tt setnum} as value."))
  (natac=build-number setnum))


;;; symnum-stuff
;;; symnum is the omega number type

(defun natac=symnum-p (term)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt term} is a symnum."))
  (term~number-p term))

(defun natac=compute-symnum-from-value (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "{\\tt value} as symnum."))
  (term~constant-create value (env~lookup-object natac*number-type (pds~environment omega*current-proof-plan))))

(defun natac=compute-symnum-from-setnum (setnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A setnum.")
	   (effect  "No effects.")
	   (value   "{\\tt setnum} as symnum."))
  (let ((value (natac=compute-value-from-setnum setnum)))
    (natac=compute-symnum-from-value value)))

(defun natac=symnum-change-sign (symnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A symnum.")
	   (effect  "No effects.")
	   (value   "- {\\tt symnum} as symnum."))
  (let* ((value (natac=compute-value-from-symnum symnum))
	 (new-value (natac=value-change-sign value)))
    (natac=compute-symnum-from-value new-value)))


#{
\subsection{Setnums}
'setnums' are numbers (integers or rationals) in set theoretical notation ; for example:\\
5    = (s (s (s (s (s zero))))),\\
-3   = (p (p (p zero))),\\
-4/3 = (frac (p (p (p (p zero)))) (s (s (s zero)))).
#}

(defun natac=set-function-p (term)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "{\\tt term} itself if {\\tt term} is in {\\tt natac*set-function-list}."
		    "NIL otherwise."))
  (find term
	natac*set-function-list
	:test #'natac=functions-eql))

(defun natac=syntactic-setnum-p (term)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt term} syntactically is a setnum."))
  (when (term~p term)
    (labels ((natac=setnum-p-rec (term-list)
				 (cond ((null term-list))
				       ((natac=eql (car term-list) natac*zero))
				       ((not (data~appl-p (car term-list))) nil)
				       ((natac=set-function-p (data~appl-function (car term-list)))
					(let ((args (data~appl-arguments (car term-list))))
					  (and (natac=setnum-p-rec (list (car args)))
					       (natac=setnum-p-rec (rest args))
					       (natac=setnum-p-rec (rest term-list)))))
				       (t nil))))
      (natac=setnum-p-rec (list term)))))

(defun natac=normalized-zero-setnum-p (term)
  (declare (edited  "09-NOV-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "t iff {\\tt term} equals zero as setnum."))
  (unless (data~appl-p term)
    (natac=eql term natac*zero)))

(defun natac=normalized-whole-numbered-setnum-p (term)
  (declare (edited  "09-NOV-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt term} is a whole-numbered, normalized setnum."))
  (or (natac=normalized-positive-whole-numbered-setnum-p term)
      (natac=normalized-nonpositive-whole-numbered-setnum-p term)))

(defun natac=normalized-positive-whole-numbered-setnum-p (term)
  (declare (edited  "09-NOV-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt term} is a whole-numbered, normalized positive setnum." ))
  (when (and (data~appl-p term)
	     (natac=functions-eql (data~appl-function term) natac*succ))
    (natac=normalized-nonnegative-whole-numbered-setnum-p (car (data~appl-arguments term)))))

(defun natac=normalized-nonpositive-whole-numbered-setnum-p (term)
  (declare (edited  "09-NOV-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt term} is a whole-numbered, normalized nonpositive setnum." ))
  (if (data~appl-p term)
      (when (natac=functions-eql (data~appl-function term) natac*pred)
	(natac=normalized-nonpositive-whole-numbered-setnum-p (car (data~appl-arguments term))))
    (natac=normalized-zero-setnum-p term)))

(defun natac=normalized-nonnegative-whole-numbered-setnum-p (term)
  (declare (edited  "09-NOV-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt term} is a whole-numbered, normalized nonnegative setnum." ))
  (if (data~appl-p term)
      (when (natac=functions-eql (data~appl-function term) natac*succ)
	(natac=normalized-nonnegative-whole-numbered-setnum-p (car (data~appl-arguments term))))
    (natac=normalized-zero-setnum-p term)))

(defun natac=normalized-setnum-p (term)
  (declare (edited  "09-NOV-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt term} is a normalized setnum." ))
  (when (term~p term)
    (cond ((natac=normalized-zero-setnum-p term))
	  ((not (data~appl-p term)) nil)
	  ((natac=functions-eql (data~appl-function term) natac*frac)
	   (and (natac=normalized-whole-numbered-setnum-p (car (data~appl-arguments term)))
		(natac=normalized-positive-whole-numbered-setnum-p (cadr (data~appl-arguments term)))
		(let ((one-as-value (natac=create-one-as-value))
		      (numerator-as-value (natac=compute-value-from-setnum (car (data~appl-arguments term))))
		      (denominator-as-value (natac=compute-value-from-setnum (cadr (data~appl-arguments term)))))
		  (and (not (natac=value-integerp (natac=values-div numerator-as-value denominator-as-value)))
		       (natac=values-eql one-as-value (natac=values-gcd numerator-as-value denominator-as-value))))))
	  ((natac=functions-eql (data~appl-function term) natac*succ)
	   (natac=normalized-nonnegative-whole-numbered-setnum-p (car (data~appl-arguments term))))
	  ((natac=functions-eql (data~appl-function term) natac*pred)
	   (natac=normalized-nonpositive-whole-numbered-setnum-p term))
	  (t nil))))

(defun natac=setnum-p (term)
  (declare (edited  "09-NOV-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt term} is a normalized setnum." ))
  (natac=normalized-setnum-p term))

(defun natac=setnum-integerp (setnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A setnum.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt setnum} is an integer."))
  (or (not (data~appl-p setnum))
      (not (natac=functions-eql (data~appl-function setnum) natac*frac))))

(defun natac=setnum-fractionp (setnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A setnum.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt setnum} is a fraction."))
  (when (data~appl-p setnum)
    (natac=functions-eql natac*frac (data~appl-function setnum))))

(defun natac=pos-int-setnum-decrement (setnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A positive integer setnum.")
	   (effect  "No effects.")
	   (value   "{\\tt setnum} - 1 as setnum."))
  (car (data~appl-arguments setnum)))

(defun natac=neg-int-setnum-increment (setnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A negative integer setnum.")
	   (effect  "No effects.")
	   (value   "{\\tt setnum} + 1 as setnum."))
  (car (data~appl-arguments setnum)))

(defun natac=setnum-numerator (setnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A setnum.")
	   (effect  "No effects.")
	   (value   "The numerator of {\\tt setnum} as setnum."))
  (if (natac=setnum-integerp setnum)
      setnum
    (car (data~appl-arguments setnum))))

(defun natac=setnum-denominator (setnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A setnum.")
	   (effect  "No effects.")
	   (value   "The denominator of {\\tt setnum} as setnum."))
  (if (natac=setnum-integerp setnum)
      (natac=create-one-as-setnum)
    (cadr (data~appl-arguments setnum))))

(defun natac=create-one-as-setnum ()
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "No input.")
	   (effect  "No effects.")
	   (value   "1 as setnum."))
  (let ((one-as-value (natac=create-one-as-value)))
    (natac=compute-setnum-from-value one-as-value)))

(defun natac=compute-setnum-from-value (value)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value.")
	   (effect  "No effects.")
	   (value   "{\\tt value} as setnum."))
  (natac=transform-number value))

(defun natac=compute-setnum-from-symnum (symnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A symnum.")
	   (effect  "No effects.")
	   (value   "{\\tt symnum} aas setnum."))
  (let ((value (natac=compute-value-from-symnum symnum)))
    (natac=compute-setnum-from-value value)))

(defun natac~data-equal (num1 num2)
  (or (data~equal num1 num2)
      (let ((num1-as-value (cond ((natac=symnum-p num1)
				  (natac=compute-value-from-symnum num1))
				 ((natac=setnum-p num1)
				  (natac=compute-value-from-setnum num1))
				 (t nil)))
	    (num2-as-value (cond ((natac=symnum-p num2)
				  (natac=compute-value-from-symnum num2))
				 ((natac=setnum-p num2)
				  (natac=compute-value-from-setnum num2))
				 (t nil))))
	(when (and num1-as-value num2-as-value)
	  (natac=values-eql num1-as-value num2-as-value)))))


#{
\subsection{Natacnums}
natacnum is a file-own `datatype' representing numbers up to rationals.
A natacnum is a two-element list:\\
\begin{enumerate}
\item
The first element is the value of the number.
\item
The second element is the corresponding number in set theoretical representation.
\end{enumerate}
i.e. a list of value and setnum.\\
for example: five as natacnum: (5 (s (s (s (s (s zero))))))\\
However note that the setnum need not be canceled, so, for example,
(-2 (frac (s (s (s (s zero)))) (p (p zero)))) is as well a valid natacnum as
(-2 (p (p zero))).
If we assume the setnum representation in a natacnum to be canceled and normalized
we refer explicitly to a canceled natacnum.
#}

(defun natac=symnum-or-setnum-p (term)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff term is either a symnum or a setnum."
		    "NIL otherwise."))
  (or (natac=symnum-p term)
      (natac=setnum-p term)))

(defun natac~num-p (term)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "T iff term is a num, i.e. either a symnum or a setnum."))
  (natac=symnum-or-setnum-p term))

(defun natac=compute-natacnum-from-value-and-setnum (value setnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A value and a setnum representing the same number.")
	   (effect  "No effects.")
	   (value   "A natacnum representing this number."))
  (list value
	setnum))

(defun natac=compute-natacnum (num)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A number either as value or as symnum or as setnum.")
	   (effect  "No effects.")
	   (value   "A natacnum representing this number."))
  (cond ((natac=value-p num)
	 (let ((setnum (natac=compute-setnum-from-value num)))
	   (natac=compute-natacnum-from-value-and-setnum num setnum)))
	((natac=symnum-p num)
	 (let ((value (natac=compute-value-from-symnum num))
	       (setnum (natac=compute-setnum-from-symnum num)))
	   (natac=compute-natacnum-from-value-and-setnum value setnum)))
	((natac=syntactic-setnum-p num)
	 (let ((value (natac=compute-value-from-setnum num)))
	   (natac=compute-natacnum-from-value-and-setnum value num)))
	(t num)))

(defun natac=natacnum-value (natacnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum.")
	   (effect  "No effects.")
	   (value   "{\\tt natacnum} as value."))
  (car natacnum))

(defun natac=natacnum-abs-value (natacnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum.")
	   (effect  "No effects.")
	   (value   "The absolute value of {\\tt natacnum} as value."))
  (let ((value (natac=natacnum-value natacnum)))
    (natac=value-abs-value value)))

(defun natac=natacnum-setnum (natacnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum.")
	   (effect  "No effects.")
	   (value   "{\\tt natacnum} as setnum."))
  (cadr natacnum))

(defun natac=natacnum-sort (natacnum)
  (declare (edited  "03-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum or a sort.")
	   (effect  "No effects.")
	   (value   "natac*nat if {\\tt natacnum} is a natural natacnum."
		    "natac*int if {\\tt natacnum} is an integer natacnum and {\\tt natacnum} smaller than 0."
		    "natac*rat if {\\tt natacnum} is a rational natacnum but not an integer."
		    "If {\\tt natacnum} is a sort rather than a natacnum the sort itself is returned."
		    " (This convention is useful for the predicate {\\tt in} which is a special case"
		    "  with respect to its second argument which is a sort rather than a number."
		    "NIL if {\\tt natacnum} is NIL."))
  (cond ((not natacnum)
	 nil)
	((natac=sort-p natacnum)
	 natacnum)
	((natac=natacnum-naturalp natacnum)
	 natac*nat)
	((natac=natacnum-integerp natacnum)
	 natac*nnat)
	((natac=natacnum-rationalp natacnum)
	 natac*rat)))

(defun natac=natacnum-naturalp (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum} is natural."))
  (let ((value (natac=natacnum-value natacnum))
	(setnum (natac=natacnum-setnum natacnum)))
    (and (natac=value-naturalp value)
	 (not (natac=setnum-fractionp setnum)))))

(defun natac=natacnum-integerp (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum} is integer."))
  (let ((value (natac=natacnum-value natacnum))
	(setnum (natac=natacnum-setnum natacnum)))
    (and (natac=value-integerp value)
	 (not (natac=setnum-fractionp setnum)))))

(defun natac=natacnum-rationalp (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum} is rational."))
  (let ((value (natac=natacnum-value natacnum)))
    (natac=value-rationalp value)))

(defun natac=natacnums-eq (natacnum-1 natacnum-2)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "Two natacnums.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum-1} and {\\tt natacnum-2} point to the same memory address."))
  (eq natacnum-1 natacnum-2))

(defun natac=natacnums-eql (natacnum-1 natacnum-2)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "Two natacnums.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum-1} and {\\tt natacnum-2} represent the same number."))
  (let ((value-1 (natac=natacnum-value natacnum-1))
	(value-2 (natac=natacnum-value natacnum-2)))
    (natac=values-eql value-1 value-2)))

(defun natac=natacnum-zerop (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum} = 0 (as natacnum)"))
  (let ((value (natac=natacnum-value natacnum)))
    (natac=value-zerop value)))

(defun natac=natacnum-plusp (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum} is a positive natacnum."))
  (let ((value (natac=natacnum-value natacnum)))
    (natac=value-plusp value)))

(defun natac=natacnum-minusp (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum} is a negative natacnum."))
  (let ((value (natac=natacnum-value natacnum)))
    (natac=value-minusp value)))

(defun natac=abs-smaller-natacnum (natacnum-1 natacnum-2)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "Two natacnums.")
	   (effect  "No effects.")
	   (value   "{\\tt natacnum-1} if absolute value of {\\tt natacnum-1} smaller than absolute value of {\\tt natacnum-2}."
		    "{\\tt natacnum-2} otherwise."))
  (let ((abs-value-1 (natac=natacnum-abs-value natacnum-1))
	(abs-value-2 (natac=natacnum-abs-value natacnum-2)))
    (if (natac=values-< abs-value-1 abs-value-2)
	natacnum-1
      natacnum-2)))

(defun natac=abs-greater-natacnum (natacnum-1 natacnum-2)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "Two natacnums")
	   (effect  "No effects.")
	   (value   "{\\tt natacnum-2} if absolute value of {\\tt natacnum-1} smaller than absolute value of {\\tt natacnum-2}."
		    "{\\tt natacnum-1} otherwise."))
  (let ((abs-value-1 (natac=natacnum-abs-value natacnum-1))
	(abs-value-2 (natac=natacnum-abs-value natacnum-2)))
    (if (natac=values-< abs-value-1 abs-value-2)
	natacnum-2
      natacnum-1)))

(defun natac=pos-int-natacnum-decrement (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A positive natacnum.")
	   (effect  "No effects.")
	   (value   "{\\tt natacnum} - 1 as natacnum."))
  (let* ((value (natac=natacnum-value natacnum))
	 (new-value (natac=value-decrement value))
	 (setnum (natac=natacnum-setnum natacnum))
	 (new-setnum (natac=pos-int-setnum-decrement setnum)))
    (natac=compute-natacnum-from-value-and-setnum new-value new-setnum)))

(defun natac=neg-int-natacnum-increment (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A negative natacnum.")
	   (effect  "No effects.")
	   (value   "{\\tt natacnum} + 1 as natacnum."))
  (let* ((value (natac=natacnum-value natacnum))
	 (new-value (natac=value-increment value))
	 (setnum (natac=natacnum-setnum natacnum))
	 (new-setnum (natac=neg-int-setnum-increment setnum)))
    (natac=compute-natacnum-from-value-and-setnum new-value new-setnum)))

(defun natac=int-natacnum-decrement-abs-value (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nonzero natacnum.")
	   (effect  "No effects.")
	   (value   "{\\tt natacnum} - 1 as natacnum if {\\tt natacnum} is positive."
		    "{\\tt natacnum} + 1 as natacnum if {\\tt natacnum} is negative."))
  (if (natac=natacnum-plusp natacnum)
      (natac=pos-int-natacnum-decrement natacnum)
    (natac=neg-int-natacnum-increment natacnum)))

(defun natac=natacnum-change-sign (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum.")
	   (effect  "No effects.")
	   (value   "- {\\tt natacnum} as natacnum."))
  (let* ((value (natac=natacnum-value natacnum))
	 (new-value (natac=value-change-sign value))
	 (new-setnum (natac=compute-setnum-from-value new-value)))
    (natac=compute-natacnum-from-value-and-setnum new-value new-setnum)))

(defun natac=canc-rat-natacnum-numerator (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A canceled rational natacnum.")
	   (effect  "No effects.")
	   (value   "The numerator of {\\tt natacnum} as natacnum."))
  (let* ((value (natac=natacnum-value natacnum))
	 (setnum (natac=natacnum-setnum natacnum))
	 (numerator-as-value (natac=value-numerator value))
	 (numerator-as-setnum (natac=setnum-numerator setnum)))
    (natac=compute-natacnum-from-value-and-setnum numerator-as-value numerator-as-setnum)))

(defun natac=canc-rat-natacnum-denominator (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A canceled rational natacnum.")
	   (effect  "No effects.")
	   (value   "The denominator of {\\tt natacnum} as natacnum."))
  (let* ((value (natac=natacnum-value natacnum))
	 (setnum (natac=natacnum-setnum natacnum))
	 (denominator-as-value (natac=value-denominator value))
	 (denominator-as-setnum (natac=setnum-denominator setnum)))
    (natac=compute-natacnum-from-value-and-setnum denominator-as-value denominator-as-setnum)))

(defun natac=rat-natacnum-numerator (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A rational natacnum (not necessarily canceled).")
	   (effect  "No effects.")
	   (value   "The numerator of {\\tt natacnum} as natacnum."))
  (let* ((setnum (natac=natacnum-setnum natacnum))
	 (numerator-as-setnum (natac=setnum-numerator setnum))
	 (numerator-as-value (natac=compute-value-from-setnum numerator-as-setnum)))
    (natac=compute-natacnum-from-value-and-setnum numerator-as-value numerator-as-setnum)))

(defun natac=rat-natacnum-denominator (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A rational natacnum (not necessarily canceled).")
	   (effect  "No effects.")
	   (value   "The denominator of {\\tt natacnum} as natacnum."))
  (let* ((setnum (natac=natacnum-setnum natacnum))
	 (denominator-as-setnum (natac=setnum-denominator setnum))
	 (denominator-as-value (natac=compute-value-from-setnum denominator-as-setnum)))
    (natac=compute-natacnum-from-value-and-setnum denominator-as-value denominator-as-setnum)))

(defun natac=rat-natacnum-canceled-p (natacnum)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum representing a rational but not integer number.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum} is canceled, i.e."
		    " numerator is whole-numbered."
		    " denominator is natural, greater than or equal to 2 and"
		    " gcd (numerator, denominator) = 1."))
  (let ((numerator (natac=rat-natacnum-numerator natacnum))
	(denominator (natac=rat-natacnum-denominator natacnum)))
    (and (natac=natacnum-plusp denominator)
	 (not (natac=int-natacnum-eql-one-p denominator))
	 (natac=int-natacnums-no-common-divisor-p numerator denominator))))

(defun natac=canc-rat-natacnums-equal-denominators-p (natacnum-1 natacnum-2)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Two canceled rational natacnums.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum-1} and {\\tt natacnum-2} have equal denominators."))
  (let ((denominator-1 (natac=canc-rat-natacnum-denominator natacnum-1))
	(denominator-2 (natac=canc-rat-natacnum-denominator natacnum-2)))
    (natac=natacnums-eql denominator-1 denominator-2)))

(defun natac=int-natacnums-no-common-divisor-p (natacnum-1 natacnum-2)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Two integer natacnums.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt natacnum-1} and {\\tt natacnum-2} have no common divisor."))
  (let ((value-1 (natac=natacnum-value natacnum-1))
	(value-2 (natac=natacnum-value natacnum-2)))
    (natac=int-values-no-common-divisor-p value-1 value-2)))

(defun natac=canc-rat-natacnums-denominators-no-common-divisor-p (natacnum-1 natacnum-2)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Two canceled rational natacnums.")
	   (effect  "No effects.")
	   (value   "T iff the denominators of {\\tt natacnum-1} and {\\tt natacnum-2} have no common divisor."))
  (let ((denominator-1 (natac=canc-rat-natacnum-denominator natacnum-1))
	(denominator-2 (natac=canc-rat-natacnum-denominator natacnum-2)))
    (natac=int-natacnums-no-common-divisor-p denominator-1 denominator-2)))

(defun natac=factor-for-expansion-from-denominator (canceled-fraction-as-natacnum denominator-of-uncanceled-fraction-as-natacnum-or-value)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "A canceled fraction as natacnum."
		    "The denominator of an uncanceled fraction having the same value as"
		    "{\\tt canceled-fraction-as-natacnum} as natacnum or as value.")
	   (effect  "No effects.")
	   (value   "The factor necessary for expanding {\\tt canceled-fraction-as-natacnum} to"
		    "the fraction with denominator {\\tt denominator-of-uncanceled-fraction-as-natacnum-or-value}"
		    "as natacnum."))
  (let ((denominator-of-uncanceled-fraction-as-value
	 (if (natac=value-p denominator-of-uncanceled-fraction-as-natacnum-or-value)
	     denominator-of-uncanceled-fraction-as-natacnum-or-value
	   (natac=natacnum-value denominator-of-uncanceled-fraction-as-natacnum-or-value))))
    (natac=factor-for-expansion-from-denominator-cont canceled-fraction-as-natacnum denominator-of-uncanceled-fraction-as-value)))

(defun natac=factor-for-expansion-from-denominator-cont (canceled-fraction-as-natacnum denominator-of-uncanceled-fraction-as-value)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "A canceled fraction as natacnum."
		    "The denominator of an uncanceled fraction having the same value as"
		    "{\\tt canceled-fraction-as-natacnum} as value.")
	   (effect  "No effects.")
	   (value   "The factor necessary for expanding {\\tt canceled-fraction-as-natacnum} to"
		    "the fraction with denominator {\\tt denominator-of-uncanceled-fraction-as-natacnum-or-value}"
		    "as natacnum."))
  (let* ((denominator-of-canceled-fraction (natac=canc-rat-natacnum-denominator canceled-fraction-as-natacnum))
	 (denominator-of-canceled-fraction-as-value (natac=natacnum-value denominator-of-canceled-fraction)))
    (natac=compute-natacnum (natac=values-div denominator-of-uncanceled-fraction-as-value
					      denominator-of-canceled-fraction-as-value))))

(defun natac=factor-for-expansion (canceled-natacnum uncanceled-natacnum)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "A canceled natacnum."
		    "An uncanceled natacnum having the same value as {\\tt canceled-natacnum}.")
	   (effect  "No effects.")
	   (value   "The factor necessary for expanding {\\tt canceled-natacnum} to {\\tt uncanceled-natacnum}"
		    "as natacnum."))
  (let ((denominator-of-uncanceled-natacnum (natac=rat-natacnum-denominator uncanceled-natacnum)))
    (natac=factor-for-expansion-from-denominator canceled-natacnum denominator-of-uncanceled-natacnum)))

(defun natac=canc-rat-natacnums-least-common-denominator (natacnum-1 natacnum-2)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Two canceled rational natacnums.")
	   (effect  "No effects.")
	   (value   "The least common denominator of {\\tt natacnum-1} and {\\tt natacnum-2} as natacnum."))
  (natac=compute-natacnum (natac=canc-rat-natacnums-least-common-denominator-as-value natacnum-1 natacnum-2)))

(defun natac=canc-rat-natacnums-least-common-denominator-as-value (natacnum-1 natacnum-2)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Two canceled rational natacnums.")
	   (effect  "No effects.")
	   (value   "The least common denominator of {\\tt natacnum-1} and {\\tt natacnum-2} as value."))
  (let* ((denominator-1 (natac=canc-rat-natacnum-denominator natacnum-1))
	 (denominator-2 (natac=canc-rat-natacnum-denominator natacnum-2))
	 (denominator-1-as-value (natac=natacnum-value denominator-1))
	 (denominator-2-as-value (natac=natacnum-value denominator-2)))
    (natac=values-lcm denominator-1-as-value denominator-2-as-value)))

(defun natac=int-natacnum-eql-one-p (natacnum)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "An integer natacnum.")
	   (effect  "No effects.")
	   (value   "T iff natacnum = 1."))
  (let ((value (natac=natacnum-value natacnum)))
    (natac=int-value-eql-one-p value)))

(defun natac=int-natacnum-zerop (natacnum)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "An integer natacnum.")
	   (effect  "No effects.")
	   (value   "T iff natacnum = 0."))
  (let ((value (natac=natacnum-value natacnum)))
    (natac=value-zerop value)))


#{
\subsection{Arithmetic functions}
#}

(defun natac=functions-eql (func-1 func-2)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "Two functions. Either as names of functions or as objects, mixed possible.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt func-1} and {\\tt func-2} represent the same function."))
  (natac=eql func-1 func-2))

(defun natac=arith-function-p (term)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "{\\tt term} itself if {\\tt term} is an arithmetic function,"
		    "i.e. is a member of {\\tt natac*arith-function-list}."
		    "NIL otherwise."))
  (find term
	natac*arith-function-list
	:test #'natac=functions-eql))

(defun natac=arith-predicate-p (term)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "{\\tt term} itself if {\\tt term} is an arithmetic predicate,"
		    "i.e. is a member of {\\tt natac*arith-predicate-list}."
		    "NIL otherwise."))
  (find term
	natac*arith-predicate-list
	:test #'natac=functions-eql))


#{
\subsection{Nataccomps}
nataccomp is a file-own `datatype' representing computations on numbers.\\
A nataccomp is a list with four elements:
\begin{enumerate}
\item
The first element is the operator (as object).
\item
The second element is a list itself, consisting of:
\begin{enumerate}
\item
The first argument as natacnum.
\item
The second argument (as natacnum if the second argument is a number (which, for example,
									    is not true for operator 'in' where the second argument is a sort))
or NIL if operator is defined to have one argument.
\end{enumerate}
\item
The name of the theory necessary for the computation.
\item
A list with useful theorem names. (For recursively defined operators in the order: base theorem, step theorem.)
This list can be empty.
\end{enumerate}
#}

(defun natac=compute-arg-list (left-arg right-arg)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "The left argument and the right argument of the involved computation.")
	   (effect  "No effects.")
	   (value   "A list of these two arguments."))
  (list left-arg
	right-arg))

(defun natac=compute-preliminary-nataccomp (operator left-arg right-arg theory-name)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "Operator, left argument, right argument and necessary theory"
		    "of the involved computation.")
	   (effect  "No effects.")
	   (value   "The preliminary nataccomp consisting of the first three parts of a nataccomp."))
  (let ((arg-list (natac=compute-arg-list left-arg right-arg)))
    (list operator
	  arg-list
	  theory-name)))

(defun natac=compute-combination-of-preliminary-nataccomp-and-theorems (preliminary-nataccomp theorems)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A preliminary nataccomp, consisting of the first three parts of a nataccomp and"
		    "the list of theorems.")
	   (effect  "No effects.")
	   (value   "A complete nataccomp (including the fourth part)."))
  (append preliminary-nataccomp
	  (list theorems)))

(defun natac=compute-nataccomp (operator left-arg right-arg)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "Operator, left argument and right argument of the involved computation.")
	   (effect  "No effects.")
	   (value   "A nataccomp representing the computation."))
  (let* ((theory-name (natac=compute-name-of-necessary-theory operator left-arg right-arg))
	 (preliminary-nataccomp (natac=compute-preliminary-nataccomp operator left-arg right-arg theory-name)))
    (unless theory-name (natac=error operator left-arg right-arg))
    (if (and (or (natac=theory-names-eql theory-name natac*natural)
		 (natac=theory-names-eql theory-name natac*integer))
	     (or (natac=functions-eql operator natac*plus)
		 (natac=functions-eql operator natac*times)
		 (natac=functions-eql operator natac*power)))
	(let ((theorems (natac=compute-theorems preliminary-nataccomp)))
	  (natac=compute-combination-of-preliminary-nataccomp-and-theorems preliminary-nataccomp theorems))
      preliminary-nataccomp)))

(defun natac=nataccomp-operator (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp.")
	   (effect  "No effects.")
	   (value   "The operator of the computation."))
  (car nataccomp))

(defun natac=nataccomp-arguments (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp.")
	   (effect  "No effects.")
	   (value   "The arguments (list) of the computation."))
  (cadr nataccomp))

(defun natac=nataccomp-left-arg (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp.")
	   (effect  "No effects.")
	   (value   "The left argument of the computation."))
  (car (natac=nataccomp-arguments nataccomp)))

(defun natac=nataccomp-right-arg (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp.")
	   (effect  "No effects.")
	   (value   "The right argument of the computation."))
  (cadr (natac=nataccomp-arguments nataccomp)))

(defun natac=nataccomp-theory-name (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp.")
	   (effect  "No effects.")
	   (value   "The name of the theory necessary for the computation."))
  (caddr nataccomp))

(defun natac=nataccomp-theorems (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A natacomp.")
	   (effect  "No effects.")
	   (value   "The theorems (list) useful for the computation (maybe empty)."))
  (cadddr nataccomp))

(defun natac=nataccomp-base-theorem (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp with a nonempty theorem-part.")
	   (effect  "No effects.")
	   (value   "The base theorem for the computation."))
  (car (natac=nataccomp-theorems nataccomp)))

(defun natac=nataccomp-step-theorem (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp with a nonempty theorem-part.")
	   (effect  "No effects.")
	   (value   "The step theorem for the computation."))
  (cadr (natac=nataccomp-theorems nataccomp)))

(defun natac=nataccomp-abs-smaller-num (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp with a nonempty right argument which must be a natacnum (not a sort).")
	   (effect  "No effects.")
	   (value   "The left argument if its absolute value is less than"
		    "the absolute value of the second argument."
		    "The right argument otherwise."))
  (let ((left-arg (natac=nataccomp-left-arg nataccomp))
	(right-arg (natac=nataccomp-right-arg nataccomp)))
    (natac=abs-smaller-natacnum left-arg right-arg)))

(defun natac=nataccomp-abs-greater-num (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp with a nonempty right argument which must be a natacnum (not a sort).")
	   (effect  "No effects.")
	   (value   "The right argument if the absolute value of the left argument is less than"
		    "the absolute value of the right argument."
		    "The left argument otherwise."))
  (let ((left-arg (natac=nataccomp-left-arg nataccomp))
	(right-arg (natac=nataccomp-right-arg nataccomp)))
    (natac=abs-greater-natacnum left-arg right-arg)))

(defun natac=nataccomp-rec-num (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp with a nonempty right argument which must be a natacnum (not a sort).")
	   (effect  "No effects.")
	   (value   "The number used for recursion"
		    "(usually the one with smaller absolute value, except for operator {\\tt power} where"
		    "recursion is always done with right argument)."))
  (let ((operator (natac=nataccomp-operator nataccomp)))
    (if (natac=functions-eql operator natac*power)
	(natac=nataccomp-right-arg nataccomp)
      (natac=nataccomp-abs-smaller-num nataccomp))))

(defun natac=nataccomp-base-num (nataccomp)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp with a nonempty right argument which must be a natacnum (not a sort).")
	   (effect  "No effects.")
	   (value   "The number NOT used for recursion"
		    "(usually the one with greater absolute value, except for operator {\\tt power} where"
		    "base num is always the left argument)."))
  (let ((operator (natac=nataccomp-operator nataccomp)))
    (if (natac=functions-eql operator natac*power)
	(natac=nataccomp-left-arg nataccomp)
      (natac=nataccomp-abs-greater-num nataccomp))))


#{
\subsection{Theories and Sorts}
#}

(defun natac=theory-names-eql (theory-name-1 theory-name-2)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "Two theory names.")
	   (effect  "No effects.")
	   (value   "T iff both names are string-equal."))
  (natac=names-eql theory-name-1 theory-name-2))

(defun natac=sort-p (term)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A term.")
	   (effect  "No effects.")
	   (value   "{\\tt term} itself if {\\tt term} is a sort,"
		    "i.e. is a member of {\\tt natac*sort-list}."))
  (find term
	natac*sort-list
	:test #'natac=sorts-eql))

(defun natac=sorts-eql (sort-1 sort-2)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "Two sorts. Either as names of sorts or as objects, mixed possible.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt sort-1} and {\\tt sort-2} represent the same function."))
  (natac=eql sort-1 sort-2))

(defun natac=array-index-of-sort (sort)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A sort (either as name or as object).")
	   (effect  "No effects.")
	   (value   "The number for array access in {\\tt natac*theory-array}."))
  (cond ((natac=sorts-eql sort natac*nat) 0)
	((natac=sorts-eql sort natac*nnat) 1)
	((natac=sorts-eql sort natac*int) 1)
	((natac=sorts-eql sort natac*rat) 2)
	(t (error "~a is yet an unknown number sort." sort))))

(defun natac=array-index-of-operator (operator)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "An operator (either as name or as object).")
	   (effect  "No effects.")
	   (value   "The number for array access in {\\tt natac*theory-array}."))
  (cond ((natac=functions-eql operator natac*plus) 0)
	((natac=functions-eql operator natac*minus) 1)
	((natac=functions-eql operator natac*times) 2)
	((natac=functions-eql operator natac*divide) 3)
	((natac=functions-eql operator natac*power) 4)
	((natac=functions-eql operator natac*div) 5)
	((natac=functions-eql operator natac*mod) 6)
	((natac=functions-eql operator natac*gcd) 7)
	((natac=functions-eql operator natac*lcm) 8)
	((natac=functions-eql operator natac*less) 9)
	((natac=functions-eql operator natac*leq) 10)
	((natac=functions-eql operator natac*greater) 11)
	((natac=functions-eql operator natac*geq) 12)
	((natac=functions-eql operator natac*=) 13)
	((natac=functions-eql operator natac*in) 14)
	(t (error "Expansion of numerical function ~a is not yet implemented." operator))))

(defun natac=compute-name-of-necessary-theory (operator left-arg right-arg)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "Operator, left argument and right argument of the involved computation.")
	   (effect  "No effects.")
	   (value   "The name of the theory necessary for the computation."))
  (let ((left-sort (natac=natacnum-sort left-arg))
	(right-sort (natac=natacnum-sort right-arg)))
    (aref natac*theory-array
	  (natac=array-index-of-operator operator)
	  (natac=array-index-of-sort left-sort)
	  (natac=array-index-of-sort right-sort))))

(defun natac=extract-i-th-sort-from-theorem-node (theorem-node i)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A proof node (usually of an imported theorem) with at least"
		    "i introductory nested {\\tt forall-sort}'s."
		    "A positive (LISP) number i.")
	   (effect  "No effects.")
	   (value   "The sort corresponding to the i-th {\\tt forall-sort}."))
  (let ((base-pos (pos~add-front 2)))
    (labels ((natac=get-pos-of-sort (pos i)
				    (if (zerop i)
					pos
				      (natac=get-pos-of-sort (pos~add-front 1 (pos~add-front 0 pos)) (1- i)))))
      (data~struct-at-position (node~formula theorem-node)
			       (natac=get-pos-of-sort base-pos (1- i))))))

(defun natac=extract-outer-sort-from-theorem-node (theorem-node)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A proof node (usually of an inserted theorem) with"
		    "a beginning {\\tt forall-sort}.")
	   (effect  "No effects.")
	   (value   "The sort corresponding to this {\\tt forall-sort}."))
  (natac=extract-i-th-sort-from-theorem-node theorem-node 1))

(defun natac=extract-inner-sort-from-theorem-node (theorem-node)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A proof node (usually of an inserted theorem) with at least"
		    "two introductory nested {\\tt forall-sort}'s.")
	   (effect  "No effects.")
	   (value   "The sort corresponding to the second {\\tt forall-sort}."))
  (natac=extract-i-th-sort-from-theorem-node theorem-node 2))

(defun natac=extract-sort-from-theorem-node (theorem-node)
  (declare (edited  "04-JUL-2000")
	   (authors Fisch)
	   (input   "A proof node (usually of an inserted theorem) with"
		    "a beginning {\\tt forall-sort}.")
	   (effect  "No effects.")
	   (value   "The sort corresponding to this {\\tt forall-sort}."))
  (natac=extract-outer-sort-from-theorem-node theorem-node))


#{
\subsection{Theorems}
#}

(defun natac=compute-theorems (preliminary-nataccomp)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A preliminary nataccomp, i.e. without the 'theorem'-slot.")
	   (effect  "No effects.")
	   (value   "A list of names of relevant theorems for recursion (maybe empty)."))
  (let ((theory-name (natac=nataccomp-theory-name preliminary-nataccomp)))
    (cond ((natac=theory-names-eql theory-name natac*natural)
	   (natac=compute-nat-theorems preliminary-nataccomp))
	  ((natac=theory-names-eql theory-name natac*integer)
	   (natac=compute-int-theorems preliminary-nataccomp))
	  (t nil))))

(defun natac=compute-nat-theorems (preliminary-nataccomp)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A preliminary nataccomp.")
	   (effect  "No effects.")
	   (value   "A list of names of relevant theorems for recursion in theory {\\tt natural} (maybe empty)."))
  (let ((operator (natac=nataccomp-operator preliminary-nataccomp)))
    (cond ((natac=functions-eql operator natac*plus)
	   (natac=compute-nat-plus-theorems preliminary-nataccomp))
	  ((natac=functions-eql operator natac*times)
	   (natac=compute-nat-times-theorems preliminary-nataccomp))
	  ((natac=functions-eql operator natac*power)
	   (natac=compute-nat-power-theorems)))))

(defun natac=compute-nat-plus-theorems (preliminary-nataccomp)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A preliminary nataccomp.")
	   (effect  "No effects.")
	   (value   "A list of base and step theorem for operator {\\tt plus}"
		    "in theory {\\tt natural}."))
  (let ((right-arg (natac=nataccomp-right-arg preliminary-nataccomp))
	(rec-num (natac=nataccomp-rec-num preliminary-nataccomp)))
    (if (natac=natacnums-eq rec-num right-arg)
	(list natac*plus-nat-base natac*plus-nat-step)
      (list natac*plus-nat-base2 natac*plus-nat-step2))))

(defun natac=compute-nat-times-theorems (preliminary-nataccomp)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A preliminary nataccomp.")
	   (effect  "No effects.")
	   (value   "A list of base and step theorem for operator {\\tt times}"
		    "in theory {\\tt natural}."))
  (let ((right-arg (natac=nataccomp-right-arg preliminary-nataccomp))
	(rec-num (natac=nataccomp-rec-num preliminary-nataccomp)))
    (if (natac=natacnums-eq rec-num right-arg)
	(list natac*times-nat-base natac*times-nat-step)
      (list natac*times-nat-base2 natac*times-nat-step2))))

(defun natac=compute-nat-power-theorems ()
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A preliminary nataccomp.")
	   (effect  "No effects.")
	   (value   "A list of base and step theorem for operator {\\tt power}"
		    "in theory {\\tt natural}."))
  (list natac*power-nat-base natac*power-nat-step))
  
(defun natac=compute-int-theorems (preliminary-nataccomp)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A preliminary nataccomp.")
	   (effect  "No effects.")
	   (value   "A list of names of relevant theorems for recursion in theory {\\tt integer} (maybe empty)."))
  (let ((operator (natac=nataccomp-operator preliminary-nataccomp)))
    (cond ((natac=functions-eql operator natac*plus)
	   (natac=compute-int-plus-theorems preliminary-nataccomp))
	  ((natac=functions-eql operator natac*times)
	   (natac=compute-int-times-theorems preliminary-nataccomp))
	  ((natac=functions-eql operator natac*power)
	   (natac=compute-int-power-theorems)))))

(defun natac=compute-int-plus-theorems (preliminary-nataccomp)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A preliminary nataccomp.")
	   (effect  "No effects.")
	   (value   "A list of base and step theorem for operator {\\tt plus}"
		    "in theory {\\tt integer}."))
  (let ((right-arg (natac=nataccomp-right-arg preliminary-nataccomp))
	(rec-num (natac=nataccomp-rec-num preliminary-nataccomp)))
    (if (natac=natacnums-eq rec-num right-arg)
	(if (not (natac=natacnum-minusp rec-num))
	    (list natac*plus-int-base natac*plus-int-step-s)
	  (list natac*plus-int-base natac*plus-int-step-p))
      (if (not (natac=natacnum-minusp rec-num))
	  (list natac*plus-int-base2 natac*plus-int-step2-s)
	(list natac*plus-int-base2 natac*plus-int-step2-p)))))

(defun natac=compute-int-times-theorems (preliminary-nataccomp)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A preliminary nataccomp.")
	   (effect  "No effects.")
	   (value   "A list of base and step theorem for operator {\\tt times}"
		    "in theory {\\tt integer}."))
  (let ((right-arg (natac=nataccomp-right-arg preliminary-nataccomp))
	(rec-num (natac=nataccomp-rec-num preliminary-nataccomp)))
    (if (natac=natacnums-eq rec-num right-arg)
	(if (not (natac=natacnum-minusp rec-num))
	    (list natac*times-int-base natac*times-int-step-s)
	  (list natac*times-int-base natac*times-int-step-p))
      (if (not (natac=natacnum-minusp rec-num))
	  (list natac*times-int-base2 natac*times-int-step2-s)
	(list natac*times-int-base2 natac*times-int-step2-p)))))

(defun natac=compute-int-power-theorems ()
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A preliminary nataccomp.")
	   (effect  "No effects.")
	   (value   "A list of base and step theorem for operator {\\tt power}"
		    "in theory {\\tt integer}."))
  (list natac*power-int-base natac*power-int-step))


#{
\subsection{Objects and Names}
#}

(defun natac=object-p (thing)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "Something.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt thing} is an object rather than a name."))
  (term~p thing))

(defun natac=obj-from-name (name)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A name.")
	   (effect  "No effects.")
	   (value   "The corresponding object."))
  (env~lookup-object name (pds~environment omega*current-proof-plan)))

(defun natac=return-object (thing)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A name or an object.")
	   (effect  "No effects.")
	   (value   "{\\tt thing} itself if thing is an object."
		    "The corresponding object if {\\tt thing} is the name of an object."))
  (if (natac=object-p thing)
      thing
    (natac=obj-from-name thing)))

(defun natac=objects-eql (obj-1 obj-2)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "Two objects.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt obj-1} and {\\tt obj-2} are data~equal and neither of them is NIL."))
  (unless (or (null obj-1)
	      (null obj-2))
    (or (data~equal obj-1 obj-2)
	(data~schema-equal obj-1 obj-2))))

(defun natac=eql (thing-1 thing-2)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "Two names or objects.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt thing-1} and {\\tt thing-2} refer to the same object and neither of them is NIL."))
  (unless (or (null thing-1)
	      (null thing-2))
    (let ((obj-1 (natac=return-object thing-1))
	  (obj-2 (natac=return-object thing-2)))
      (natac=objects-eql obj-1 obj-2))))

(defun natac=names-eql (name-1 name-2)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "Two names.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt name-1} and {\\tt name-2} are string-equal."))
  (string-equal name-1 name-2))

(defun natac=pos-of-term-with-function-from-line (function line &optional start-pos)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A function, either as name or as object."
		    "A proof line."
		    "Optional: a position."
		    "{\\tt function} must appear exactly once in {\\tt line} or"
		    "if the optional position is given {\\tt function} must appear exactly once"
		    "in the substruct at the given position in {\\tt line}.")
	   (effect  "No effects.")
	   (value   "The position of the substruct in {\\tt line} with {\\tt function}."))
  (car (natac=pos-list-of-terms-with-function-from-line function line start-pos)))

(defun natac=pos-list-of-terms-with-function-from-line (function line &optional start-pos)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A function, either as name or as object."
		    "A proof line."
		    "Optional: a position.")
	   (effect  "No effects.")
	   (value   "A list of the positions of the substructs in {\\tt line} with {\\tt function}."))
  (if start-pos
      (mapcar #'(lambda (pos)
		  (pos~concatenate start-pos pos))
	      (natac=pos-list-of-terms-with-function-from-struct function (data~struct-at-position (node~formula line) start-pos)))
    (natac=pos-list-of-terms-with-function-from-struct function (node~formula line))))

(defun natac=pos-list-of-terms-with-function-from-struct (function struct)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A function, either as name or as object."
		    "A struct.")
	   (effect  "No effects.")
	   (value   "A list of the positions of the substructs in {\\tt struct} with {\\tt function}."))
  (let ((func-obj (natac=return-object function)))
    (mapcar #'pos~butlast (data~substruct-positions func-obj struct))))

(defun natac=pos-of-term-with-function-from-struct (function struct)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A function, either as name or as object."
		    "A struct."
		    "{\\tt function} must appear exactly once in {\\tt struct}.")
	   (effect  "No effects.")
	   (value   "The position of the substructs in {\\tt struct} with {\\tt function}."))
  (car (natac=pos-list-of-terms-with-function-from-struct function struct)))

(defun natac=pos-list-of-terms-with-arith-function-from-struct (struct)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A struct.")
	   (effect  "No effects.")
	   (value   "A list of the positions of the substructs in {\\tt struct}"
		    "with an arithmetic function (in natac*function-list)."))
  (apply #'append (natac=sort-pos-list (natac=function-position-list struct))))

(defun natac=pos-of-term-with-arith-function-from-struct (struct)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A struct."
		    "There must be exactly one arithmetic function in {\\tt struct}.")
	   (effect  "No effects.")
	   (value   "The position of the substructs in {\\tt struct}"
		    "with the arithmetic function (in natac*function-list)."))
  (car (natac=pos-list-of-terms-with-arith-function-from-struct struct)))

(defun natac=pos-list-of-terms-with-arith-function-from-line (line &optional start-pos)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A proof line."
		    "Optional: a position.")
	   (effect  "No effects.")
	   (value   "A list of the positions of the substructs in {\\tt line}"
		    "with an arithmetic function (in natac*function-list)."))
  (if start-pos
      (mapcar #'(lambda (pos)
		  (pos~concatenate start-pos pos))
	      (natac=pos-list-of-terms-with-arith-function-from-struct (data~struct-at-position (node~formula line) start-pos)))
    (natac=pos-list-of-terms-with-arith-function-from-struct (node~formula line))))

(defun natac=pos-of-term-with-arith-function-from-line (line &optional start-pos)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A proof line."
		    "Optional: a position."
		    "There must be exactly one arithmetic function in {\\tt line} or"
		    "if the optional position is given there must be exactly one"
		    "arithmetic function in the substruct at the given position in {\\tt line}.")
	   (effect  "No effects.")
	   (value   "The position of the substruct in {\\tt line}"
		    "with the arithmetic function (in natac*function-list)."))
  (car (natac=pos-list-of-terms-with-arith-function-from-line line start-pos)))

(defun natac=tactics-eql (tactic-1 tactic-2)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "Two names of tactics.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt tactic-1} and {\\tt tactic-2} are eq."))
  (eq tactic-1 tactic-2))


#{
\subsection{Hashtables}
#}

(setf (get 'theorems-hash 'key-predicate)
      nil)
(setf (get 'theorems-hash 'explanation)
      nil)

(setf (get 'natural-numbers-hash 'key-predicate)
      #'(lambda (key)
	  (and (integerp key)
	       (not (minusp key)))))
(setf (get 'natural-numbers-hash 'explanation)
      "a nonnegative integer")
  
(setf (get 'neg-natural-numbers-hash 'key-predicate)
      #'(lambda (key)
	  (and (integerp key)
	       (not (plusp key)))))
(setf (get 'neg-natural-numbers-hash 'explanation)
      "a nonpositive integer")

(setf (get 'integer-numbers-hash 'key-predicate)
      #'integerp)
(setf (get 'integer-numbers-hash 'explanation)
      "an integer")

(setf (get 'rational-numbers-hash 'key-predicate)
      #'rationalp)
(setf (get 'rational-numbers-hash 'explanation)
      "a rational number")
  
(setf (get 'change-sign-hash 'key-predicate)
      #'rationalp)
(setf (get 'change-sign-hash 'explanation)
      "a rational number")
  
(defun natac~remove-hashes-of-proof-plan (proof-plan)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "The proof plan whose hashes are to be removed.")
	   (effect  "Clears all hashes in {\\tt natac*hash-list}.")
	   (value   "Undefined."))
  (let ((key-list (mapcar #'(lambda (hash)
			      (list proof-plan hash))
			  natac*hash-list)))
    (mapc #'(lambda (key)
	      (clrhash (gethash key natac*hashtable))
	      (remhash key natac*hashtable))
	  key-list)))

(defun natac=hash-values-eq (value-1 value-2)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "Two hash values.")
	   (effect  "No effects.")
	   (value   "T iff {\\tt value-1} and {\\tt value-2} are eq."))
  (eq value-1 value-2))

(defun natac=debug-hashes-key-integrity (hash key-predicate explanation)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A hash."
		    "A predicate."
		    "A string.")
	   (effect  "Tests whether all keys in {\\tt hash} satisfy {\\tt key-predicate}"
		    "and prints the result to terminal, using explanation.")
	   (value   "NIL."))
  (format t "~%Each key is ~a :  ~a"
	  explanation
	  (with-hash-table-iterator (it hash)
				    (labels ((test (entry-found &optional key value)
						   (if entry-found
						       (and (funcall key-predicate key)
							    (multiple-value-call #'test (it)))
						     t)))
				      (multiple-value-call #'test (it))))))

(defun natac=debug-hashes-integrity-1 (hash)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A hash.")
	   (effect  "Tests whether there is no hash entry with value NIL in {\\tt hash}"
		    "and prints the result to terminal.")
	   (value   "NIL."))
  (format t "~%No hash entry with value NIL :  ~a"
	  (with-hash-table-iterator (it hash)
				    (labels ((test (entry-found &optional key value)
						   (if entry-found
						       (and value (multiple-value-call #'test (it)))
						     t)))
				      (multiple-value-call #'test (it))))))

(defun natac=debug-hashes-integrity-2 (hash)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A hash.")
	   (effect  "Tests whether any value in {\\tt hash} appears twice"
		    "and prints the result to terminal.")
	   (value   "NIL."))
  (format t "~%No value appears twice :  ~a"
	  (with-hash-table-iterator
	   (it-1 hash)
	   (labels ((test-1 (entry-found-1 &optional key-1 value-1)
			    (if entry-found-1
				(and (= 1
					(with-hash-table-iterator
					 (it-2 hash)
					 (labels ((test-2 (entry-found-2 &optional key-2 value-2)
							  (if entry-found-2
							      (if (natac=hash-values-eq value-1 value-2)
								  (+ 1 (multiple-value-call #'test-2 (it-2)))
								(multiple-value-call #'test-2 (it-2)))
							    0)))
					   (multiple-value-call #'test-2 (it-2)))))
				     (multiple-value-call #'test-1 (it-1)))
			      t)))
	     (multiple-value-call #'test-1 (it-1))))))

(defun natac=debug-hashes-integrity-3 (hash)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A hash.")
	   (effect  "Tests whether any value in {\\tt hash} is a proof node"
		    "and prints the result to terminal.")
	   (value   "NIL."))
  (format t "~%Each value is a proof node :  ~a"
	  (with-hash-table-iterator (it hash)
				    (labels ((test (entry-found &optional key value)
						   (if entry-found
						       (and (pdsn~p value)
							    (multiple-value-call #'test (it)))
						     t)))
				      (multiple-value-call #'test (it))))))

(defun natac=debug-hashes-print-general-information (hash)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A hash.")
	   (effect  "Prints some information on hash {\\tt hash} to terminal: size, number of elements currently stored in,"
		    "test predicate, rehash-size and rehash-threshold.")
	   (value   "NIL."))
  (format t "~%Size :  ~a~%Number of hash entries :  ~a~%Test :  ~a~%Rehash-size :  ~a~%Rehash-threshold :  ~a~%"
	  (hash-table-size hash)
	  (hash-table-count hash)
	  (hash-table-test hash)
	  (hash-table-rehash-size hash)
	  (hash-table-rehash-threshold hash)))

(defun natac=debug-hashes-show-hash-content (hash)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A hash.")
	   (effect  "Prints the content of {\\tt hash} to terminal (key and value).")
	   (value   "NIL."))
  (format t "~%~%Content :~%")
  (maphash #'(lambda (key value)
	       (format t "~a     ~a~%" key value))
	   hash))

(defun natac=debug-hashes (&optional (debug-level 1))
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "Optional: A (LISP) integer greater than or equal to 0 and smaller than or equal to 2.")
	   (effect  "Prints some debug information on all hashes in {\\tt natac*hash-list} to terminal."
		    "The greater {\\tt debug-level} is, the more information is displayed.")
	   (value   "Undefined."))
  (let ((current-proof-plan omega*current-proof-plan))
    (mapcar #'(lambda (hash-name)
		(let* ((global-key (list current-proof-plan hash-name))
		       (hash (gethash global-key natac*hashtable))
		       (key-predicate (get hash-name 'key-predicate))
		       (explanation (get hash-name 'explanation)))
		  (format t "~%~%Hash :  ~a" hash-name)
		  (natac=debug-hashes-print-general-information hash)
		  (when (> debug-level 0)
		    (natac=debug-hashes-integrity-1 hash)
		    (natac=debug-hashes-integrity-2 hash)
		    (natac=debug-hashes-integrity-3 hash)
		    (unless (null key-predicate)
		      (natac=debug-hashes-key-integrity hash
							key-predicate
							explanation)))
		  (when (> debug-level 1)
		    (natac=debug-hashes-show-hash-content hash))))
	    natac*hash-list)
    (format t "~%~%")))
  
(defun natac=gethash (key hash)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "The key for the hash entry we want to get."
		    "The hash we want to access.")
	   (effect  "No effects.")
	   (value   "The value of the hash entry with key {\\tt key} in {\\tt hash}."))
  (let* ((current-proof-plan omega*current-proof-plan)
	 (global-key (list current-proof-plan hash)))
    (gethash key
	     (gethash global-key
		      natac*hashtable))))

(defun natac=sethash (key hash value)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "The key for the hash entry we want to set."
		    "The hash we want to create a new entry in."
		    "The value for the hash entry we want to set.")
	   (effect  "Creates a new hash entry in {\\tt hash} with key {\\tt key} and value {\\tt value}.")
	   (value   "{\\tt value}."))
  (let* ((current-proof-plan omega*current-proof-plan)
	 (global-key (list current-proof-plan hash))
	 (real-hash (gethash global-key natac*hashtable)))
    (setf (gethash key real-hash) value)))



#{
\section{Expansion Functions}
#}


(defun natac=get-theorem (theorem theory)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "The name of a theorem."
		    "The name of a theory.")
	   (effect  "If {\\tt theorem} is not yet present in {\\tt theorems-hash} it will be inserted"
		    "into the current proof plan and a new entry with key {\\tt theorem} and"
		    "value {\\tt new-proof-line} will be created in {\\tt theorems-hash}.")
	   (value   "The proof line containing {\\tt theorem}."))
  (cond ((natac=gethash theorem 'theorems-hash))
	(t (natac=sethash theorem
			  'theorems-hash
			  (tacl~insert&return-assumption theory theorem)))))

(defun natac=get-nat-theorem (theorem)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "The name of a theorem from theory {\\tt natural}.")
	   (effect  "If {\\tt theorem} is not yet present in {\\tt theorems-hash} it will be inserted"
		    "into the current proof plan and a new entry with key {\\tt theorem} and"
		    "value {\\tt new-proof-line} will be created in {\\tt theorems-hash}.")
	   (value   "The proof line containing {\\tt theorem}."))
  (natac=get-theorem theorem natac*natural))

(defun natac=get-int-theorem (theorem)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "The name of a theorem from theory {\\tt integer}.")
	   (effect  "If {\\tt theorem} is not yet present in {\\tt theorems-hash} it will be inserted"
		    "into the current proof plan and a new entry with key {\\tt theorem} and"
		    "value {\\tt new-proof-line} will be created in {\\tt theorems-hash}.")
	   (value   "The proof line containing {\\tt theorem}."))
  (natac=get-theorem theorem natac*integer))

(defun natac=get-rat-theorem (theorem)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "The name of a theorem from theory {\\tt rational}.")
	   (effect  "If {\\tt theorem} is not yet present in {\\tt theorems-hash} it will be inserted"
		    "into the current proof plan and a new entry with key {\\tt theorem} and"
		    "value {\\tt new-proof-line} will be created in {\\tt theorems-hash}.")
	   (value   "The proof line containing {\\tt theorem}."))
  (natac=get-theorem theorem natac*rational))

(defun natac=get-num-sort (natacnum sort)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum."
		    "A sort."
		    "{\\tt natacnum} must be of sort {\\tt sort}.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The proof line which states that {\\tt natacnum} is of sort {\\tt sort}."))
  (cond ((natac=sorts-eql sort natac*nat)
	 (natac=get-num-nat natacnum))
	((natac=sorts-eql sort natac*nnat)
	 (natac=get-num-nnat natacnum))
	((natac=sorts-eql sort natac*int)
	 (natac=get-num-int natacnum))
	((natac=sorts-eql sort natac*rat)
	 (natac=get-num-rat natacnum))))
  
(defun natac=get-num-nat (natacnum)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A natural natacnum.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The proof line which states that {\\tt natacnum} is natural."))
  (let ((num-as-value (natac=natacnum-value natacnum)))
    (cond ((natac=gethash num-as-value 'natural-numbers-hash))
	  ((zerop num-as-value)
	   (natac=sethash 0 'natural-numbers-hash (natac=get-nat-theorem natac*zero-nat)))
	  (t (let* ((new-num-as-natacnum (natac=pos-int-natacnum-decrement natacnum))
		    (new-num-as-setnum (natac=natacnum-setnum new-num-as-natacnum)))
	       (natac=sethash num-as-value
			      'natural-numbers-hash
			      (car (tacl~apply natac*foralle-sort
					       (list nil
						     (natac=get-nat-theorem natac*succ-nat)
						     (natac=get-num-nat new-num-as-natacnum))
					       (list new-num-as-setnum)))))))))

(defun natac=get-num-nnat (natacnum)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A nonpostive integer natacnum.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The proof line which states that {\\tt natacnum} is a nonpositive integer."))
  (let ((num-as-value (natac=natacnum-value natacnum)))
    (cond ((natac=gethash num-as-value 'neg-natural-numbers-hash))
	  ((zerop num-as-value)
	   (natac=sethash 0 'neg-natural-numbers-hash (natac=get-int-theorem natac*zero-nnat)))
	  (t (let* ((new-num-as-natacnum (natac=neg-int-natacnum-increment natacnum))
		    (new-num-as-setnum (natac=natacnum-setnum new-num-as-natacnum)))
	       (natac=sethash num-as-value
			      'neg-natural-numbers-hash
			      (car (tacl~apply natac*foralle-sort
					       (list nil
						     (natac=get-int-theorem natac*pred-nnat)
						     (natac=get-num-nnat new-num-as-natacnum))
					       (list new-num-as-setnum)))))))))
	    
(defun natac=get-num-int (natacnum)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "An integer natacnum.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The proof line which states that {\\tt natacnum} is integer."))
  (let ((num-as-value (natac=natacnum-value natacnum))
	(num-as-setnum (natac=natacnum-setnum natacnum)))
    (cond ((natac=gethash num-as-value 'integer-numbers-hash))
	  ((minusp num-as-value)
	   (natac=sethash num-as-value
			  'integer-numbers-hash
			  (car (tacl~apply natac*foralle-sort
					   (list nil
						 (natac=get-int-theorem natac*nnat-int)
						 (natac=get-num-nnat natacnum))
					   (list num-as-setnum)))))
	  (t
	   (natac=sethash num-as-value
			  'integer-numbers-hash
			  (car (tacl~apply natac*foralle-sort
					   (list nil
						 (natac=get-int-theorem natac*nat-int)
						 (natac=get-num-nat natacnum))
					   (list num-as-setnum))))))))

(defun natac=get-num-rat (natacnum)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A rational natacnum.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The proof line which states that {\\tt natacnum} is rational."))
  (let ((num-as-value (natac=natacnum-value natacnum)))
    (cond ((natac=gethash num-as-value 'rational-numbers-hash))
	  ((natac=natacnum-integerp natacnum)
	   (let ((num-as-setnum (natac=natacnum-setnum natacnum)))
	     (natac=sethash num-as-value
			    'rational-numbers-hash
			    (car (tacl~apply natac*foralle-sort
					     (list nil
						   (natac=get-rat-theorem natac*int-rat)
						   (natac=get-num-int natacnum))
					     (list num-as-setnum))))))
	  (t
	   (let* ((numerator (natac=canc-rat-natacnum-numerator natacnum))
		  (denominator (natac=canc-rat-natacnum-denominator natacnum))
		  (numerator-as-setnum (natac=natacnum-setnum numerator))
		  (denominator-as-setnum (natac=natacnum-setnum denominator))
		  (impe (tacl~sequence (foralle-1 (natac*foralle-sort (list nil
									    (natac=get-rat-theorem natac*rat-crit)
									    (natac=get-num-int numerator))
								      (list numerator-as-setnum)))
				       (foralle-2 (natac*foralle-sort (list nil
									    (car foralle-1)
									    (natac=get-num-nat denominator))
								      (list denominator-as-setnum)))
				       (impe (natac*impe (list nil
							       nil
							       (car foralle-2))
							 nil))))
		  (conc (car impe))
		  (prem (cadr impe))
		  (andi (tacl~apply natac*andi (list prem nil nil) nil))
		  (left-conj (cadr andi))
		  (right-conj (caddr andi))
		  (pos-of-gcd (natac=pos-of-term-with-function-from-line natac*gcd right-conj)))
	     (natac=apply-arith-simplify left-conj)
	     (natac=apply-expand-num right-conj nil pos-of-gcd t)
	     (natac=sethash num-as-value 'rational-numbers-hash conc))))))

(defun natac=simpl-single-change-sign-int-cont (natacnum)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "An integer natacnum.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The proof line that states:"
		    " (= (change-sign {\\tt natacnum}) -{\\tt natacnum}"))
  (let* ((base-theorem natac*change-sign-base)
	 (step-theorem (if (natac=natacnum-plusp natacnum)
			   natac*change-sign-s
			 natac*change-sign-p))
	 (position (pos~list-position '(2 1))))
    (labels ((natac=change-sign-rec
	      (natacnum)
	      (let ((num-as-value (natac=natacnum-value natacnum)))
		(cond ((natac=gethash num-as-value 'change-sign-hash))
		      ((let ((line-for-reverse (natac=gethash (natac=value-change-sign num-as-value) 'change-sign-hash)))
			 (when line-for-reverse
			   (natac=sethash num-as-value
					  'change-sign-hash
					  (natac=change-sign-from-reverse natacnum
									  line-for-reverse)))))
		      ((zerop num-as-value)
		       (natac=sethash 0
				      'change-sign-hash
				      (natac=get-int-theorem base-theorem)))
		      (t (let* ((new-natacnum (natac=int-natacnum-decrement-abs-value natacnum))
				(new-num-as-setnum (natac=natacnum-setnum new-natacnum))
				(step-theorem-node (natac=get-int-theorem step-theorem))
				(sort (natac=extract-sort-from-theorem-node step-theorem-node)))
			   (natac=sethash num-as-value
					  'change-sign-hash
					  (car (tacl~sequence
						(foralle (natac*foralle-sort (list nil
										   step-theorem-node
										   (natac=get-num-sort new-natacnum sort))
									     (list new-num-as-setnum)))
						(=subst (natac*=subst (list nil
									    (car foralle)
									    (natac=change-sign-rec new-natacnum))
								      (list position))))))))))))
      (natac=change-sign-rec natacnum))))



(defun natac~expand-simplify-num (outline parameter)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "Outline and parameter from an application of 'simplify-num'.")
	   (effect  "Preparation of expansion.")
	   (value   "Undefined."))
  (natac=expand-sen-start outline parameter natac*simplify-num t))

(defun natac~expand-expand-num (outline parameter)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "Outline and parameter from an application of 'expand-num'.")
	   (effect  "Preparation of expansion.")
	   (value   "Undefined."))
  (natac=expand-sen-start outline parameter natac*expand-num t))

(defun natac~expand-arith-simplify (outline parameter)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "Outline and parameter from an application of 'arith-simplify'.")
	   (effect  "Preparation of expansion.")
	   (value   "Undefined."))
  (let* ((formula (node~formula (car outline)))
	 (function (natac=get-arith-function formula)))
    (if (and function (equal function :in)                           ;;;; (in xxxxx Real)  hack        VS
	     (term~primitive-p (second (data~appl-arguments formula)))
	     (string-equal :real (keim~name (second (data~appl-arguments formula)))))
	(progn
	  (tacl~init outline)
	  (tacl~apply 'in-real outline nil)
	  (tacl~end))
      (natac=expand-as-start outline parameter t))))

(defun natac=expand-sen-start (outline parameter tactic &optional first)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Outline and parameter from an application of either 'simplify-num'"
		    "or 'expand-num'."
		    "Optional: A flag indicating whether this is the parent expansion.")
	   (effect  "Initialization of expansion.")
	   (value   "Undefined."))
  (when (or natac*expansion first)
    (let* ((term-1 (data~struct-at-position (node~formula (car outline)) (car parameter)))
	   (term-2 (data~struct-at-position (node~formula (cadr outline)) (car parameter)))
	   (operator (if (and (data~appl-p term-1)
			      (natac=arith-function-p (data~appl-function term-1)))
			 (data~appl-function term-1)
		       (data~appl-function term-2)))
	   (args (if (and (data~appl-p term-1)
			  (natac=arith-function-p (data~appl-function term-1)))
		     (data~appl-arguments term-1)
		   (data~appl-arguments term-2)))
	   (single-computation (and (natac~num-p (car args))
				    (natac~num-p (cadr args)))))
      (tacl~init outline)
      (if single-computation
	  (natac=expand-sen-base-init outline parameter operator args)
	(natac=expand-sen-step-init outline parameter tactic))
      (tacl~end))))

(defun natac=expand-sen-step-init (outline parameter tactic)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Outline and parameter from an application of either 'simplify-num'"
		    "or 'expand-num'."
		    "The tactic which was applied (either 'simplify-num' or 'expand-num'.")
	   (effect  "Step by step simplification.")
	   (value   "Undefined."))
  (if (natac~num-p (data~struct-at-position (node~formula (car outline)) (car parameter)))
      (let* ((unsimpl-line (cadr outline))
	     (simpl-line (car outline))
	     (position-list (mapcar #'(lambda (pos)
					(pos~concatenate (car parameter) pos))
				    (car (natac=sort-pos-list
					  (natac=function-position-list
					   (data~struct-at-position (node~formula unsimpl-line) (car parameter))))))))
	(natac=expand-sen-step unsimpl-line simpl-line position-list tactic))
    (let* ((unsimpl-line (car outline))
	   (simpl-line (cadr outline))
	   (position-list (mapcar #'(lambda (pos)
				      (pos~concatenate (car parameter) pos))
				  (car (natac=sort-pos-list
					(natac=function-position-list
					 (data~struct-at-position (node~formula unsimpl-line) (car parameter))))))))
      (natac=expand-sen-step unsimpl-line simpl-line position-list tactic))))

(defun natac=expand-sen-step (unsimpl-line simpl-line pos-list tactic)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "The unsimplified line."
		    "The simplified line."
		    "The list of positions where simplification was applied."
		    "The tactic applied.")
	   (effect  "Continues step by step simplification.")
	   (value   "Undefined."))
  (if (natac=tactics-eql tactic natac*simplify-num)
      (natac=apply-simplify-num unsimpl-line simpl-line pos-list)
    (natac=apply-expand-num unsimpl-line simpl-line pos-list)))

(defun natac=expand-as-start (outline parameter &optional first)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Outline and parameter from an application of 'arith-simplify'."
		    "Optional: A flag indicating whether this is the parent expansion.")
	   (effect  "Initialization of expansion.")
	   (value   "Undefined."))
  (when (or natac*expansion first)
    (labels ((natac=pos-of-operator () (pos~add-front 0))
	     (natac=operator () (let* ((line (car outline))
				       (pos-of-operator (natac=pos-of-operator)))
				  (data~struct-at-position (node~formula line) pos-of-operator)))
	     (natac=pos-of-left-operand () (pos~add-front 1))
	     (natac=left-operand () (let* ((line (car outline))
					   (pos-of-left-operand (natac=pos-of-left-operand)))
				      (data~struct-at-position (node~formula line) pos-of-left-operand)))
	     (natac=pos-of-right-operand () (pos~add-front 2))
	     (natac=right-operand () (let* ((line (car outline))
					    (pos-of-right-operand (natac=pos-of-right-operand)))
				       (data~struct-at-position (node~formula line) pos-of-right-operand))))
      (let* ((operator (natac=operator))
	     (left-operand (natac=left-operand))
	     (right-operand (natac=right-operand))
	     (single-computation (and (natac~num-p left-operand)
				      (or (natac~num-p right-operand)
					  (natac=sort-p right-operand)))))
	(tacl~init outline)
	(if single-computation
	    (let ((left-arg (natac=compute-natacnum left-operand))
		  (right-arg (natac=compute-natacnum right-operand)))
	      (natac=expand-as-base-init operator left-arg right-arg (car outline)))
	  (natac=expand-as-step-init outline parameter))
	(tacl~end)))))

(defun natac=expand-as-step-init (outline parameter)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Outline and parameter from an application of 'arith-simplify'.")
	   (effect  "Step by step simplification.")
	   (value   "Undefined."))
  (natac=apply-arith-simplify
   (natac=apply-expand-num-all (car outline) nil)))

(defun natac=apply-simplify-num (unsimpl-line simpl-line pos-or-pos-list &optional final-=ref func2num num2func)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A line with unsimplified arithmetic expressions."
		    "The same line with the arithmetic expressions referred to by {\\tt pos-or-pos-list}"
		    "simplified. Or NIL."
		    "A position or a list of positions where arithmetic simplifications in {\\tt unsimpl-line}"
		    "are possible."
		    "Optional: A flag indicating whether the (open) line resulting from all simplifications"
		    "          should be closed (using '=ref')."
		    "          A flag indicating whether 'func2num' should be applied to the given line(s)"
		    "          before simplification."
		    "          A flag indicating whether 'num2func' should be applied to the resulting line"
		    "          after simplification.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "Applies 'simplify-num' to the arithmetic expressions in {\\tt unsimpl-line} referred to by {\\tt pos-or-pos-list}"
		    "step by step and returns the resulting line."))
  (labels ((natac=apply-simplify-num-cont
            (unsimpl-line simpl-line pos-list)
            (if (null pos-list)
                unsimpl-line
              (let* ((new-unsimpl-line (natac=apply-simplify-num-cont unsimpl-line nil (rest pos-list)))
                     (pos (car pos-list))
                     (parameter (list pos
                                      (data~struct-at-position (node~formula new-unsimpl-line) pos)))
                     (outline (tacl~apply natac*simplify-num
                                          (list simpl-line
                                                new-unsimpl-line)
                                          parameter)))
                (natac=expand-sen-start outline parameter natac*simplify-num)
                (car outline)))))
    (when func2num (natac~func2num-line unsimpl-line))
    (when (and func2num simpl-line) (natac~func2num-line simpl-line))
    (let* ((pos-list (if (listp pos-or-pos-list) pos-or-pos-list (list pos-or-pos-list)))
	   (final-line (natac=apply-simplify-num-cont unsimpl-line simpl-line pos-list)))
      (cond (final-=ref (natac=apply-=ref final-line))
            (num2func (natac~num2func-line final-line)
		      final-line)
            (t final-line)))))

(defun natac=apply-simplify-num-all (unsimpl-line simpl-line &optional final-=ref func2num num2func)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A line with unsimplified arithmetic expressions."
		    "The same line with the arithmetic expressions simplified. Or NIL."
		    "Optional: A flag indicating whether the (open) line resulting from all simplifications"
		    "          should be closed (using '=ref')."
		    "          A flag indicating whether 'func2num' should be applied to the given line(s)"
		    "          before simplification."
		    "          A flag indicating whether 'num2func' should be applied to the resulting line"
		    "          after simplification.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "Applies 'simplify-num' to all arithmetic expressions in {\\tt unsimpl-line}"
		    "step by step and returns the resulting line."))
  (let ((position-list (apply #'append (natac=sort-pos-list (natac=function-position-list (node~formula unsimpl-line))))))    
    (natac=apply-simplify-num unsimpl-line simpl-line position-list final-=ref func2num num2func)))
  
(defun natac=apply-expand-num (unsimpl-line simpl-line pos-or-pos-list &optional final-=ref func2num num2func)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A line with unsimplified arithmetic expressions."
		    "The same line with the arithmetic expressions referred to by {\\tt pos-or-pos-list}"
		    "simplified. This line must be open. Or NIL."
		    "A position or a list of positions where arithmetic simplifications in {\\tt unsimpl-line}"
		    "are possible."
		    "Optional: A flag indicating whether the (open) line resulting from all simplifications"
		    "          should be closed (using '=ref')."
		    "          A flag indicating whether 'func2num' should be applied to the given line(s)"
		    "          before simplification."
		    "          A flag indicating whether 'num2func' should be applied to the resulting line"
		    "          after simplification.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "Applies 'expand-num' to the arithmetic expressions in {\\tt unsimpl-line} referred to by {\\tt pos-or-pos-list}"
		    "step by step and returns the resulting line."))
  (labels ((natac=apply-expand-num-cont
	    (unsimpl-line simpl-line pos-list)
	    (if (null pos-list)
		unsimpl-line
	      (let* ((new-unsimpl-line (natac=apply-expand-num-cont unsimpl-line nil (rest pos-list)))
		     (pos (car pos-list))
		     (parameter (list pos
				      (data~struct-at-position (node~formula new-unsimpl-line) pos)))
		     (outline (tacl~apply natac*expand-num
					  (list new-unsimpl-line
						simpl-line)
					  parameter)))
		(natac=expand-sen-start outline parameter natac*expand-num)
		(cadr outline)))))
    (when func2num (natac~func2num-line unsimpl-line))
    (when (and func2num simpl-line) (natac~func2num-line simpl-line))
    (let* ((pos-list (if (listp pos-or-pos-list) pos-or-pos-list (list pos-or-pos-list)))
	   (final-line (natac=apply-expand-num-cont unsimpl-line simpl-line pos-list)))
      (cond (final-=ref (natac=apply-=ref final-line))
	    (num2func (natac~num2func-line final-line)
		      final-line)
	    (t final-line)))))

(defun natac=apply-expand-num-all (unsimpl-line simpl-line &optional final-=ref func2num num2func)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A line with unsimplified arithmetic expressions."
		    "The same line with the arithmetic expressions simplified. Or NIL."
		    "Optional: A flag indicating whether the (open) line resulting from all simplifications"
		    "          should be closed (using '=ref')."
		    "          A flag indicating whether 'func2num' should be applied to the given line(s)"
		    "          before simplification."
		    "          A flag indicating whether 'num2func' should be applied to the resulting line"
		    "          after simplification.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "Applies 'simplify-num' to all arithmetic expressions in {\\tt unsimpl-line}"
		    "step by step and returns the resulting line."))
  (let ((position-list (apply #'append (natac=sort-pos-list (natac=function-position-list (node~formula unsimpl-line))))))
    (natac=apply-expand-num unsimpl-line simpl-line position-list final-=ref func2num num2func)))
  
(defun natac=error (operator left-arg right-arg)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Operator, left and right argument of an invalid computation.")
	   (effect  "Error.")
	   (value   "Undefined."))
  (let ((prob-theory-name (keim~name (prob~theory omega*current-proof-plan)))
	(operator-name (keim~name operator))
	(left-arg-as-value (natac=natacnum-value left-arg))
	(right-arg-as-value (if (natac=sort-p right-arg)
				right-arg
			      (natac=natacnum-value right-arg))))
    (error "~a cannot be computed (in theory ~a)."
	   (list operator-name left-arg-as-value right-arg-as-value)
	   prob-theory-name)))

(defun natac=expand-sen-base-init (outline parameter operator args)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Outline and parameter from an application of either 'simplify-num'"
		    "or 'expand-num'."
		    "The operator."
		    "The argument(s) as list.")
	   (effect  "Base simplification.")
	   (value   "Undefined."))
  (let* ((left-operand (natac=compute-natacnum (car args)))
	 (right-operand (natac=compute-natacnum (cadr args)))
	 (nataccomp (natac=compute-nataccomp operator left-operand right-operand))
	 (open-eq-line (natac=create-open-eq-line outline parameter)))
    (natac~num2func-line open-eq-line)
    (natac=close-open-line (cond ((natac=functions-eql operator natac*plus)
				  (natac=simpl-plus nataccomp open-eq-line))
				 ((natac=functions-eql operator natac*minus)
				  (natac=simpl-minus nataccomp open-eq-line))
				 ((natac=functions-eql operator natac*times)
				  (natac=simpl-times nataccomp open-eq-line))
				 ((natac=functions-eql operator natac*power)
				  (natac=simpl-power nataccomp open-eq-line))
				 ((natac=functions-eql operator natac*divide)
				  (natac=simpl-divide nataccomp open-eq-line))
				 ((natac=functions-eql operator natac*div)
				  (natac=simpl-div nataccomp open-eq-line))
				 ((natac=functions-eql operator natac*mod)
				  (natac=simpl-mod nataccomp open-eq-line))
				 ((natac=functions-eql operator natac*gcd)
				  (natac=simpl-gcd nataccomp open-eq-line))
				 ((natac=functions-eql operator natac*lcm)
				  (natac=simpl-lcm nataccomp open-eq-line))))))

(defun natac=expand-as-base-init (operator left-arg right-arg line)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Operator, left and right argument (as natacnums) from an application"
		    "of 'arith-simplify'."
		    "The line 'arith-simplify' was applied to.")
	   (effect  "Base simplification.")
	   (value   "Undefined."))
  (let ((nataccomp (natac=compute-nataccomp operator left-arg right-arg)))
    (natac~num2func-line line)
    (natac=close-open-line (cond ((natac=functions-eql operator natac*less)
				  (natac=simpl-less nataccomp line))
				 ((natac=functions-eql operator natac*leq)
				  (natac=simpl-leq nataccomp line))
				 ((natac=functions-eql operator natac*greater)
				  (natac=simpl-greater nataccomp line))
				 ((natac=functions-eql operator natac*geq)
				  (natac=simpl-geq nataccomp line))
				 ((natac=functions-eql operator natac*=)
				  (natac=simpl-= nataccomp line))
				 (t (natac=simpl-in nataccomp line))))))

(defun natac=create-open-eq-line (outline parameter)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "Outline and parameter of an application of either 'simplify-num' or"
		    "'expand-num' or 'arith-simplify'.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "An open line having the form:"
		    " (= unsimplified-arithmetic-expression simplified-arithmetic-expression)."))
  (let* ((new-line (caddr (tacl~apply natac*=subst
				      (list (car outline)
					    (cadr outline)
					    nil)
				      (list (car parameter)))))
	 (position (pos~add-front 1))
	 (term (data~struct-at-position (node~formula new-line) position)))
    (if (and (data~appl-p term)
	     (natac=arith-function-p (data~appl-function term)))
	new-line
      (cadr (tacl~apply natac*=sym (list new-line nil) nil)))))

(defun natac=close-open-line (line-to-close)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "A line.")
	   (effect  "Further proof lines can be inserted..")
	   (value   "If {\\tt line-to-close} is already closed {\\tt line-to-close} itself is returned."
		    "If {\\tt line-to-close} contains no further arithmetic functions it is assumed"
		    "to have the form:"
		    " (= x x) where x is a number in an arbitrary representation"
		    "{\\tt line-to-close} is then closed by '=ref' and returned."
		    "Otherwise the remaining arithmetic expressions are simplified and"
		    "the resulting line is closed by '=ref' and returned."))
  (cond ((not (pdsn~open-node-p line-to-close))
	 line-to-close)
	((null (natac=pos-list-of-terms-with-arith-function-from-line line-to-close))
	 (natac=apply-=ref line-to-close))
	(t
	 (let* ((position (caar (natac=sort-pos-list (natac=function-position-list (node~formula line-to-close)))))
		(parameter (list position
				 (data~struct-at-position (node~formula line-to-close)
							  position)))
		(outline (tacl~apply natac*expand-num
				     (list line-to-close
					   nil)
				     parameter)))
	   (natac=apply-=ref (cadr outline))
	   (natac=expand-sen-start outline
				   parameter
				   natac*expand-num)))))

(defun natac=apply-=ref (line-to-close)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "An open line having the form:"
		    " (= x x).")
	   (effect  "No effects.")
	   (value   "{\\tt line-to-close}, now closed."))
  (tacl~apply natac*=ref
	      (list line-to-close)
	      (list (data~struct-at-position (node~formula line-to-close) (pos~add-front 1)))))

(defun natac=change-sign (line &optional natacnum)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "An open line with a formula:"
		    " (= unsimplified-expression simplified-expression)"
		    " where unsimplified-expression contains an arbitrary number of 'change-sign's."
		    "Optional: A natacnum. If {\\tt natacnum} is given it must be the argument of each (!)"
		    "'change-sign' in {\\tt line}.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "An open line with all appearances of 'change-sign' simplified."))
  (let ((change-sign-pos-list (natac=pos-list-of-terms-with-function-from-line natac*change-sign line)))
    (natac=change-sign-cont change-sign-pos-list line natacnum)))

(defun natac=change-sign-cont (position-list line natacnum)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "A list of positions of expressions with 'change-sign' in {\\tt line}."
		    "A line."
		    "Either a natacnum or NIL."
		    "If {\\tt natacnum} is not NIL {\\tt natacnum} must be the argument of each (!)"
		    "'change-sign' in {\\tt line}.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "An open line with all appearances of 'change-sign' simplified."))
  (if (null position-list)
      line
    (natac=change-sign-cont (rest position-list)
			    (natac=simpl-single-change-sign (car position-list)
							    line
							    natacnum)
			    natacnum)))

(defun natac=simpl-single-change-sign (position line natacnum)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A position referring to an expression in {\\tt line} which has the form:"
		    " (change-sign x) where x is a rational number."
		    "A line."
		    "Either a natacnum or NIL."
		    "If {\\tt natacnum} is not NIL {\\tt natacnum} must be the argument of the"
		    "'change-sign' in {\\tt line} at position {\\tt position}.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "An open line with the 'change-sign' at position {\\tt position} simplified."))
  (labels ((natac=operand ()
			  (labels ((natac=pos-of-operand () (pos~add-end 1 position)))
			    (let ((pos-of-operand (natac=pos-of-operand)))
			      (natac=compute-natacnum (data~struct-at-position (node~formula line) pos-of-operand))))))
    (let ((operand (cond (natacnum)
			 (t (natac=operand)))))
      (if (not (natac=natacnum-integerp operand))
	  (let* ((new-line (natac=simpl-single-change-sign-rat position line natacnum))
		 (new-position (pos~add-end 1 position))
		 (new-natacnum (natac=canc-rat-natacnum-numerator natacnum)))
	    (natac=simpl-single-change-sign-int new-position new-line new-natacnum))
	(natac=simpl-single-change-sign-int position line natacnum)))))

(defun natac=simpl-single-change-sign-int (position line natacnum)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A position referring to an expression in {\\tt line} which has the form:"
		    " (change-sign x) where x is an integer."
		    "A line."
		    "Either a natacnum or NIL."
		    "If {\\tt natacnum} is not NIL {\\tt natacnum} must be the argument of the"
		    "'change-sign' in {\\tt line} at position {\\tt position}.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "An open line with the 'change-sign' at position {\\tt position} simplified."))
  (if (pdsn~open-node-p line)
      (cadr (tacl~apply natac*=subst
			(list line nil (natac=simpl-single-change-sign-int-cont natacnum))
			(list position)))
    (car (tacl~apply natac*=subst
		     (list nil line (natac=simpl-single-change-sign-int-cont natacnum))
		     (list position)))))

(defun natac=change-sign-from-reverse (natacnum line-for-reverse)
  (declare (edited  "05-JUL-2000")
	   (authors Fisch)
	   (input   "An integer natacnum."
		    "The proof line that states:"
		    " (= (change-sign -{\\tt natacnum}) {\\tt natacnum})")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The proof line that states:"
		    " (= (change-sign {\\tt natacnum}) -{\\tt natacnum}"))
  (let* ((reverse-theorem natac*change-sign-reverse)
	 (num-as-setnum (natac=natacnum-setnum natacnum))
	 (change-sign-natacnum (natac=natacnum-change-sign natacnum))
	 (change-sign-num-as-setnum (natac=natacnum-setnum change-sign-natacnum)))
    (car (tacl~sequence
	  (foralle-1 (natac*foralle-sort (list nil
					       (natac=get-int-theorem reverse-theorem)
					       (natac=get-num-int change-sign-natacnum))
					 (list change-sign-num-as-setnum)))
	  (foralle-2 (natac*foralle-sort (list nil
					       (car foralle-1)
					       (natac=get-num-int natacnum))
					 (list num-as-setnum)))
	  (impe (natac*impe (list nil line-for-reverse (car foralle-2)) nil))))))

(defun natac=simpl-single-change-sign-rat (position line natacnum)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A position referring to an expression in {\\tt line} which has the form:"
		    " (change-sign x) where x is a non-integer rational."
		    "A line."
		    "Either a natacnum or NIL."
		    "If {\\tt natacnum} is not NIL {\\tt natacnum} must be the argument of the"
		    "'change-sign' in {\\tt line} at position {\\tt position}.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "An open line with the 'change-sign' at position {\\tt position} simplified."))
  (if (natac=natacnum-integerp natacnum)
      line
    (let* ((num-as-setnum (natac=natacnum-setnum natacnum))
	   (new-line (natac=rat-simpl-numerators-and-denominators
		      (car (tacl~apply natac*foralle-sort
				       (list nil
					     (natac=get-rat-theorem natac*change-sign-rat)
					     (natac=get-num-rat natacnum))
				       (list num-as-setnum))))))
      (if (pdsn~open-node-p line)
	  (cadr (tacl~apply natac*=subst
			    (list line nil new-line)
			    (list position)))
	(car (tacl~apply natac*=subst
			 (list nil line new-line)
			 (list position)))))))

(defun natac=simpl-sp-or-ps (nataccomp line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp (from the previous computation, usually an addition"
		    "of a postive integer and a negative one."
		    "An open line having the form:"
		    " (= (s (s (s ... (s (p (p (p ... (p zero)...))))...))) x) or"
		    " (= (p (p (p ... (p (s (s (s ... (s zero)...))))...))) x) .")
	   (effect  "Further proof lines can be inserted.")
	   (value   "A closed line having the form:"
		    " (= x x)."))
  (let* ((rec-num (natac=nataccomp-rec-num nataccomp))
	 (rec-num-as-value (natac=natacnum-value rec-num))
	 (base-num (natac=nataccomp-base-num nataccomp))
	 (base-num-as-value (natac=natacnum-value base-num)))
    (if (or (and (not (minusp rec-num-as-value))
		 (not (minusp base-num-as-value)))
	    (and (not (plusp rec-num-as-value))
		 (not (plusp base-num-as-value))))
	line
      (let* ((theorem-node (if (plusp rec-num-as-value)
			       (natac=get-int-theorem natac*sp-int)
			     (natac=get-int-theorem natac*ps-int)))
	     (sort (natac=extract-sort-from-theorem-node theorem-node))
	     (initial-num (natac=int-natacnum-decrement-abs-value base-num))
	     (initial-pos (natac=simpl-sp-or-ps-initial-pos nataccomp)))
	(labels ((natac=simpl-sp-or-ps-rec
		  (num pos line)
		  (let ((num-as-value (natac=natacnum-value num))
			(num-as-setnum (natac=natacnum-setnum num)))
		    (if (or (natac=value-zerop num-as-value)
			    (natac=values-abs-values-eql (natac=values-minus base-num-as-value num-as-value)
							 rec-num-as-value))
			(car (tacl~apply natac*foralle-sort
					 (list line
					       theorem-node
					       (natac=get-num-sort num sort))
					 (list num-as-setnum)))
		      (let ((new-num (natac=int-natacnum-decrement-abs-value num))
			    (new-pos (pos~rest pos))
			    (new-line (cadr (tacl~sequence
					     (foralle (natac*foralle-sort (list nil
										theorem-node
										(natac=get-num-sort num sort))
									  (list num-as-setnum)))
					     (=subst (natac*=subst (list line
									 nil
									 (car foralle))
								   (list pos)))))))
			(natac=simpl-sp-or-ps-rec new-num new-pos new-line))))))
	  (natac=simpl-sp-or-ps-rec initial-num initial-pos line))))))

(defun natac=simpl-sp-or-ps-initial-pos (nataccomp)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp (from the previous computation, usually an addition"
		    "of a postive integer and a negative one.")
	   (effect  "No effects.")
	   (value   "The position for the first simplification."))
  (let* ((rec-num (natac=nataccomp-rec-num nataccomp))
	 (abs-value-of-rec-num (natac=natacnum-abs-value rec-num)))
    (labels ((natac=simpl-sp-or-ps-initial-pos-rec
	      (value)
	      (if (zerop value)
		  (pos~empty)
		(pos~add-front 1 (natac=simpl-sp-or-ps-initial-pos-rec (natac=value-decrement value))))))
      (natac=simpl-sp-or-ps-initial-pos-rec abs-value-of-rec-num))))

(defun natac=rat-expand-fraction (canceled-fraction factor)
  (declare (edited  "20-JUL-2000")
	   (authors Fisch)
	   (input   "A canceled fraction as natacnum."
		    "The integer factor for multiplication as natacnum.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "A closed line having the form:"
		    " (= uncanceled-fraction canceled-fraction) is returned."))
  (labels ((natac=left-disjunct-as-term
	    (line)
	    (labels ((natac=pos-of-left-disjunct () (pos~add-front 1)))
	      (let ((pos-of-left-disjunct (natac=pos-of-left-disjunct)))
		(data~struct-at-position (node~formula line) pos-of-left-disjunct))))
	   (natac=right-disjunct-as-term
	    (line)
	    (labels ((natac=pos-of-right-disjunct () (pos~add-front 2)))
	      (let ((pos-of-right-disjunct (natac=pos-of-right-disjunct)))
		(data~struct-at-position (node~formula line) pos-of-right-disjunct)))))
    (let* ((pos-of-canceled-fraction (pos~add-front 2))
	   (numerator-of-canceled-fraction (natac=canc-rat-natacnum-numerator canceled-fraction))
	   (denominator-of-canceled-fraction (natac=canc-rat-natacnum-denominator canceled-fraction))
	   (numerator-of-canceled-fraction-as-setnum (natac=natacnum-setnum numerator-of-canceled-fraction))
	   (denominator-of-canceled-fraction-as-setnum (natac=natacnum-setnum denominator-of-canceled-fraction))
	   (factor-as-setnum (natac=natacnum-setnum factor))
	   (impe (tacl~sequence
		  (foralle-1 (natac*foralle-sort (list nil
						       (natac=get-rat-theorem natac*cancel-fraction)
						       (natac=get-num-int numerator-of-canceled-fraction))
						 (list numerator-of-canceled-fraction-as-setnum)))
		  (foralle-2 (natac*foralle-sort (list nil
						       (car foralle-1)
						       (natac=get-num-nat denominator-of-canceled-fraction))
						 (list denominator-of-canceled-fraction-as-setnum)))
		  (foralle-3 (natac*foralle-sort (list nil
						       (car foralle-2)
						       (natac=get-num-int factor))
						 (list factor-as-setnum)))
		  (impe (natac*impe (list nil nil (car foralle-3)) nil))))
	   (conc (car impe))
	   (prem (cadr impe))
	   (andi (tacl~apply natac*andi (list prem nil nil) nil))
	   (left-conj (cadr andi))
	   (right-conj (caddr andi))
	   (pos-list-of-times (natac=pos-list-of-terms-with-function-from-line natac*times conc)))
      (natac=apply-arith-simplify
       (if (natac=natacnum-minusp factor)
	   (let ((left-disjunct-as-term (natac=left-disjunct-as-term left-conj)))
	     (cadr (tacl~apply natac*orir
			       (list left-conj
				     nil)
			       (list left-disjunct-as-term))))
	 (let ((right-disjunct-as-term (natac=right-disjunct-as-term left-conj)))
	   (cadr (tacl~apply natac*oril
			     (list left-conj
				   nil)
			     (list right-disjunct-as-term))))))
      (natac=apply-arith-simplify right-conj)
      (if (natac=natacnum-integerp canceled-fraction)
	  (let* ((preliminary-line (natac=apply-simplify-num conc nil pos-list-of-times nil))
		 (fraction (natac=compute-natacnum (data~struct-at-position (node~formula preliminary-line)
									    (pos~add-front 2)))))
	    (car (tacl~apply natac*=subst
			     (list nil
				   preliminary-line
				   (natac=rat-make-rat-to-int fraction))
			     (list pos-of-canceled-fraction))))
	(natac=apply-simplify-num conc nil pos-list-of-times nil)))))
  
(defun natac=rat-cancel-fraction (line &optional pos-of-uncanc-fraction)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "An open line."
		    "Optional: The position of an uncanceled fraction in {\\tt line}."
		    "If {\\tt pos-of-uncanc-fraction} is not given {\\tt line} is assumed to"
		    "have the form:"
		    " (= uncanceled-fraction canceled-fraction).")
	   (effect  "Further proof lines can be inserted.")
	   (value   "If {\\tt pos-of-uncanc-fraction} is given the uncanceled fraction in {\\tt line} at"
		    "position {\\tt pos-of-uncanc-fraction} is replaced by its canceled representation"
		    "and this new line is returned."
		    "Otherwise a closed line proving"
		    " (= uncanceled-fraction canceled-fraction) is returned."))
  (labels ((natac=pos-of-uncanceled-fraction ()
					     (cond (pos-of-uncanc-fraction)
						   (t (pos~add-front 1))))
	   (natac=uncanceled-fraction
	    ()
	    (let ((pos-of-uncanceled-fraction (natac=pos-of-uncanceled-fraction)))
	      (natac=compute-natacnum (data~struct-at-position (node~formula line) pos-of-uncanceled-fraction))))
	   (natac=canceled-fraction
	    (uncanceled-fraction)
	    (if (not pos-of-uncanc-fraction)
		(labels ((natac=pos-of-canceled-fraction () (pos~add-front 2)))
		  (let ((pos-of-canceled-fraction (natac=pos-of-canceled-fraction)))
		    (natac=compute-natacnum (data~struct-at-position (node~formula line) pos-of-canceled-fraction))))
	      (let ((uncanceled-fraction-as-value (natac=natacnum-value uncanceled-fraction)))
		(natac=compute-natacnum uncanceled-fraction-as-value)))))
    (let* ((pos-of-uncanceled-fraction (natac=pos-of-uncanceled-fraction))
	   (uncanceled-fraction (natac=uncanceled-fraction))
	   (numerator-of-uncanceled-fraction (natac=rat-natacnum-numerator uncanceled-fraction))
	   (canceled-fraction (natac=canceled-fraction uncanceled-fraction)))
      (cond ((or (natac=natacnum-integerp uncanceled-fraction)
		 (natac=rat-natacnum-canceled-p uncanceled-fraction))
	     line)
	    ((natac=int-natacnum-zerop numerator-of-uncanceled-fraction)
	     (let ((final-line (natac=rat-make-rat-to-int uncanceled-fraction)))
	       (if pos-of-uncanc-fraction
		   final-line
		 (cadr (tacl~apply natac*=subst
				   (list line nil final-line)
				   (list pos-of-uncanceled-fraction))))))
	    (t
	     (let ((final-line (natac=rat-expand-fraction canceled-fraction
							  (natac=factor-for-expansion canceled-fraction uncanceled-fraction))))
	       (if pos-of-uncanc-fraction
		   final-line
		 (cadr (tacl~apply natac*=subst
				   (list line nil final-line)
				   (list pos-of-uncanceled-fraction))))))))))

(defun natac=rat-make-int-to-rat (nataccomp open-eq-line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= operator x y)")
	   (effect  "Further proof lines can be inserted.")
	   (value   "If x and/or y are integers they are replaced by the corresponding"
		    "fraction representation (denominator = 1) and the resulting line"
		    "is returned."))
  (labels ((natac=pos-of-left-arg () (pos~list-position '(1 1)))
	   (natac=pos-of-right-arg () (pos~list-position '(1 2))))
    (let ((left-arg (natac=nataccomp-left-arg nataccomp))
	  (right-arg (natac=nataccomp-right-arg nataccomp)))
      (cond ((natac=natacnum-integerp left-arg)
	     (let ((pos-of-left-arg (natac=pos-of-left-arg)))
	       (natac=rat-make-int-to-rat-cont left-arg pos-of-left-arg open-eq-line)))
	    ((natac=natacnum-integerp right-arg)
	     (let ((pos-of-right-arg (natac=pos-of-right-arg)))
	       (natac=rat-make-int-to-rat-cont right-arg pos-of-right-arg open-eq-line)))
	    (t
	     open-eq-line)))))

(defun natac=rat-make-int-to-rat-cont (natacnum position open-eq-line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "An integer natacnum."
		    "The position of {\\tt natacnum} in {\\tt open-eq-line}."
		    "An open line")
	   (effect  "Further proof lines can be inserted.")
	   (value   "{\\tt natacnum} is replaced by its fraction representation"
		    "(denominator = 1) and the resulting line is returned."))
  (let ((num-as-setnum (natac=natacnum-setnum natacnum)))
    (cadr (tacl~sequence
	   (foralle (natac*foralle-sort (list nil
					      (natac=get-rat-theorem natac*int-to-rat)
					      (natac=get-num-int natacnum))
					(list num-as-setnum)))
	   (=subst (natac*=subst (list open-eq-line
				       nil
				       (car foralle))
				 (list position)))))))

(defun natac=rat-make-rat-to-int (fraction)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "An uncanceled fraction with:"
		    " numerator equals zero or"
		    " denominator equals one.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "If {\\tt fraction} is whole-numbered although in fraction representation"
		    "a line proving the equality with the normalized representation"
		    "of {\\tt fraction} is returned."
		    "Otherwise nil."))
  (let ((numerator-of-fraction (natac=canc-rat-natacnum-numerator fraction))
	(denominator-of-fraction (natac=canc-rat-natacnum-denominator fraction)))
    (cond ((natac=int-natacnum-zerop numerator-of-fraction)
	   (natac=rat-make-rat-to-int-numerator-equals-zero fraction))
	  ((natac=int-natacnum-eql-one-p denominator-of-fraction)
	   (natac=rat-make-rat-to-int-cont fraction))
	  (t nil))))

(defun natac=rat-make-rat-to-int-numerator-equals-zero (natacnum)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A natacnum in fraction representation with value 0"
		    "(for example: (0 (frac zero (s (s (s zero)))) ).")
	   (effect  "Further proof lines can be inserted.")
	   (value   "A line proving the equality with the normalized representation"
		    "of {\\tt fraction} is returned."))
  (let* ((denominator (natac=rat-natacnum-denominator natacnum))
	 (denominator-as-setnum (natac=natacnum-setnum denominator))
	 (impe (tacl~sequence
		(foralle (natac*foralle-sort (list nil
						   (natac=get-rat-theorem natac*numerator-equals-zero)
						   (natac=get-num-nat denominator))
					     (list denominator-as-setnum)))
		(impe (natac*impe (list nil nil (car foralle)) nil))))
	 (conc (car impe))
	 (prem (cadr impe)))
    (natac=apply-arith-simplify prem)
    conc))
  
(defun natac=rat-make-rat-to-int-cont (natacnum)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A whole-numbered natacnum in fraction representation whose value does not equal zero."
		    "and whose denominator equals one.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "A line proving the equality with the normalized representation"
		    "of {\\tt fraction} is returned."))
  (let* ((numerator (natac=canc-rat-natacnum-numerator natacnum))
	 (numerator-as-setnum (natac=natacnum-setnum numerator)))
    (car (tacl~apply natac*foralle-sort
		     (list nil
			   (natac=get-rat-theorem natac*rat-to-int)
			   (natac=get-num-int numerator))
		     (list numerator-as-setnum)))))

(defun natac=rat-simpl-numerators-and-denominators (line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A line.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "All occurences of (numerator x) or (denominator x)"
		    "where x is a rational number are simplified."
		    "The resulting line is returned."))
  (let ((numerator-pos-list (natac=pos-list-of-terms-with-function-from-line natac*numerator line))
	(denominator-pos-list (natac=pos-list-of-terms-with-function-from-line natac*denominator line)))
    (natac=rat-simpl-numerators numerator-pos-list
				(natac=rat-simpl-denominators denominator-pos-list
							      line))))

(defun natac=rat-simpl-numerators (position-list line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A list of positions referring to expressions (numerator x) in {\\tt line}."
		    "A line.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "All occurences of (numerator x) referred to in {\\tt position-list}"
		    "where x is a rational number are simplified."
		    "The resulting line is returned."))
  (if (null position-list)
      line
    (natac=rat-simpl-numerators (rest position-list)
				(natac=rat-simpl-single-numerator (car position-list)
								  line))))

(defun natac=rat-simpl-denominators (position-list line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A list of positions referring to expressions (denominator x) in {\\tt line}."
		    "A line.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "All occurences of (denominator x) referred to in {\\tt position-list}"
		    "where x is a rational number are simplified."
		    "The resulting line is returned."))
  (if (null position-list)
      line
    (natac=rat-simpl-denominators (rest position-list)
				  (natac=rat-simpl-single-denominator (car position-list)
								      line))))

(defun natac=rat-simpl-single-numerator (position line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A position referring to an expression (numerator x) in {\\tt line}."
		    "A line.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The expression (numerator x) in {\\tt line} referred to by {\\tt position}"
		    "where x is a rational number is simplified."
		    "The resulting line is returned."))
  (labels ((natac=operand ()
			  (labels ((natac=pos-of-operand () (pos~add-end 1 position)))
			    (let ((pos-of-operand (natac=pos-of-operand)))
			      (natac=compute-natacnum (data~struct-at-position (node~formula line) pos-of-operand))))))
    (let ((operand (natac=operand)))
      (if (natac=natacnum-integerp operand)
	  (natac=rat-simpl-single-numerator-or-denominator-int position
							       line
							       operand
							       (natac=get-rat-theorem natac*numerator-of-int))
	(natac=rat-simpl-single-numerator-or-denominator-rat position
							     line
							     operand
							     (natac=get-rat-theorem natac*numerator-of-frac))))))

(defun natac=rat-simpl-single-denominator (position line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A position referring to an expression (denominator x) in {\\tt line}."
		    "A line.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The expression (denominator x) in {\\tt line} referred to by {\\tt position}"
		    "where x is a rational number is simplified."
		    "The resulting line is returned."))
  (labels ((natac=operand ()
			  (labels ((natac=pos-of-operand () (pos~add-end 1 position)))
			    (let ((pos-of-operand (natac=pos-of-operand)))
			      (natac=compute-natacnum (data~struct-at-position (node~formula line) pos-of-operand))))))
    (let ((operand (natac=operand)))
      (if (natac=natacnum-integerp operand)
	  (natac=rat-simpl-single-numerator-or-denominator-int position
							       line
							       operand
							       (natac=get-rat-theorem natac*denominator-of-int))
	(natac=rat-simpl-single-numerator-or-denominator-rat position
							     line
							     operand
							     (natac=get-rat-theorem natac*denominator-of-frac))))))

(defun natac=rat-simpl-single-numerator-or-denominator-int (position line operand theorem-node)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A position referring to an expression"
		    "(numerator {\\tt operand}) or (denominator {\\tt operand}) in {\\tt line}."
		    "A line."
		    "The Operand which must be an integer."
		    "The theorem node which serves to simplify the expression.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The expression (numerator {\\tt operand}) or (denominator {\\tt operand}) in {\\tt line}"
		    "referred to by {\\tt position}"
		    "where x is a rational number is simplified."
		    "The resulting line is returned."))
  (let* ((operand-as-setnum (natac=natacnum-setnum operand))
	 (foralle (tacl~apply natac*foralle-sort
			      (list nil
				    theorem-node
				    (natac=get-num-int operand))
			      (list operand-as-setnum))))
    (if (pdsn~open-node-p line)
	(cadr (tacl~apply natac*=subst
			  (list line
				nil
				(car foralle))
			  (list position)))
      (car (tacl~apply natac*=subst
		       (list nil
			     line
			     (car foralle))
		       (list position))))))

(defun natac=rat-simpl-single-numerator-or-denominator-rat (position line operand theorem-node)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A position referring to an expression"
		    "(numerator {\\tt operand}) or (denominator {\\tt operand}) in {\\tt line}."
		    "A line."
		    "The Operand which must be an non-integer rational number."
		    "The theorem node which serves to simplify the expression.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "The expression (numerator {\\tt operand}) or (denominator {\\tt operand}) in {\\tt line}"
		    "referred to by {\\tt position}"
		    "where x is a rational number is simplified."
		    "The resulting line is returned."))
  (let* ((numerator-of-operand (natac=canc-rat-natacnum-numerator operand))
	 (numerator-of-operand-as-setnum (natac=natacnum-setnum numerator-of-operand))
	 (denominator-of-operand (natac=canc-rat-natacnum-denominator operand))
	 (denominator-of-operand-as-setnum (natac=natacnum-setnum denominator-of-operand))
	 (impe
	  (tacl~sequence
	   (foralle-1 (natac*foralle-sort (list nil
						theorem-node
						(natac=get-num-int numerator-of-operand))
					  (list numerator-of-operand-as-setnum)))
	   (foralle-2 (natac*foralle-sort (list nil
						(car foralle-1)
						(natac=get-num-nat denominator-of-operand))
					  (list denominator-of-operand-as-setnum)))
	   (impe (natac*impe (list nil
				   (natac=get-num-rat operand)
				   (car foralle-2))
			     nil)))))
    (if (pdsn~open-node-p line)
	(cadr (tacl~apply natac*=subst
			  (list line
				nil
				(car impe))
			  (list position)))
      (car (tacl~apply natac*=subst
		       (list nil
			     line
			     (car impe))
		       (list position))))))

(defun natac=simpl-plus (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (plus x y) z), where x and y are rational numbers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let ((theory-name (natac=nataccomp-theory-name nataccomp)))
    (cond ((natac=theory-names-eql theory-name natac*natural)
	   (natac=simpl-plus-nat nataccomp open-eq-line))
	  ((natac=theory-names-eql theory-name natac*integer)
	   (natac=simpl-plus-int nataccomp open-eq-line))
	  ((natac=theory-names-eql theory-name natac*rational)
	   (natac=simpl-plus-rat nataccomp open-eq-line)))))

(defun natac=simpl-plus-nat (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (plus x y) z), where x and y are natural numbers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (natac=base-step nataccomp
		   (natac=recursion nataccomp
				    open-eq-line)))

(defun natac=simpl-plus-int (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (plus x y) z), where x and y are integers as setnums and at least"
		    " one of them is negative.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (natac=simpl-sp-or-ps nataccomp
			(natac=base-step nataccomp
					 (natac=recursion nataccomp
							  open-eq-line))))

(defun natac=base-step (nataccomp support-line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line to be closed by a base theorem of an operator"
		    "which is to be simplified recursively (currently 'plus' and 'times').")
	   (effect  "Further proof lines can be inserted.")
	   (value   "An open line having the form:"
		    " (= x x) where x is an integer a setnum."))
  (let* ((theory (natac=nataccomp-theory-name nataccomp))
	 (operator (natac=nataccomp-operator nataccomp))
	 (rec-num-as-natacnum (natac=nataccomp-rec-num nataccomp))
	 (rec-num-as-value (natac=natacnum-value rec-num-as-natacnum))
	 (base-num-as-natacnum (natac=nataccomp-base-num nataccomp))
	 (base-num-as-setnum (natac=natacnum-setnum base-num-as-natacnum))
	 (base-theorem-name (natac=nataccomp-base-theorem nataccomp))
	 (base-theorem-node (natac=get-theorem base-theorem-name theory))
	 (sort (natac=extract-sort-from-theorem-node base-theorem-node)))
    (if (zerop rec-num-as-value)
	(car (tacl~apply natac*foralle-sort
			 (list support-line
			       base-theorem-node
			       (natac=get-num-sort base-num-as-natacnum sort))
			 (list base-num-as-setnum)))
      (cadr (tacl~sequence
	     (foralle (natac*foralle-sort (list nil
						base-theorem-node
						(natac=get-num-sort base-num-as-natacnum sort))
					  (list base-num-as-setnum)))
	     (=subst (natac*=subst (list support-line
					 nil
					 (car foralle))
				   (list (pos~butlast (car (data~substruct-positions operator (node~formula support-line))))))))))))

(defun natac=recursion (nataccomp open-eq-line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= operator x y) z) where x and y are integers as setnums"
		    " and operator is to be simplified recursively (currently 'plus' and 'times').")
	   (effect  "Further proof lines can be inserted.")
	   (value   "An open line resulting from application(s) of the corresponding step theorem."
		    "This line should be further simplified by function 'natac=base-step'."))
  (labels ((natac=compute-new-pos-of-operator
	    (position-of-operator operator)
	    (if (natac=functions-eql operator natac*plus)
		(pos~add-end 1 position-of-operator)
	      (pos~add-end 2 position-of-operator))))
    (let* ((theory (natac=nataccomp-theory-name nataccomp))
	   (operator (natac=nataccomp-operator nataccomp))
	   (rec-num-as-natacnum (natac=nataccomp-rec-num nataccomp))
	   (rec-num-as-value (natac=natacnum-value rec-num-as-natacnum))
	   (base-num-as-natacnum (natac=nataccomp-base-num nataccomp))
	   (base-num-as-setnum (natac=natacnum-setnum base-num-as-natacnum))
	   (step-theorem-name (natac=nataccomp-step-theorem nataccomp))
	   (step-theorem-node (natac=get-theorem step-theorem-name theory))
	   (outer-sort (natac=extract-outer-sort-from-theorem-node step-theorem-node))
	   (inner-sort (natac=extract-inner-sort-from-theorem-node step-theorem-node)))
      (if (zerop rec-num-as-value)
	  open-eq-line
	(let ((initial-natacnum (natac=int-natacnum-decrement-abs-value rec-num-as-natacnum))
	      (initial-position (pos~butlast (car (data~substruct-positions operator (node~formula open-eq-line)))))
	      (support-line (car (tacl~apply natac*foralle-sort
					     (list nil
						   step-theorem-node
						   (natac=get-num-sort base-num-as-natacnum outer-sort))
					     (list base-num-as-setnum)))))
	  (labels ((natac=recursion-rec
		    (num-as-natacnum position-of-operator substituted-line)
		    (let* ((num-as-value (natac=natacnum-value num-as-natacnum))
			   (num-as-setnum (natac=natacnum-setnum num-as-natacnum))
			   (new-substituted-line (cadr (tacl~sequence
							(foralle (natac*foralle-sort (list nil
											   support-line
											   (natac=get-num-sort num-as-natacnum
													       inner-sort))
										     (list num-as-setnum)))
							(=subst (natac*=subst
								 (list substituted-line nil (car foralle))
								 (list position-of-operator)))))))
		      (if (zerop num-as-value)
			  new-substituted-line
			(let ((new-natacnum (natac=int-natacnum-decrement-abs-value num-as-natacnum))
			      (new-pos-of-operator (natac=compute-new-pos-of-operator position-of-operator operator)))
			  (natac=recursion-rec new-natacnum
					       new-pos-of-operator
					       new-substituted-line))))))
	    (natac=recursion-rec initial-natacnum
				 initial-position
				 open-eq-line)))))))

(defun natac=simpl-plus-rat (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (plus x y) z), where x and y are rationals as setnums and at least"
		    " one of them is not whole-numbered.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	 (right-arg (natac=nataccomp-right-arg nataccomp)))
    (natac=rat-cancel-fraction
     (cond ((natac=canc-rat-natacnums-equal-denominators-p left-arg right-arg)
	    (natac=simpl-plus-rat-equal-denoms nataccomp open-eq-line))
	   ((natac=canc-rat-natacnums-denominators-no-common-divisor-p left-arg right-arg)
	    (natac=simpl-plus-rat-greedy nataccomp open-eq-line))
	   (t
	    (natac=simpl-plus-rat-sophis nataccomp open-eq-line))))))

(defun natac=simpl-plus-rat-greedy (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (plus x y) z), where x and y are rationals as setnums and at least"
		    " one of them is not whole-numbered.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=pos-of-comp () (pos~add-front 1))
	   (natac=pos-of-result () (pos~add-front 2)))
    (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	   (right-arg (natac=nataccomp-right-arg nataccomp))
	   (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	   (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	   (num-and-denom-simpl
	    (natac=rat-simpl-numerators-and-denominators
	     (car (tacl~sequence
		   (foralle-1 (natac*foralle-sort (list nil
							(natac=get-rat-theorem natac*plus-rat)
							(natac=get-num-rat left-arg))
						  (list left-arg-as-setnum)))
		   (foralle-2 (natac*foralle-sort (list nil
							(car foralle-1)
							(natac=get-num-rat right-arg))
						  (list right-arg-as-setnum)))))))
	   (pos-of-comp (natac=pos-of-comp))
	   (pos-of-result (natac=pos-of-result))
	   (pos-list-for-simplification (natac=pos-list-of-terms-with-arith-function-from-line num-and-denom-simpl pos-of-result)))
      (cadr (tacl~apply natac*=subst
			(list open-eq-line
			      nil
			      (natac=apply-simplify-num num-and-denom-simpl nil pos-list-for-simplification nil))
			(list pos-of-comp))))))

(defun natac=simpl-plus-rat-equal-denoms (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (plus x y) z), where x and y are rationals as setnums"
		    " with the same denominator and at least"
		    " one of them is not whole-numbered.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=pos-for-subst () (pos~add-front 1))
	   (natac=start-pos () (pos~add-front 2)))
    (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	   (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	   (right-arg (natac=nataccomp-right-arg nataccomp))
	   (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	   (num-and-denom-simpl
	    (natac=rat-simpl-numerators-and-denominators
	     (car (tacl~sequence
		   (foralle-1 (natac*foralle-sort (list nil
							(natac=get-rat-theorem natac*plus-rat-equal-denoms)
							(natac=get-num-rat left-arg))
						  (list left-arg-as-setnum)))
		   (foralle-2 (natac*foralle-sort (list nil
							(car foralle-1)
							(natac=get-num-rat right-arg))
						  (list right-arg-as-setnum)))))))
	   (impe (tacl~apply natac*impe (list nil nil num-and-denom-simpl) nil))
	   (prem (cadr impe))
	   (conc (car impe))
	   (pos-for-subst (natac=pos-for-subst))
	   (start-pos (natac=start-pos))
	   (pos-of-plus (natac=pos-of-term-with-function-from-line natac*plus conc start-pos)))
      (natac=apply-=ref prem)
      (cadr (tacl~apply natac*=subst
			(list open-eq-line
			      nil
			      (natac=apply-simplify-num conc nil pos-of-plus nil))
			(list pos-for-subst))))))

(defun natac=simpl-plus-rat-sophis (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (plus x y) z), where x and y are rationals as setnums and at least"
		    " one of them is not whole-numbered.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=pos-for-subst () (pos~add-front 1))
	   (natac=start-pos () (pos~add-front 2))
	   (natac=pos-of-left-arg () (pos~list-position '(1 1)))
	   (natac=pos-of-right-arg () (pos~list-position '(1 2)))
	   (natac=new-left-numerator (line)
				     (labels ((natac=pos-of-new-left-numerator () (pos~list-position '(1 1 1))))
				       (let ((pos-of-new-left-numerator (natac=pos-of-new-left-numerator)))
					 (natac=compute-natacnum (data~struct-at-position (node~formula line)
											  pos-of-new-left-numerator)))))
	   (natac=new-right-numerator (line)
				      (labels ((natac=pos-of-new-right-numerator () (pos~list-position '(1 2 1))))
					(let ((pos-of-new-right-numerator (natac=pos-of-new-right-numerator)))
					  (natac=compute-natacnum (data~struct-at-position (node~formula line)
											   pos-of-new-right-numerator)))))
	   (natac=new-denominator (line)
				  (labels ((natac=pos-of-new-denominator () (pos~list-position '(1 1 2))))
				    (let ((pos-of-new-denominator (natac=pos-of-new-denominator)))
				      (natac=compute-natacnum (data~struct-at-position (node~formula line) pos-of-new-denominator))))))
    (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	   (left-arg-denominator (natac=canc-rat-natacnum-denominator left-arg))
	   (left-arg-denominator-as-value (natac=natacnum-value left-arg-denominator))
	   (right-arg (natac=nataccomp-right-arg nataccomp))
	   (right-arg-denominator (natac=canc-rat-natacnum-denominator right-arg))
	   (right-arg-denominator-as-value (natac=natacnum-value right-arg-denominator))
	   (lcm-of-denominators-as-value (natac=values-lcm left-arg-denominator-as-value right-arg-denominator-as-value))
	   (factor-for-left-fraction (natac=factor-for-expansion-from-denominator left-arg lcm-of-denominators-as-value))
	   (factor-for-right-fraction (natac=factor-for-expansion-from-denominator right-arg lcm-of-denominators-as-value))
	   (left-arg-expanded (unless (natac=int-natacnum-eql-one-p factor-for-left-fraction)
				(natac=rat-expand-fraction left-arg factor-for-left-fraction)))
	   (right-arg-expanded (unless (natac=int-natacnum-eql-one-p factor-for-right-fraction)
				 (natac=rat-expand-fraction right-arg factor-for-right-fraction)))
	   (pos-of-left-arg (natac=pos-of-left-arg))
	   (pos-of-right-arg (natac=pos-of-right-arg))
	   (help-prob (if right-arg-expanded
			  (cadr (tacl~apply natac*=subst
					    (list open-eq-line nil right-arg-expanded)
					    (list pos-of-right-arg)))
			open-eq-line))
	   (new-prob (if left-arg-expanded
			 (cadr (tacl~apply natac*=subst
					   (list help-prob nil left-arg-expanded)
					   (list pos-of-left-arg)))
		       help-prob))
	   (new-left-numerator (natac=new-left-numerator new-prob))
	   (new-left-numerator-as-setnum (natac=natacnum-setnum new-left-numerator))
	   (new-right-numerator (natac=new-right-numerator new-prob))
	   (new-right-numerator-as-setnum (natac=natacnum-setnum new-right-numerator))
	   (new-denominator (natac=new-denominator new-prob))
	   (new-denominator-as-setnum (natac=natacnum-setnum new-denominator))
	   (impe (tacl~sequence
		  (foralle-1 (natac*foralle-sort (list nil
						       (natac=get-rat-theorem natac*plus-rat-expanded-fracs)
						       (natac=get-num-int new-left-numerator))
						 (list new-left-numerator-as-setnum)))
		  (foralle-2 (natac*foralle-sort (list nil
						       (car foralle-1)
						       (natac=get-num-int new-right-numerator))
						 (list new-right-numerator-as-setnum)))
		  (foralle-3 (natac*foralle-sort (list nil
						       (car foralle-2)
						       (natac=get-num-nat new-denominator))
						 (list new-denominator-as-setnum)))
		  (impe (natac*impe (list nil nil (car foralle-3)) nil))))
	   (prem (cadr impe))
	   (conc (car impe))
	   (pos-for-subst (natac=pos-for-subst))
	   (start-pos (natac=start-pos))
	   (pos-of-plus (natac=pos-of-term-with-function-from-line natac*plus conc start-pos)))
      (natac=apply-arith-simplify prem)
      (cadr (tacl~apply natac*=subst
			(list new-prob
			      nil
			      (natac=apply-simplify-num conc nil pos-of-plus nil))
			(list pos-for-subst))))))

(defun natac=simpl-times (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (times x y) z), where x and y are rationals as setnums")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let ((theory-name (natac=nataccomp-theory-name nataccomp)))
    (cond ((natac=theory-names-eql theory-name natac*natural)
	   (natac=simpl-times-nat nataccomp open-eq-line))
	  ((natac=theory-names-eql theory-name natac*integer)
	   (natac=simpl-times-int nataccomp open-eq-line))
	  ((natac=theory-names-eql theory-name natac*rational)
	   (natac=simpl-times-rat nataccomp open-eq-line)))))

(defun natac=simpl-times-nat (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (times x y) z), where x and y are natural numbers as setnums")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (natac=base-step nataccomp
		   (natac=recursion nataccomp
				    open-eq-line)))

(defun natac=simpl-times-int (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (times x y) z), where x and y are integers as setnums"
		    "and at least one of them is not natural.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let ((base-num (natac=nataccomp-base-num nataccomp)))
    (natac=change-sign (natac=base-step nataccomp
					(natac=recursion nataccomp
							 open-eq-line))
		       base-num)))

(defun natac=simpl-times-rat (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (times x y) z), where x and y are rational numbers as setnums"
		    "and at least one of them is not whole-numbered.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=pos-of-comp () (pos~add-front 1))
	   (natac=pos-of-result () (pos~add-front 2)))
    (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	   (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	   (right-arg (natac=nataccomp-right-arg nataccomp))
	   (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	   (num-and-denom-simpl
	    (natac=rat-simpl-numerators-and-denominators
	     (car (tacl~sequence
		   (foralle-1 (natac*foralle-sort (list nil
							(natac=get-rat-theorem natac*times-rat)
							(natac=get-num-rat left-arg))
						  (list left-arg-as-setnum)))
		   (foralle-2 (natac*foralle-sort (list nil
							(car foralle-1)
							(natac=get-num-rat right-arg))
						  (list right-arg-as-setnum)))))))
	   (pos-of-comp (natac=pos-of-comp))
	   (pos-of-result (natac=pos-of-result))
	   (pos-list-of-times (natac=pos-list-of-terms-with-function-from-line natac*times num-and-denom-simpl pos-of-result)))
      (natac=rat-cancel-fraction
       (cadr (tacl~apply natac*=subst
			 (list open-eq-line
			       nil
			       (natac=apply-simplify-num num-and-denom-simpl nil pos-list-of-times nil))
			 (list pos-of-comp)))))))

(defun natac=simpl-power (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (power x y) z), where x is a rational number as setnum"
		    "and y is an integer as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let ((theory-name (natac=nataccomp-theory-name nataccomp))
	(base (natac=nataccomp-left-arg nataccomp))
	(exponent (natac=nataccomp-right-arg nataccomp)))
    (cond ((and (natac=natacnum-zerop base)
		(not (natac=natacnum-zerop exponent)))
	   (natac=simpl-power-base-equals-zero nataccomp open-eq-line))
	  ((natac=int-natacnum-eql-one-p base)
	   (natac=simpl-power-base-equals-one nataccomp open-eq-line))
	  ((natac=theory-names-eql theory-name natac*natural)
	   (natac=simpl-power-nat nataccomp open-eq-line))
	  ((natac=theory-names-eql theory-name natac*integer)
	   (natac=simpl-power-int nataccomp open-eq-line))
	  ((natac=theory-names-eql theory-name natac*rational)
	   (natac=simpl-power-rat nataccomp open-eq-line)))))

(defun natac=simpl-power-base-equals-zero (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (power x y) z), where x equals 0 (as setnum)"
		    "and y is a positive natural number as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let* ((exponent (natac=nataccomp-right-arg nataccomp))
	 (exponent-as-setnum (natac=natacnum-setnum exponent))
	 (impe
	  (tacl~sequence
	   (foralle (natac*foralle-sort (list nil
					      (natac=get-nat-theorem natac*power-nat-base-zero)
					      (natac=get-num-nat exponent))
					(list exponent-as-setnum)))
	   (impe-1 (natac*impe (list nil nil (car foralle)) nil))))
	 (prem (cadr impe))
	 (conc (car impe)))
    (natac=apply-arith-simplify prem)
    (car (tacl~apply natac*weaken (list open-eq-line conc) nil))))

(defun natac=simpl-power-base-equals-one (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (power x y) z), where x equals 1 (as setnum)"
		    "and y is an integer as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let* ((exponent (natac=nataccomp-right-arg nataccomp))
	 (exponent-as-setnum (natac=natacnum-setnum exponent))
	 (theorem-node (if (natac=natacnum-naturalp exponent)
			   (natac=get-nat-theorem natac*power-nat-base-one)
			 (natac=get-rat-theorem natac*power-rat-base-one)))
	 (sort (natac=extract-sort-from-theorem-node theorem-node)))
    (car (tacl~apply natac*foralle-sort
		     (list open-eq-line
			   theorem-node
			   (natac=get-num-sort exponent sort))
		     (list exponent-as-setnum)))))

(defun natac=simpl-power-nat (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (power x y) z), where x and y are natural numbers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (natac=base-step nataccomp
		   (natac=recursion nataccomp
				    open-eq-line)))

(defun natac=simpl-power-int (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (power x y) z), where x is an integer as setnum"
		    "and y is a natural number as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (natac=base-step nataccomp
		   (natac=recursion nataccomp
				    open-eq-line)))

(defun natac=simpl-power-rat (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (power x y) z), where x is a rational number as setnum"
		    "and y is an integer as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let ((exponent (natac=nataccomp-right-arg nataccomp)))
    (natac=rat-cancel-fraction
     (cond ((natac=natacnum-naturalp exponent)
	    (natac=simpl-power-nat-exp nataccomp open-eq-line))
	   ((natac=natacnum-integerp exponent)
	    (natac=simpl-power-nnat-exp nataccomp open-eq-line))))))

(defun natac=simpl-power-nat-exp (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (power x y) z), where x is a rational number as setnum"
		    "and y is a natural number as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=pos-of-comp () (pos~add-front 1))
	   (natac=pos-of-result () (pos~add-front 2)))
    (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	   (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	   (right-arg (natac=nataccomp-right-arg nataccomp))
	   (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	   (num-and-denom-simpl
	    (natac=rat-simpl-numerators-and-denominators
	     (car (tacl~sequence
		   (foralle-1 (natac*foralle-sort (list nil
							(natac=get-rat-theorem natac*power-rat-nat)
							(natac=get-num-rat left-arg))
						  (list left-arg-as-setnum)))
		   (foralle-2 (natac*foralle-sort (list nil
							(car foralle-1)
							(natac=get-num-nat right-arg))
						  (list right-arg-as-setnum)))))))
	   (pos-of-comp (natac=pos-of-comp))
	   (pos-of-result (natac=pos-of-result))
	   (pos-list-of-power (natac=pos-list-of-terms-with-function-from-line natac*power num-and-denom-simpl pos-of-result)))
      (cadr (tacl~apply natac*=subst
			(list open-eq-line
			      nil
			      (natac=apply-simplify-num num-and-denom-simpl nil pos-list-of-power nil))
			(list pos-of-comp))))))

(defun natac=simpl-power-nnat-exp (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (power x y) z), where x is a rational number as setnum"
		    "and y is a nopositive integer as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=pos-for-subst () (pos~add-front 1))
	   (natac=pos-of-result () (pos~add-front 2)))
    (let* ((base (natac=nataccomp-left-arg nataccomp))
	   (base-as-setnum (natac=natacnum-setnum base))
	   (exponent (natac=nataccomp-right-arg nataccomp))
	   (exponent-as-setnum (natac=natacnum-setnum exponent))
	   (foralle-2 (tacl~sequence
		       (foralle-1 (natac*foralle-sort (list nil
							    (natac=get-rat-theorem natac*power-rat-nnat)
							    (natac=get-num-rat base))
						      (list base-as-setnum)))
		       (foralle-2 (natac*foralle-sort (list nil
							    (car foralle-1)
							    (natac=get-num-nnat exponent))
						      (list exponent-as-setnum)))))
	   (line (natac=change-sign (car foralle-2) exponent))
	   (pos-of-result (natac=pos-of-result))
	   (pos-of-power (natac=pos-of-term-with-function-from-line natac*power line pos-of-result))
	   (pos-for-subst (natac=pos-for-subst)))
      (natac=one-over (cadr (tacl~apply natac*=subst
					(list open-eq-line
					      nil
					      (natac=apply-simplify-num line nil pos-of-power nil))
					(list pos-for-subst)))))))

(defun natac=one-over (line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A line.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "Simplifies all occurences of (one-over x) in {\\tt line}"
		    "where x is a rational number as setnum."
		    "The resulting line is returned."))
  (let ((one-over-pos-list (natac=pos-list-of-terms-with-function-from-line natac*one-over line)))
    (natac=one-over-cont one-over-pos-list line)))

(defun natac=one-over-cont (position-list line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A list of positions refering to expressions (one-over x) in {\\tt line}"
		    "where x is a rational number as setnum."
		    "A line.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "Simplifies all occurences of (one-over x) in {\\tt line}"
		    "referred to by an element of {\\tt position-list}."
		    "The resulting line is returned."))
  (if (null position-list)
      line
    (natac=one-over-cont (rest position-list)
			 (natac=simpl-single-one-over (car position-list)
						      line))))

(defun natac=simpl-single-one-over (position line)
  (declare (edited  "21-JUL-2000")
	   (authors Fisch)
	   (input   "A position refering to an expression (one-over x) in {\\tt line}"
		    "where x is a rational number as setnum."
		    "A line.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "Simplifies the (one-over x) in {\\tt line}"
		    "referred to by {\\tt position}."
		    "The resulting line is returned."))
  (let* ((line-1 (natac=rat-simpl-numerators-and-denominators (natac=exp-defn-of-one-over line)))
	 (line-2 (natac=rat-cancel-fraction line-1 position)))
    (if (eq line-1 line-2)
	line-1
      (cadr (tacl~apply natac*=subst
			(list line-1
			      nil
			      line-2)
			(list position))))))

(defun natac=exp-defn-of-one-over (line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A line.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "Expands the defintion of 'minus' and returns the resulting line."))
  (let* ((one-over-def (th~find-assumption "one-over" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant one-over-def))
	 (definiens (data~copy (th~ass-node one-over-def) :downto '(term+constant type+primitive)))
	 (pos-of-one-over (pos~add-end 0 (natac=pos-of-term-with-function-from-line natac*one-over line))))
    (cadr (tacl~apply natac*defni
		      (list line
			    nil)
		      (list definiendum
			    definiens
			    pos-of-one-over)))))
  
(defun natac=simpl-minus (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (minus x y) z), where x and y are rationals as setnums")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let ((right-arg (natac=nataccomp-right-arg nataccomp)))
    (natac=change-sign (natac=exp-defn-of-minus open-eq-line)
		       right-arg)))
  
(defun natac=exp-defn-of-minus (line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "An open line having the form:"
		    " (= (minus x y) z), where x and y are rationals as setnums")
	   (effect  "Further proof lines can be inserted.")
	   (value   "Expands the defintion of 'minus' and returns the resulting line."))
  (let* ((minus-def (th~find-assumption "minus" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant minus-def))
	 (definiens (data~copy (th~ass-node minus-def) :downto '(term+constant type+primitive)))
	 (pos-of-minus (pos~add-end 0 (natac=pos-of-term-with-function-from-line natac*minus line))))
    (cadr (tacl~apply natac*defni
		      (list line
			    nil)
		      (list definiendum
			    definiens
			    pos-of-minus)))))

(defun natac=simpl-divide (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (divide x y) z), where x and y are rational numbers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let* ((line (natac=one-over (natac=exp-defn-of-divide open-eq-line)))
	 (pos-of-times (natac=pos-of-term-with-function-from-line natac*times line)))
    (natac=apply-expand-num line nil pos-of-times nil)))

(defun natac=exp-defn-of-divide (line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A line.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "Expands the defintion of 'divide' and returns the resulting line."))
  (let* ((divide-def (th~find-assumption "divide" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant divide-def))
	 (definiens (data~copy (th~ass-node divide-def) :downto '(term+constant type+primitive)))
	 (pos-of-divide (pos~add-end 0 (natac=pos-of-term-with-function-from-line natac*divide line))))
    (cadr (tacl~apply natac*defni
		      (list line
			    nil)
		      (list definiendum
			    definiens
			    pos-of-divide)))))

(defun natac=simpl-div (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (div x y) z), where x and y are integers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=pos-of-result () (pos~add-front 2))
	   (natac=result () (let ((pos-of-result (natac=pos-of-result)))
			      (natac=compute-natacnum (data~struct-at-position (node~formula open-eq-line)
									       pos-of-result)))))
    (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	   (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	   (right-arg (natac=nataccomp-right-arg nataccomp))
	   (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	   (result (natac=result))
	   (result-as-setnum (natac=natacnum-setnum result))
	   (theorem-node (natac=get-int-theorem natac*div-int))
	   (sort-1 (natac=extract-i-th-sort-from-theorem-node theorem-node 1))
	   (sort-2 (natac=extract-i-th-sort-from-theorem-node theorem-node 2))
	   (sort-3 (natac=extract-i-th-sort-from-theorem-node theorem-node 3))
	   (impe-1
	    (tacl~sequence
	     (foralle-1 (natac*foralle-sort (list nil
						  theorem-node
						  (natac=get-num-sort left-arg sort-1))
					    (list left-arg-as-setnum)))
	     (foralle-2 (natac*foralle-sort (list nil
						  (car foralle-1)
						  (natac=get-num-sort right-arg sort-2))
					    (list right-arg-as-setnum)))
	     (foralle-3 (natac*foralle-sort (list nil
						  (car foralle-2)
						  (natac=get-num-sort result sort-3))
					    (list result-as-setnum)))
	     (impe-1 (natac*impe (list nil nil (car foralle-3)) nil))))
	   (prem-1 (cadr impe-1))
	   (conc-1 (car impe-1))
	   (impe-2 (tacl~apply natac*impe (list open-eq-line nil conc-1) nil))
	   (prem-2 (cadr impe-2))
	   (conc-2 (car impe-2))
	   (andi (tacl~apply natac*andi (list prem-2 nil nil) nil))
	   (left-conj (cadr andi))
	   (right-conj (caddr andi)))
      (natac=apply-arith-simplify prem-1)
      (natac=apply-arith-simplify left-conj)
      (natac=apply-arith-simplify right-conj)
      conc-2)))

(defun natac=simpl-mod (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (mod x y) z), where x and y are integers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	 (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	 (right-arg (natac=nataccomp-right-arg nataccomp))
	 (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	 (theorem-node (natac=get-int-theorem natac*mod-int))
	 (outer-sort (natac=extract-outer-sort-from-theorem-node theorem-node))
	 (inner-sort (natac=extract-inner-sort-from-theorem-node theorem-node))
	 (impe
	  (tacl~sequence
	   (foralle-1 (natac*foralle-sort (list nil
						theorem-node
						(natac=get-num-sort left-arg outer-sort))
					  (list left-arg-as-setnum)))
	   (foralle-2 (natac*foralle-sort (list nil
						(car foralle-1)
						(natac=get-num-sort right-arg inner-sort))
					  (list right-arg-as-setnum)))
	   (impe (natac*impe (list nil nil (car foralle-2)) nil))))
	 (prem (cadr impe))
	 (conc (car impe))
	 (pos-of-minus (natac=pos-of-term-with-function-from-line natac*minus conc)))
    (natac=apply-arith-simplify prem)
    (natac=apply-simplify-num conc open-eq-line pos-of-minus nil)))

(defun natac=simpl-gcd (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd x y) z), where x and y are integers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (natac=simpl-gcd-or-lcm nataccomp open-eq-line))

(defun natac=simpl-lcm (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (lcm x y) z), where x and y are integers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (natac=simpl-gcd-or-lcm nataccomp open-eq-line))
  
(defun natac=simpl-gcd-or-lcm (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd x y) z) or (= (lcm x y) z), where x and y are integers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let ((operator (natac=nataccomp-operator nataccomp))
	(left-arg (natac=nataccomp-left-arg nataccomp))
	(right-arg (natac=nataccomp-right-arg nataccomp)))
    (cond ((natac=natacnum-minusp left-arg)
	   (natac=simpl-gcd-or-lcm-change-sign-left-arg nataccomp open-eq-line))
	  ((natac=natacnum-minusp right-arg)
	   (natac=simpl-gcd-or-lcm-change-sign-right-arg nataccomp open-eq-line))
	  ((natac=int-natacnum-zerop left-arg)
	   (natac=simpl-gcd-or-lcm-left-arg-eql-zero nataccomp open-eq-line))
	  ((natac=int-natacnum-zerop right-arg)
	   (natac=simpl-gcd-or-lcm-right-arg-eql-zero nataccomp open-eq-line))
	  ((natac=natacnums-eql left-arg right-arg)
	   (natac=simpl-gcd-or-lcm-equal-args nataccomp open-eq-line))
	  ((natac=functions-eql operator natac*gcd)
	   (if (natac=natacnums-eq left-arg (natac=nataccomp-abs-greater-num nataccomp))
	       (natac=simpl-gcd-left-arg-greater nataccomp open-eq-line)
	     (natac=simpl-gcd-right-arg-greater nataccomp open-eq-line)))
	  (t
	   (natac=simpl-lcm-by-gcd nataccomp open-eq-line)))))

(defun natac=simpl-gcd-or-lcm-change-sign-left-arg (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd x y) z) or (= (lcm x y) z), where x is a negative integer as setnum and"
		    " y is an integer as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=update-nataccomp
	    ()
	    (let* ((operator (natac=nataccomp-operator nataccomp))
		   (left-arg (natac=nataccomp-left-arg nataccomp))
		   (right-arg (natac=nataccomp-right-arg nataccomp))
		   (change-sign-left-arg (natac=natacnum-change-sign left-arg)))
	      (cons operator (cons (list change-sign-left-arg right-arg) (rest (rest nataccomp)))))))
    (let* ((operator (natac=nataccomp-operator nataccomp))
	   (left-arg (natac=nataccomp-left-arg nataccomp))
	   (theorem-node (if (natac=functions-eql operator natac*gcd)
			     (natac=get-int-theorem natac*gcd-neg-left-arg)
			   (natac=get-int-theorem natac*lcm-neg-left-arg)))
	   (updated-nataccomp (natac=update-nataccomp)))
      (natac=simpl-gcd-or-lcm updated-nataccomp
			      (natac=simpl-gcd-or-lcm-change-sign-arg nataccomp open-eq-line theorem-node left-arg)))))
  
(defun natac=simpl-gcd-or-lcm-change-sign-right-arg (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd x y) z) or (= (lcm x y) z), where x is an integer as setnum and"
		    " y is a negative integer as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=update-nataccomp
	    ()
	    (let* ((operator (natac=nataccomp-operator nataccomp))
		   (left-arg (natac=nataccomp-left-arg nataccomp))
		   (right-arg (natac=nataccomp-right-arg nataccomp))
		   (change-sign-right-arg (natac=natacnum-change-sign right-arg)))
	      (cons operator (cons (list left-arg change-sign-right-arg) (rest (rest nataccomp)))))))
    (let* ((operator (natac=nataccomp-operator nataccomp))
	   (right-arg (natac=nataccomp-right-arg nataccomp))
	   (theorem-node (if (natac=functions-eql operator natac*gcd)
			     (natac=get-int-theorem natac*gcd-neg-right-arg)
			   (natac=get-int-theorem natac*lcm-neg-right-arg)))
	   (updated-nataccomp (natac=update-nataccomp)))
      (natac=simpl-gcd-or-lcm updated-nataccomp
			      (natac=simpl-gcd-or-lcm-change-sign-arg nataccomp open-eq-line theorem-node right-arg)))))
  
(defun natac=simpl-gcd-or-lcm-change-sign-arg (nataccomp open-eq-line theorem-node neg-arg)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd x y) z) or (= (lcm x y) z), where x and y are integers as setnums and"
		    " x or y is negative."
		    "The name of the theorem which serves to replace negative args:"
		    " {\\tt natac*gcd-neg-left-arg} or {\\tt natac*gcd-neg-right-arg} or"
		    " {\\tt natac*lcm-neg-left-arg} or {\\tt natac*lcm-neg-right-arg}."
		    "x, if x is negative; y, otherwise.")
	   (effect  "Replaces the negative argument by taking its additive inverse and generates"
		    "a new open line. (If both arguments are negative the left one will be replaced.")
	   (value   "A new open line which is identical to the given one, except the negative argument"
		    "which is replaced by its additive inverse. (If both arguments are negative"
		    "the left one is replaced."))
  (labels ((natac=pos-for-subst () (pos~add-front 1)))
    (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	   (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	   (right-arg (natac=nataccomp-right-arg nataccomp))
	   (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	   (outer-sort (natac=extract-outer-sort-from-theorem-node theorem-node))
	   (inner-sort (natac=extract-inner-sort-from-theorem-node theorem-node))
	   (pos-for-subst (natac=pos-for-subst)))
      (natac=change-sign (cadr (tacl~sequence
				(foralle-1 (natac*foralle-sort (list nil
								     theorem-node
								     (natac=get-num-sort left-arg outer-sort))
							       (list left-arg-as-setnum)))
				(foralle-2 (natac*foralle-sort (list nil
								     (car foralle-1)
								     (natac=get-num-sort right-arg inner-sort))
							       (list right-arg-as-setnum)))
				(=subst (natac*=subst (list open-eq-line
							    nil
							    (car foralle-2))
						      (list pos-for-subst)))))
			 neg-arg))))

(defun natac=simpl-gcd-or-lcm-left-arg-eql-zero (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd zero x) z) or (= (lcm zero x) z), where x is a positive integer as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let* ((operator (natac=nataccomp-operator nataccomp))
	 (right-arg (natac=nataccomp-right-arg nataccomp))
	 (theorem-node (if (natac=functions-eql operator natac*gcd)
			   (natac=get-int-theorem natac*gcd-left-arg-zero)
			 (natac=get-int-theorem natac*lcm-left-arg-zero))))
    (natac=simpl-gcd-or-lcm-arg-eql-zero open-eq-line theorem-node right-arg)))

(defun natac=simpl-gcd-or-lcm-right-arg-eql-zero (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd x zero) z) or (= (lcm x zero) z), where x is a positive integer as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let* ((operator (natac=nataccomp-operator nataccomp))
	 (left-arg (natac=nataccomp-left-arg nataccomp))
	 (theorem-node (if (natac=functions-eql operator natac*gcd)
			   (natac=get-int-theorem natac*gcd-right-arg-zero)
			 (natac=get-int-theorem natac*lcm-right-arg-zero))))
    (natac=simpl-gcd-or-lcm-arg-eql-zero open-eq-line theorem-node left-arg)))

(defun natac=simpl-gcd-or-lcm-arg-eql-zero (open-eq-line theorem-node arg)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "An open line having the form:"
		    " (= (gcd x zero) z) or (= (gcd zero x) z) or (= (lcm x zero) z) or (= (lcm zero x) z),"
		    " where x is positive integer as setnum."
		    "The name of the theorem which can close this line:"
		    " {\\tt natac*gcd-right-arg-zero} or {\\tt natac*gcd-left-arg-zero} or"
		    " {\\tt natac*lcm-right-arg-zero} or {\\tt natac*lcm-left-arg-zero}."
		    "x.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let ((arg-as-setnum (natac=natacnum-setnum arg))
	(sort (natac=extract-sort-from-theorem-node theorem-node)))
    (car (tacl~apply natac*foralle-sort
		     (list open-eq-line
			   theorem-node
			   (natac=get-num-sort arg sort))
		     (list arg-as-setnum)))))

(defun natac=simpl-gcd-or-lcm-equal-args (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd x x) z) or (= (lcm x x) z), where x is a positive integer as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (let* ((operator (natac=nataccomp-operator nataccomp))
	 (arg (natac=nataccomp-left-arg nataccomp))
	 (arg-as-setnum (natac=natacnum-setnum arg))
	 (theorem-node (if (natac=functions-eql operator natac*gcd)
			   (natac=get-int-theorem natac*gcd-equal-args)
			 (natac=get-int-theorem natac*lcm-equal-args))))
    (car (tacl~apply natac*foralle-sort
		     (list open-eq-line
			   theorem-node
			   (natac=get-num-nat arg))
		     (list arg-as-setnum)))))

(defun natac=simpl-gcd-left-arg-greater (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd x y) z), where x and y are positive integers as setnums and x greater than y.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=update-nataccomp
	    ()
	    (labels ((natac=update-left-arg
		      ()
		      (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
			     (left-arg-as-value (natac=natacnum-value left-arg))
			     (right-arg (natac=nataccomp-right-arg nataccomp))
			     (right-arg-as-value (natac=natacnum-value right-arg)))
			(natac=compute-natacnum (natac=values-minus left-arg-as-value right-arg-as-value)))))
	      (let ((operator (natac=nataccomp-operator nataccomp))
		    (right-arg (natac=nataccomp-right-arg nataccomp))
		    (updated-left-arg (natac=update-left-arg)))
		(cons operator (cons (list updated-left-arg right-arg) (rest (rest nataccomp))))))))
    (let ((theorem-node (natac=get-int-theorem natac*gcd-diff-1))
	  (updated-nataccomp (natac=update-nataccomp)))
      (natac=simpl-gcd updated-nataccomp
		       (natac=simpl-gcd-diff nataccomp open-eq-line theorem-node)))))
  
(defun natac=simpl-gcd-right-arg-greater (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd x y) z), where x and y are positive integers as setnums and x smaller than y.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=update-nataccomp
	    ()
	    (labels ((natac=update-right-arg
		      ()
		      (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
			     (left-arg-as-value (natac=natacnum-value left-arg))
			     (right-arg (natac=nataccomp-right-arg nataccomp))
			     (right-arg-as-value (natac=natacnum-value right-arg)))
			(natac=compute-natacnum (natac=values-minus right-arg-as-value left-arg-as-value)))))
	      (let ((operator (natac=nataccomp-operator nataccomp))
		    (left-arg (natac=nataccomp-left-arg nataccomp))
		    (updated-right-arg (natac=update-right-arg)))
		(cons operator (cons (list left-arg updated-right-arg) (rest (rest nataccomp))))))))
    (let ((theorem-node (natac=get-int-theorem natac*gcd-diff-2))
	  (updated-nataccomp (natac=update-nataccomp)))
      (natac=simpl-gcd updated-nataccomp
		       (natac=simpl-gcd-diff nataccomp open-eq-line theorem-node)))))

(defun natac=simpl-gcd-diff (nataccomp open-eq-line theorem-node)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (gcd x y) z), where x and y are positive integers as setnums, x not equal to y."
		    "The name of the theorem for application of one step of the Euclidian algorithm:"
		    "natac*gcd-diff-1 if x greater than y or"
		    "natac*gcd-diff-2 if x smaller than y.")
	   (effect  "Applies one step of the Euclidian algorithm.")
	   (value   "A new open line which has the form:"
		    " (= (gcd a y) z), where a = x - y."))
  (labels ((natac=pos-for-subst () (pos~add-front 1)))
    (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	   (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	   (right-arg (natac=nataccomp-right-arg nataccomp))
	   (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	   (outer-sort (natac=extract-outer-sort-from-theorem-node theorem-node))
	   (inner-sort (natac=extract-inner-sort-from-theorem-node theorem-node))
	   (pos-for-subst (natac=pos-for-subst))
	   (foralle-2 (tacl~sequence
		       (foralle-1 (natac*foralle-sort (list nil
							    theorem-node
							    (natac=get-num-sort left-arg outer-sort))
						      (list left-arg-as-setnum)))
		       (foralle-2 (natac*foralle-sort (list nil
							    (car foralle-1)
							    (natac=get-num-sort right-arg inner-sort))
						      (list right-arg-as-setnum)))))
	   (pos-of-minus (natac=pos-of-term-with-function-from-line natac*minus (car foralle-2))))
      (cadr (tacl~apply natac*=subst
			(list open-eq-line
			      nil
			      (natac=apply-simplify-num (car foralle-2) nil pos-of-minus nil))
			(list pos-for-subst))))))

(defun natac=simpl-lcm-by-gcd (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= (lcm x y) z), where x and y are positive integers as setnums and x neq y.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (labels ((natac=left-disjunct-as-term
	    (line)
	    (labels ((natac=pos-of-left-disjunct () (pos~add-front 1)))
	      (let ((pos-of-left-disjunct (natac=pos-of-left-disjunct)))
		(data~struct-at-position (node~formula line) pos-of-left-disjunct))))
	   (natac=right-disjunct-as-term
	    (line)
	    (labels ((natac=pos-of-right-disjunct () (pos~add-front 2)))
	      (let ((pos-of-right-disjunct (natac=pos-of-right-disjunct)))
		(data~struct-at-position (node~formula line) pos-of-right-disjunct)))))
    (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	   (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	   (right-arg (natac=nataccomp-right-arg nataccomp))
	   (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	   (impe
	    (tacl~sequence
	     (foralle-1 (natac*foralle-sort (list nil
						  (natac=get-int-theorem natac*lcm-by-gcd)
						  (natac=get-num-nat left-arg))
					    (list left-arg-as-setnum)))
	     (foralle-2 (natac*foralle-sort (list nil
						  (car foralle-1)
						  (natac=get-num-nat right-arg))
					    (list right-arg-as-setnum)))
	     (impe (natac*impe (list nil nil (car foralle-2)) nil))))
	   (prem (cadr impe))
	   (conc (car impe))
	   (pos-of-div (natac=pos-of-term-with-function-from-line natac*div conc)))
      (natac=apply-arith-simplify
       (if (natac=natacnum-zerop left-arg)
	   (let ((left-disjunct-as-term (natac=left-disjunct-as-term prem)))
	     (cadr (tacl~apply natac*orir
			       (list prem
				     nil)
			       (list left-disjunct-as-term))))
	 (let ((right-disjunct-as-term (natac=right-disjunct-as-term prem)))
	   (cadr (tacl~apply natac*oril
			     (list prem
				   nil)
			     (list right-disjunct-as-term))))))
      (natac=apply-simplify-num conc open-eq-line pos-of-div nil)
      open-eq-line)))

(defun natac=simpl-less (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (less x y), where x and y are rational numbers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (let ((theory-name (natac=nataccomp-theory-name nataccomp)))
    (cond ((natac=theory-names-eql theory-name natac*natural)
	   (natac=simpl-less-nat nataccomp open-line))
	  ((natac=theory-names-eql theory-name natac*integer)
	   (natac=simpl-less-int nataccomp open-line))
	  ((natac=theory-names-eql theory-name natac*rational)
	   (natac=simpl-less-rat nataccomp open-line)))))

(defun natac=simpl-less-nat (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (less x y), where x and y are natural numbers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (let ((left-arg (natac=nataccomp-left-arg nataccomp))
	(right-arg (natac=nataccomp-right-arg nataccomp)))
    (natac=simpl-less-nat-rec left-arg right-arg open-line)))

(defun natac=simpl-less-nat-rec (left right open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "An open line having the form:"
		    " (less x y), where x and y are natural numbers as setnums."
		    "x as natacnum."
		    "y as natacnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (if (natac=natacnum-zerop left)
      (let* ((new-right (natac=pos-int-natacnum-decrement right))
	     (new-right-as-setnum (natac=natacnum-setnum new-right)))
	(cadr (tacl~apply natac*foralle-sort
			  (list open-line
				(natac=get-nat-theorem natac*less-nat-base)
				(natac=get-num-nat new-right))
			  (list new-right-as-setnum))))
    (let* ((new-left (natac=pos-int-natacnum-decrement left))
	   (new-left-as-setnum (natac=natacnum-setnum new-left))
	   (new-right (natac=pos-int-natacnum-decrement right))
	   (new-right-as-setnum (natac=natacnum-setnum new-right))
	   (new-line
	    (cadr (tacl~sequence
		   (foralle-1 (natac*foralle-sort (list nil
							(natac=get-nat-theorem natac*less-nat-step)
							(natac=get-num-nat new-left))
						  (list new-left-as-setnum)))
		   (foralle-2 (natac*foralle-sort (list nil
							(car foralle-1)
							(natac=get-num-nat new-right))
						  (list new-right-as-setnum)))
		   (impe (natac*impe (list open-line nil (car foralle-2)) nil))))))
      (natac=simpl-less-nat-rec new-left new-right new-line))))

(defun natac=simpl-less-int (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (less x y), where x and y are integers as setnums and at least"
		    " one of them is negative.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (let ((left-arg (natac=nataccomp-left-arg nataccomp))
	(right-arg (natac=nataccomp-right-arg nataccomp)))
    (cond ((and (not (natac=natacnum-minusp left-arg))
		(not (natac=natacnum-minusp right-arg)))
	   (natac=simpl-less-nat nataccomp open-line))
	  ((and (not (natac=natacnum-plusp left-arg))
		(not (natac=natacnum-plusp right-arg)))
	   (natac=simpl-less-int-both-args-not-pos nataccomp open-line))
	  (t
	   (natac=simpl-less-int-pos-and-neg nataccomp open-line)))))

(defun natac=simpl-less-int-both-args-not-pos (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (less x y), where x and y are nonpositive integers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (let ((left-arg (natac=nataccomp-left-arg nataccomp))
	(right-arg (natac=nataccomp-right-arg nataccomp)))
    (natac=simpl-less-int-both-args-not-pos-rec left-arg right-arg open-line)))

(defun natac=simpl-less-int-both-args-not-pos-rec (left right open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "An open line having the form:"
		    " (less x y), where x and y are nonpositive integers as setnums."
		    "x as natacnum."
		    "y as natacnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (if (natac=natacnum-zerop right)
      (let* ((new-left (natac=neg-int-natacnum-increment left))
	     (new-left-as-setnum (natac=natacnum-setnum new-left)))
	(cadr (tacl~apply natac*foralle-sort
			  (list open-line
				(natac=get-int-theorem natac*less-nnat-base)
				(natac=get-num-nnat new-left))
			  (list new-left-as-setnum))))
    (let* ((new-left (natac=neg-int-natacnum-increment left))
	   (new-left-as-setnum (natac=natacnum-setnum new-left))
	   (new-right (natac=neg-int-natacnum-increment right))
	   (new-right-as-setnum (natac=natacnum-setnum new-right))
	   (new-line
	    (cadr (tacl~sequence
		   (foralle-1 (natac*foralle-sort (list nil
							(natac=get-int-theorem natac*less-nnat-step)
							(natac=get-num-nnat new-left))
						  (list new-left-as-setnum)))
		   (foralle-2 (natac*foralle-sort (list nil
							(car foralle-1)
							(natac=get-num-nnat new-right))
						  (list new-right-as-setnum)))
		   (impe (natac*impe (list open-line nil (car foralle-2)) nil))))))
      (natac=simpl-less-int-both-args-not-pos-rec new-left new-right new-line))))

(defun natac=simpl-less-int-pos-and-neg (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (less x y), where x is a negative integer as setnum and"
		    " y is a positive integer as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	 (left-arg-plus-one (natac=neg-int-natacnum-increment left-arg))
	 (left-arg-plus-one-as-setnum (natac=natacnum-setnum left-arg-plus-one))
	 (right-arg (natac=nataccomp-right-arg nataccomp))
	 (right-arg-minus-one (natac=pos-int-natacnum-decrement right-arg))
	 (right-arg-minus-one-as-setnum (natac=natacnum-setnum right-arg-minus-one)))
    (car (tacl~sequence
	  (foralle-1 (natac*foralle-sort (list nil
					       (natac=get-int-theorem natac*neg-less-pos-int)
					       (natac=get-num-nnat left-arg-plus-one))
					 (list left-arg-plus-one-as-setnum)))
	  (foralle-2 (natac*foralle-sort (list open-line
					       (car foralle-1)
					       (natac=get-num-nat right-arg-minus-one))
					 (list right-arg-minus-one-as-setnum)))))))

(defun natac=simpl-less-rat (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (less x y), where x and y are rationals as setnums and at least"
		    " one of them is not whole-numbered.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (let ((left-arg (natac=nataccomp-left-arg nataccomp))
	(right-arg (natac=nataccomp-right-arg nataccomp)))
    (if (and (natac=natacnum-minusp left-arg)
	     (natac=natacnum-plusp right-arg))
	(natac=simpl-less-rat-neg-and-pos nataccomp open-eq-line)
      (natac=simpl-less-rat-general nataccomp open-eq-line))))

(defun natac=simpl-less-rat-general (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (less x y), where x and y are non-integer rationals as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed.")
	   (remark  "If x is a negative non-integer rational and y is a positive non-integer rational"
		    "you should better use {\\tt natac=simpl-less-rat-neg-and-pos} because this will keep"
		    "the proof shorter."
		    "However, this function will do, too."))
  (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	 (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	 (right-arg (natac=nataccomp-right-arg nataccomp))
	 (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	 (line-1 (natac=rat-simpl-numerators-and-denominators
		  (car (tacl~sequence
			(foralle-1 (natac*foralle-sort (list nil
							     (natac=get-rat-theorem natac*less-rat)
							     (natac=get-num-rat left-arg))
						       (list left-arg-as-setnum)))
			(foralle-2 (natac*foralle-sort (list nil
							     (car foralle-1)
							     (natac=get-num-rat right-arg))
						       (list right-arg-as-setnum)))))))
	 (impe (tacl~apply natac*impe (list open-line nil line-1) nil))
	 (prem (cadr impe))
	 (conc (car impe)))
    (natac=apply-arith-simplify prem)
    conc))

(defun natac=simpl-less-rat-neg-and-pos (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (less x y), where x is a negative non-integer rational as setnum and"
		    " y is a positive non-integer rational as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	 (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	 (right-arg (natac=nataccomp-right-arg nataccomp))
	 (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	 (line-1 (natac=rat-simpl-numerators-and-denominators
		  (car (tacl~sequence
			(foralle-1 (natac*foralle-sort (list nil
							     (natac=get-rat-theorem natac*less-rat-neg-and-pos)
							     (natac=get-num-rat left-arg))
						       (list left-arg-as-setnum)))
			(foralle-2 (natac*foralle-sort (list nil
							     (car foralle-1)
							     (natac=get-num-rat right-arg))
						       (list right-arg-as-setnum)))))))
	 (impe (tacl~apply natac*impe (list open-eq-line nil line-1) nil))
	 (prem (cadr impe))
	 (conc (car impe))
	 (andi (tacl~apply natac*andi (list prem nil nil) nil))
	 (left-conj (cadr andi))
	 (right-conj (caddr andi)))
    (natac=apply-arith-simplify left-conj)
    (natac=apply-arith-simplify right-conj)))

(defun natac=simpl-leq (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (leq x y), where x and y are rational numbers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (let* ((left-arg (natac=nataccomp-left-arg nataccomp))
	 (right-arg (natac=nataccomp-right-arg nataccomp)))
    (if (natac=natacnums-eql left-arg right-arg)
	(natac=simpl-leq-by-= nataccomp open-line)
      (natac=simpl-leq-by-less nataccomp open-line))))

(defun natac=simpl-leq-by-= (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (leq x x), where x is a rational number as setnum.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (let* ((theory-name (natac=nataccomp-theory-name nataccomp))
	 (left-arg (natac=nataccomp-left-arg nataccomp))
	 (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	 (right-arg (natac=nataccomp-right-arg nataccomp))
	 (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	 (theorem-node (cond ((natac=theory-names-eql theory-name natac*natural)
			      (natac=get-nat-theorem natac*equal-implies-leq-nat))
			     ((natac=theory-names-eql theory-name natac*integer)
			      (natac=get-int-theorem natac*equal-implies-leq-int))
			     ((natac=theory-names-eql theory-name natac*rational)
			      (natac=get-rat-theorem natac*equal-implies-leq-rat))))
	 (outer-sort (natac=extract-outer-sort-from-theorem-node theorem-node))
	 (inner-sort (natac=extract-inner-sort-from-theorem-node theorem-node))
	 (impe (tacl~sequence
		(foralle-1 (natac*foralle-sort (list nil
						     theorem-node
						     (natac=get-num-sort left-arg outer-sort))
					       (list left-arg-as-setnum)))
		(foralle-2 (natac*foralle-sort (list nil
						     (car foralle-1)
						     (natac=get-num-sort right-arg inner-sort))
					       (list right-arg-as-setnum)))
		(impe (natac*impe (list open-line nil (car foralle-2)) nil))))
	 (prem (cadr impe))
	 (conc (car impe)))
    (natac=apply-=ref prem)
    conc))

(defun natac=simpl-leq-by-less (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (leq x y), where x and y are rational numbers as setnums and x smaller than y.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (let* ((theory-name (natac=nataccomp-theory-name nataccomp))
	 (left-arg (natac=nataccomp-left-arg nataccomp))
	 (left-arg-as-setnum (natac=natacnum-setnum left-arg))
	 (right-arg (natac=nataccomp-right-arg nataccomp))
	 (right-arg-as-setnum (natac=natacnum-setnum right-arg))
	 (theorem-node (cond ((natac=theory-names-eql theory-name natac*natural)
			      (natac=get-nat-theorem natac*less-implies-leq-nat))
			     ((natac=theory-names-eql theory-name natac*integer)
			      (natac=get-int-theorem natac*less-implies-leq-int))
			     ((natac=theory-names-eql theory-name natac*rational)
			      (natac=get-rat-theorem natac*less-implies-leq-rat))))
	 (outer-sort (natac=extract-outer-sort-from-theorem-node theorem-node))
	 (inner-sort (natac=extract-inner-sort-from-theorem-node theorem-node))
	 (impe (tacl~sequence
		(foralle-1 (natac*foralle-sort (list nil
						     theorem-node
						     (natac=get-num-sort left-arg outer-sort))
					       (list left-arg-as-setnum)))
		(foralle-2 (natac*foralle-sort (list nil
						     (car foralle-1)
						     (natac=get-num-sort right-arg inner-sort))
					       (list right-arg-as-setnum)))
		(impe (natac*impe (list open-line nil (car foralle-2)) nil))))
	 (prem (cadr impe))
	 (conc (car impe)))
    (natac=apply-arith-simplify prem)
    conc))

(defun natac=simpl-greater (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (greater x y), where x and y are rational numbers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (labels ((natac=pos-of-greater () (pos~add-front 0)))
    (let* ((greater-def (th~find-assumption "greater" (prob~theory omega*current-proof-plan)))
	   (definiendum (th~definition-constant greater-def))
	   (definiens (data~copy (th~ass-node greater-def) :downto '(term+constant type+primitive)))
	   (pos-of-greater (natac=pos-of-greater)))
      (natac=apply-arith-simplify
       (cadr (tacl~apply natac*defni
			 (list open-line
			       nil)
			 (list definiendum
			       definiens
			       pos-of-greater)))))))

(defun natac=simpl-geq (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (geq x y), where x and y are rational numbers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (labels ((natac=pos-of-geq () (pos~add-front 0)))
    (let* ((geq-def (th~find-assumption "geq" (prob~theory omega*current-proof-plan)))
	   (definiendum (th~definition-constant geq-def))
	   (definiens (data~copy (th~ass-node geq-def) :downto '(term+constant type+primitive)))
	   (pos-of-geq (natac=pos-of-geq)))
      (natac=apply-arith-simplify
       (cadr (tacl~apply natac*defni
			 (list open-line
			       nil)
			 (list definiendum
			       definiens
			       pos-of-geq)))))))

(defun natac=simpl-= (nataccomp open-eq-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (= x y), where x and y are rational numbers as setnums.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-eq-line}, now closed."))
  (natac=apply-=ref open-eq-line))
 
(defun natac=simpl-in (nataccomp open-line)
  (declare (edited  "10-JUL-2000")
	   (authors Fisch)
	   (input   "A nataccomp."
		    "An open line having the form:"
		    " (in x sort), where x is a number as setnum and an element of sort and"
		    "sort is either NAT, NNAT, INT or RAT.")
	   (effect  "Expands this subgoal.")
	   (value   "{\\tt open-line}, now closed."))
  (labels ((natac=pos-of-in () (pos~add-front 0)))
    (let* ((num (natac=nataccomp-left-arg nataccomp))
	   (sort (natac=nataccomp-right-arg nataccomp))
	   (in-def (th~find-assumption "in" (prob~theory omega*current-proof-plan)))
	   (definiendum (th~definition-constant in-def))
	   (definiens (data~copy (th~ass-node in-def) :downto '(term+constant type+primitive)))
	   (pos-of-in (natac=pos-of-in)))
      (tacl~sequence
       (defni (natac*defni (list open-line nil)
			   (list definiendum
				 definiens
				 pos-of-in)))
       (result (natac*weaken (list (cadr defni) (natac=get-num-sort num sort)) nil)))
      open-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to simplify and expand numerical expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun natac~simplify-number (formula pos &optional representation)
  (data~replace-at-position
   formula
   pos
   (natac~reduction (data~struct-at-position formula pos)
		    (pds~environment omega*current-proof-plan)
		    representation)))

(defun natac~simplify-number* (formula poslist &optional representation)
  (if poslist
      (natac~simplify-number* (natac~simplify-number formula (car poslist) representation) (cdr poslist) representation)
    formula))

(defun natac~expand-number (formula pos term)
  (let ((red-term1 (data~struct-at-position formula pos))
	(red-term2 (natac~reduction-cont term (pds~environment omega*current-proof-plan))))
    (if (natac~data-equal red-term1 red-term2)
	(data~replace-at-position formula pos term)
      formula)))
		    
(defun natac~expand-number* (formula poslist termlist)
  (if (and poslist termlist)
      (natac~expand-number* (natac~expand-number formula (car poslist) (car termlist))
			    (cdr poslist) (cdr termlist))
    formula))
		    
    
(defun natac~reduction (term env &optional representation)
  (declare (edited  "09-NOV-2000")
	   (authors Fisch)
	   (input   "A term."
		    "The environment."
		    "Optional: Either 'symnum or 'setnum.")
	   (effect  "No effects.")
	   (value   "Simplifies the numerical expression {\\tt term} as much as possible."
		    "If the optional parameter is given the resulting number is returned"
		    "in the desired representation."
		    "If no optional parameter is given the resulting number is returned"
		    " as setnum if all numbers occuring in {\\tt term} are setnums"
		    " as symnum otherwise."))
  (multiple-value-bind (result flag)
      (natac~reduction-cont term env)
    (cond ((eq representation 'symnum)
	   result)
	  ((eq representation 'setnum)
	   (natac=compute-setnum-from-symnum result))
	  (flag
	   (natac=compute-setnum-from-symnum result))
	  (t
	   result))))


(defun natac~reduction-cont (term env)
  (declare (edited  "19-NOV-1995 19:22")
	   (authors SORGE)
	   (input   "A numerical term and an environment")
	   (effect  "None")
	   (value   "Reduces the term as far as possible"))
  (cond ((natac=symnum-p term)
	 (values term nil))
	((natac=setnum-p term)
	 (values (natac=compute-symnum-from-setnum term) t))
        ((and (term~appl-p term) (natac=arith-function-p (data~appl-function term)))
	 (natac~compute (data~appl-function term) (data~appl-arguments term) env))
	((and (term~primitive-p term) (data~equal (term~type term) (env~lookup-object 'num env)))
	 (values term nil))
	(t
	 (omega~warn "~a isn't a proper number representation." term)
	 term)))


(defun natac~compute (function args env)
  (declare (edited  "19-NOV-1995 20:16")
	   (authors SORGE)
	   (input   )
	   (effect  )
	   (value   ))
  (cond ((string-equal (keim~name function) natac*plus)
	 (natac=2arg-function-apply #'+ args env function))
	((string-equal (keim~name function) natac*minus)
	 (natac=2arg-function-apply #'- args env function))
	((string-equal (keim~name function) natac*times)
	 (natac=2arg-function-apply #'* args env function))
	((string-equal (keim~name function) natac*divide)
	 (natac=2arg-function-apply #'/ args env function))
	((string-equal (keim~name function) natac*power)
	 (natac=2arg-function-apply #'expt args env function))
	((string-equal (keim~name function) natac*div)
	 (natac=2arg-function-apply #'floor args env function))
	((string-equal (keim~name function) natac*mod)
	 (natac=2arg-function-apply #'mod args env function))
	((string-equal (keim~name function) natac*gcd)
	 (natac=2arg-function-apply #'gcd args env function))
	((string-equal (keim~name function) natac*lcm)
	 (natac=2arg-function-apply #'lcm args env function))
	(t (warn ";;; ~A is yet an unknown numerical function" function)
	   (term~appl-create function args))))


(defun natac=2arg-function-apply (function args env &optional old-func)
  (multiple-value-bind (arg1 flag1)
      (natac~reduction-cont (car args) env)
    (multiple-value-bind (arg2 flag2)
	(natac~reduction-cont (cadr args) env)
      (if (and (natac=symnum-p arg1)
	       (natac=symnum-p arg2))
	  (values
	   (term~constant-create
	    (apply function (list (keim~name arg1) (keim~name arg2)))
	    (env~lookup-object 'num env))
	   (and flag1 flag2))
	(term~appl-create old-func (list arg1 arg2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Expansion of Numbers to Function in Set Theory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun natac~num2func-line (line)
  (declare (edited  "06-JUN-2001")
	   (authors Sorge)
	   (input   "A proof line.")
	   (effect  "Replaces the formula of the line with a formula where all numbers"
		    "are substituted with numerical functions.")
	   (value   "The modified line."))
  (let ((parameters (pdsj~parameters (node~justification line))))
    (setf (node~formula line) (natac=numbers-2-function (node~formula line)))
    (setf (pdsj~parameters (node~justification line))
	  (mapcar #'(lambda (par)
		      (if (term~p par)
			  (natac=numbers-2-function par)
			par))
		  parameters))
    line))
			     
(defun natac~num2func-proof ()
  (let ((hashtable (pds~label-node-hashtable omega*current-proof-plan)))
    (maphash #'(lambda (x y) (declare (ignore x))
		 (natac~num2func-line y))
	     hashtable)))
  

(defun natac=numbers-2-function (formula)
  (declare (edited  "24-MAY-1996 11:09")
	   (authors SORGE)
	   (input   "A formula.")
	   (value   "A formula where all occurences of SYM+NUM are replaced"
		    "by their corresponding settheoretical functions."))
  (let ((func-pos-list (natac=transform-number-list
			(natac=relate-position
			 (natac=get-all-numbers formula)
			 formula))))
    (labels ((natac=n2f-int1 (formula list)
			     (if list (natac=n2f-int2 (natac=n2f-int1 formula (cdr list))
						      (caar list)
						      (cdar list))
			       formula))
	     (natac=n2f-int2 (formula term list)
			     (if list (data~replace-at-position
				       (natac=n2f-int2 formula term (cdr list))
				       (car list)
				       term)
			       formula))
	     )
      (natac=n2f-int1 formula func-pos-list))))


(defun natac=numbers-2-function! (formula)
  (declare (edited  "24-MAY-1996 11:09")
	   (authors SORGE)
	   (input   "A formula.")
	   (effect  "All occurences of SYM+NUM are replaced by their"
		    "corresponding settheoretical functions.")
	   (value   "The manipulated formula."))
  (let ((func-pos-list (natac=transform-number-list
			(natac=relate-position
			 (natac=get-all-numbers formula)
			 formula))))
    (mapc #'(lambda (x)
	      (mapc #'(lambda (y)
			(data~replace-at-position formula y (car x) :destructive t))
		    (cdr x)))
	  func-pos-list))
  formula)
		
(defun natac=get-all-numbers (formula)
  (declare (edited  "23-MAY-1996 22:37")
	   (authors SORGE)
	   (input   "A formula")
	   (value   "A list of all numbers in that formula."))
  
  (labels ((natac=get-all-subterms (term)
				   (let ((term-list (data~substructs term)))
				     (cond ((null term-list) (list term))
					   ((= (length term-list) 1) (natac=get-all-subterms (car term-list)))
					   ((term~abstr-p (car term-list))
					    (append (natac=get-all-subterms (car term-list))
						    (apply #'append (mapcar #'natac=get-all-subterms
									    (cdr term-list)))))
					   (t (append (list (car term-list)) 
						      (apply #'append (mapcar #'natac=get-all-subterms
									      (cdr term-list)))
						      ))))))
    (remove-duplicates
     (remove-if-not #'term~number-p
		    (natac=get-all-subterms formula))
     :test #'keim~equal)))
  
  
(defun natac=relate-position (list formula)	
  (declare (edited  "26-FEB-1996 18:45")
	   (authors SORGE)
	   (input   "A list of terms.")
	   (value   "A list of list. Each list containing a term and its termpositions in formula."))
  (mapcar #'(lambda (x) (append (list x)
				(data~substruct-positions x formula)))
	  list))

(defun natac=transform-number (number)
  (let ((zero (env~lookup-object natac*zero (pds~environment omega*current-proof-plan)))
	(succ (env~lookup-object natac*succ (pds~environment omega*current-proof-plan)))
	(pred (env~lookup-object natac*pred (pds~environment omega*current-proof-plan)))
	(frac (env~lookup-object natac*frac (pds~environment omega*current-proof-plan))))
    (cond ((typep number '(integer 0))
	   (let ((result zero))
	     (dotimes (foo number)
	       (setf result (term~appl-create succ (list result))))
	     result))
	  ((integerp number)
	   (let ((result zero))
	     (dotimes (foo (abs number))
	       (setf result (term~appl-create pred (list result))))
	     result))
	  ((rationalp number)
	   (if frac
	       (term~appl-create frac (list (natac=transform-number (numerator number))
					    (natac=transform-number (denominator number))))
	     (omega~error "The number ~A is not a welldefined term in the current theory, prove your problem in RATIONAL." number)))
	  (t (warn ";;; Type of ~A is not yet implemented! " number)))))

(defun natac=transform-number-list (list)
  (declare (edited  "23-MAY-1996 22:58")
	   (authors SORGE)
	   (input   "A list of lists. Each list containing a number and termpositions.")
	   (value   "A list with the numbers transformed to the settheoretical form."))
  (mapcar #'(lambda (x)
	      (remove-if #'null
			 (append (list (natac=transform-number (keim~name (car x))))
				 (cdr x))))
	  list))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Translations of Functions in Set Theory into Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun natac~func2num-line (line)
  (declare (edited  "06-JUN-2001")
	   (authors Sorge)
	   (input   "A proof line.")
	   (effect  "Replaces the formula of the line with a formula where all numerical"
		    "functions are substituted with actual numbers.")
	   (value   "The modified line."))
  (let ((parameters (pdsj~parameters (node~justification line))))
    (setf (node~formula line) (natac=function-2-numbers (node~formula line)))
    (setf (pdsj~parameters (node~justification line))
	  (mapcar #'(lambda (par)
		      (if (term~p par)
			  (natac=function-2-numbers par)
			par))
		  parameters))
    line))

(defun natac~func2num-proof ()
  (let ((hashtable (pds~label-node-hashtable omega*current-proof-plan)))
    (maphash #'(lambda (x y) (declare (ignore x))
		 (natac~func2num-line y))
	     hashtable)))
  
(defun natac=function-2-numbers (formula)
  (declare (edited  "28-MAY-1996 22:27")
	   (authors SORGE)
	   (input   "A formula.")
	   (value   "The formula where all numbers in set theoretical"
		    "representation are transformed to numbers."))
  (let ((number-list (natac=transform-zero-list formula (natac=get-zero-list formula)))
	(num (env~lookup-object natac*number-type (pds~environment omega*current-proof-plan))))
    (labels ((natac=f2n-int (formula list)
			    (if list (data~replace-at-position (natac=f2n-int formula (cdr list))
							       (caar list)
							       (term~constant-create (cadar list) num))
			      formula))
	     )
      (natac=f2n-int formula number-list)) 
    ))

(defun natac=function-2-numbers! (formula)
  (declare (edited  "24-MAY-1996 11:09")
	   (authors SORGE)
	   (input   "A formula.")
	   (effect  "All occurences set theoretical functions are replaced"
		    "by their corresponding numbers.")
	   (value   "The manipulated formula."))
  (let ((number-list (natac=transform-zero-list formula (natac=get-zero-list formula)))
	(num (env~lookup-object natac*number-type (pds~environment omega*current-proof-plan))))
    (mapc #'(lambda (x)
	      (data~replace-at-position formula (car x) (term~constant-create (cadr x) num) :destructive t))
	  number-list))
  formula)
		
(defun natac=get-zero-list (term)
  (declare (edited  "28-MAY-1996 22:29")
	   (authors SORGE)
	   (input   "A term.")
	   (value   "A list with all positions of zero in term."))
  (let ((zero (env~lookup-object natac*zero (pds~environment omega*current-proof-plan))))
    (data~substruct-positions zero term)))

(defun natac=transform-zero-list (term list)
  (declare (edited  "28-MAY-1996 22:36")
	   (authors SORGE)
	   (input   "A term and a list of positions.")
	   (value   "A list with the numbers associated with positions."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (funclist (remove-if #'null
			      (mapcar #'(lambda (x) (env~lookup-object x env))
				      natac*set-function-list)))
	 (poslist 
	  (mapcar #'(lambda (x)
		      (do* ((i 1 (1+ i))
			    (subterm (data~struct-at-position term (pos~butlast x i))
				     (data~struct-at-position term (pos~butlast x i))))
			  ((or (not (term~appl-p subterm))
			       (not (find (data~appl-function subterm) funclist :test #'keim~equal))
			       (pos~empty-p (pos~butlast x (1- i))))
		     ;;;(list (pos~butlast x (1- i)) (natac=build-number (data~struct-at-position term (pos~butlast x (1- i))))))))
			   (list (pos~butlast x (1- i)) (data~struct-at-position term (pos~butlast x (1- i)))))))
		  list)))
    (mapcar #'(lambda (x)
		(list (car x) (natac=build-number (cadr x))))
	    (natac=clean-term-list poslist))))

(defun natac=clean-term-list (termlist)
  (declare (edited  "01-NOV-2000")
	   (authors Sorge)
	   (input   "A list of pairs of positions and terms.")
	   (effect  "None.")
	   (value   "The same list where the terms containing a fraction have been checked whether they can be"
		    "transformed into a number. If not they are removed."))
  (when termlist
    (let ((term (cadar termlist))
	  (position (caar termlist))
	  (frac (env~lookup-object natac*frac (pds~environment omega*current-proof-plan))))
      (if frac
	  (if (and (term~appl-p term) (data~equal (data~appl-function term) frac))
	      (let ((cor-term (find position (cdr termlist) :test #'keim~equal :key #'car)))
		(if cor-term
		    (cons (car termlist)
			  (natac=clean-term-list (remove cor-term (cdr termlist) :test #'equal)))
		  (cons (natac=check-frac-term (car termlist))
			(natac=clean-term-list (cdr termlist)))))
	    (cons (car termlist) (natac=clean-term-list (cdr termlist))))
	termlist))))

(defun natac=check-frac-term (pos-term-pair)
  (declare (edited  "01-NOV-2000")
	   (authors Sorge)
	   (input   "A pair containing a position and a term.")
	   (effect  "None.")
	   (value   "Checks wether the enumerator or the denominator of the fraction has to be"
		    "translated into a number. Returns the updated pair."))
  (let* ((position (car pos-term-pair))
	 (term (cadr pos-term-pair))
	 (args (data~appl-arguments term))
	 (enumerator (car args))
	 (denominator (cadr args))
	 (funclist (remove-if #'null
			      (mapcar #'(lambda (x) (env~lookup-object x (pds~environment omega*current-proof-plan)))
				      (cons natac*zero natac*set-function-list)))))
    (cond ((notevery #'(lambda (x) (find x funclist :test #'keim~equal))
		     (remove-if-not #'term~primitive-p (data~all-substructs enumerator)))
	   (list (pos~add-end 2 position) denominator))
	  ((notevery #'(lambda (x) (find x funclist :test #'keim~equal))
		     (remove-if-not #'term~primitive-p (data~all-substructs denominator)))
	   (list (pos~add-end 1 position) enumerator))
	  (t (omega~warn "Something strange has happened when checking a fraction.")
	     pos-term-pair))))

(defun natac=build-number (term)
  (declare (edited  "28-MAY-1996 23:08")
	   (authors SORGE)
	   (input   "A term.")
	   (value   "A number corresponding to that term."))
  (let ((zero (env~lookup-object natac*zero (pds~environment omega*current-proof-plan)))
	(succ (env~lookup-object natac*succ (pds~environment omega*current-proof-plan)))
	(pred (env~lookup-object natac*pred (pds~environment omega*current-proof-plan)))
	(frac (env~lookup-object natac*frac (pds~environment omega*current-proof-plan))))
    (labels ((natac=build-number-rec (term)
				     (cond ((term~number-p term) (keim~name term))
					   ((keim~equal term zero) 0)
					   ((keim~equal (data~appl-function term) succ) (1+ (natac=build-number-rec (car (data~appl-arguments term)))))
					   ((keim~equal (data~appl-function term) pred) (1- (natac=build-number-rec (car (data~appl-arguments term)))))
					   (t (let ((numerator (natac=build-number-rec (car (data~appl-arguments term))))
						    (denominator (natac=build-number-rec (cadr (data~appl-arguments term)))))
						(if (zerop denominator)
						    (omega~error "Attempt to divide by zero.")
						  (/ numerator denominator)))))))
      (natac=build-number-rec term))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for Arithmetic Simplify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun natac=get-corresponding-axiom (formula)
  (let ((function (data~appl-function formula))
	(env (pds~environment omega*current-proof-plan))
	(theory (prob~proof-theory omega*current-proof-plan)))
    (cond ((data~equal function (env~lookup-object natac*greater env))
	   (th~find-assumption 'greater theory))
	  ((data~equal function (env~lookup-object natac*geq env))
	   (th~find-assumption 'geq theory))
	  ((data~equal function (env~lookup-object natac*less env))
	   (th~find-assumption 'less theory))
	  ((data~equal function (env~lookup-object natac*leq env))
	   (th~find-assumption 'leq-nat theory))
	  ((data~schema-equal function (env~lookup-object natac*= env))
	   (th~find-assumption '= theory))
	  ((data~equal function (env~lookup-object natac*in env))
	   (th~find-assumption 'in theory))
	  )))

(defun natac=apply-arith-simplify (open-line &optional func2num)
  (declare (edited  "24-JUL-2000")
	   (authors Fisch)
	   (input   "An open line that can be closed by 'arith-simplify'."
		    "Optional: A flag indicating whether 'func2num' should be applied to the given line"
		    "          before simplification.")
	   (effect  "Further proof lines can be inserted.")
	   (value   "{\\tt open-line} closed by arith-simplify."))
  (when func2num (natac~func2num-line open-line))
  (let* ((axiom (natac=get-corresponding-axiom (node~formula open-line)))
	 (parameter (list (keim~name axiom)))
	 (outline (tacl~apply natac*arith-simplify (list open-line) parameter)))
    (natac=expand-as-start outline parameter)
    (car outline)))

(defun natac=all-num-type (term)
  (declare (edited  "18-OCT-2000")
	   (authors Sorge)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if all types involved in the term are number types."))
  (flet ((data-primitives (data)
			  (let ((pos-list (data~positions data #'data~primitive-p)))
			    (mapcar #'(lambda (x)
					(data~struct-at-position data x))
				    pos-list))))
    (let* ((struct-list (data-primitives term))
	   (type-list (mapcan #'(lambda (x) (data-primitives (term~type x))) struct-list))
	   (num (env~lookup-object natac*number-type (pds~environment omega*current-proof-plan))))
      (every #'(lambda (x) (keim~equal x num)) type-list))))

(defun natac=ensure-arith-correctness (formula)
  (let ((function (natac=get-arith-function formula)))
    (when function
      (if (equal function :in)
	  (let ((arg (natac=simplify-arith-argument (car (data~appl-arguments formula)))))
	    (when (or (term~number-p arg)
		      (natac=all-num-type arg))
	      t))
	(let ((arg1 (natac=simplify-arith-argument (car (data~appl-arguments formula))))
	      (arg2 (natac=simplify-arith-argument (cadr (data~appl-arguments formula)))))
	  (when (and (term~number-p arg1) (term~number-p arg2))
	    (apply function (list (keim~name arg1) (keim~name arg2)))))))))
      
(defun natac=compute-arith-simplify (formula)
  (let ((function (natac=get-arith-function formula))
	(arg1 (natac=simplify-arith-argument (car (data~appl-arguments formula))))
	(arg2 (natac=simplify-arith-argument (cadr (data~appl-arguments formula)))))
    (term~appl-create (data~appl-function formula) (list arg1 arg2))))

(defun natac=simplify-arith-argument (term)
  (natac~reduction-cont (beta~contract term) (pds~environment omega*current-proof-plan)))


(defun natac=get-arith-function (formula)
  (when (term~appl-p formula)
    (let ((function (data~appl-function formula))
	  (env (pds~environment omega*current-proof-plan)))
      (cond ((data~equal function (env~lookup-object natac*greater env)) #'>)
	    ((data~equal function (env~lookup-object natac*geq env)) #'>=)
	    ((data~equal function (env~lookup-object natac*less env)) #'<)
	    ((data~equal function (env~lookup-object natac*leq env)) #'<=)
	    ((data~schema-equal function (env~lookup-object natac*= env)) #'=)
	    ((data~schema-equal function (env~lookup-object natac*in env)) :in)
	    ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positions of numerical functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun natac=pos-length (pos)
  (declare (edited  "12-MAY-1996 20:41")
	   (authors SORGE)
	   (input   "A position.")
	   (value   "The length of the position."))
  (length (pos~number-list pos)))
  

(defun natac=sort-pos-list (pos-list)
  (declare (edited  "12-MAY-1996 20:01")
	   (authors SORGE)
	   (input   "A list of positions.")
	   (value   "A list of position lists. The list is ordered in a way"
		    "that the first position subsumes the latter ones."))
  (let* ((hl (sort pos-list #'(lambda (x y) (< (natac=pos-length x) (natac=pos-length y))))))
    (natac=sort-with-prefix hl)))


(defun natac=sort-with-prefix (list)
  (let* ((x (car list))
	 (sublist (append (list x)
			  (remove-if #'null
				     (mapcar #'(lambda (y)
					 	 (when (pos~prefix-p x y)
						   y))
					     (cdr list))))))
    (when list (append (list sublist) (natac=sort-with-prefix
				       (remove-if #'(lambda (x) (and (find x sublist)
								     (find x list)))
						  list))))))
					
(defun natac=function-position-list (formula)
  (declare (edited  "12-MAY-1996 15:59")
	   (authors SORGE)
	   (input   "A formula.")
	   (value   "A list with all positions of numerical functions."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (funclist (remove-if #'null
			      (mapcar #'(lambda (x) (env~lookup-object x env))
				      natac*function-list)))
	 (func-pos-list (apply #'append
			       (remove-if #'null
					  (mapcar #'(lambda (x) (data~substruct-positions x formula))
						  funclist)))))
    (mapcar #'pos~butlast func-pos-list)))
