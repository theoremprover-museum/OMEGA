;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: KEIM -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 2. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: omega@ags.uni-sb.de                                   ;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;    Commands to invoke MyCAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commands for interactive use of MyCAS
(com~defcommand create-poly
  (function cas=create-poly)
  (argnames symb str)
  (argtypes symbol string)
  (arghelps "A symbol" "A string for a polynomial")
  (frag-cats extern mycas)
  (help "A polynomial is read and bound to symbol"))

(com~defcommand add-poly
  (function cas=add-poly)
  (argnames poly1 poly2)
  (argtypes symbol symbol)
  (arghelps "First polynomial" "Second polynomial")
  (frag-cats extern mycas)
  (help "Add two polynomials."))

(com~defcommand sub-poly
  (function cas=sub-poly)
  (argnames poly1 poly2)
  (argtypes symbol symbol)
  (arghelps "First polynomial" "Second polynomial")
  (frag-cats extern mycas)
  (help "Subtract poly1 from poly2."))

(com~defcommand mult-poly
  (function cas=mult-poly)
  (argnames poly1 poly2 key)
  (argtypes symbol symbol symbol)
  (arghelps "First polynomial" "Second polynomial" "A keyword for a algorithm-scheme")
  (frag-cats extern mycas)
  (help "Multiply two polynomials."))

(com~defcommand scalar-mult
  (function cas=scalar-mult)
  (argnames poly scalar)
  (argtypes symbol number)
  (arghelps "Polynomial" "Scalar")
  (frag-cats extern mycas)
  (help "Multiply polynomial with scalar."))

(com~defcommand diff-poly
  (function cas=diff-poly)
  (argnames poly variable)
  (argtypes symbol number)
  (arghelps "Polynomial" "Variable-number")
  (frag-cats extern mycas)
  (help "Differentiate the polynomial in a specified variable."))

(com~defcommand integ-poly
  (function cas=integ-poly)
  (argnames poly variable constant)
  (argtypes symbol number number)
  (arghelps "Polynomial" "Variable number" "Integration constant")
  (frag-cats extern mycas)
  (help "Integrate the polynomial in a specified variable and add the integration constant."))

(com~defcommand roots
  (function cas=roots)
  (argnames poly)
  (argtypes symbol)
  (arghelps "Polynomial")
  (frag-cats extern mycas)
  (help "Computes the roots of a polynomial."))

(com~defcommand show-polys
  (function cas=show-polys)
  (argnames )
  (argtypes )
  (frag-cats extern mycas)
  (help "Shows a list of the existing polynomials"))

(com~defcommand show-poly
  (function cas=show-poly)
  (argnames poly)
  (argtypes symbol)
  (arghelps "Symbol")
  (frag-cats extern mycas)
  (help "Show a specified polynomial."))

(com~defcommand store-last-poly
  (function cas=store-last-poly)
  (argnames symbol)
  (argtypes symbol)
  (arghelps "Symbol")
  (frag-cats extern mycas)
  (help "Stores the last polynomial as symbol"))

(com~defcommand copy-poly
  (function cas=copy-poly)
  (argnames poly symbol)
  (argtypes symbol symbol)
  (arghelps "Polynomial" "Symbol")
  (frag-cats extern mycas)
  (help "Copys a poynomial"))

(com~defcommand delete-poly
  (function cas=delete-poly)
  (argnames poly)
  (argtypes symbol)
  (arghelps "Polynomial")
  (frag-cats extern mycas)
  (help "Deletes a polynomial from the stack"))

