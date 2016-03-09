;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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
(eval-when (load compile eval)
  (unless (com~find-category 'real)
    (com~defcategory real
		     (help "Tactics of the theory  real ."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun real=function-p (term function)
  (and (data~appl-p term)
       (lam~equal-p (data~appl-function term)
                    function)))





;; pop-plus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic pop-plus
		 (outline-mappings (((existent existent) pop-plus-a)
				    ((nonexistent existent) pop-plus-f)
				    ((existent nonexistent) pop-plus-b)))
		 (expansion-function polynom=expand-pop-plus)
		 (parameter-types position)
		 (help "(a+(b+c)) => (b+(a+c))"))

(com~defcommand pop-plus
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a+(b+c))"
            "A line containing (b+(a+c))"
            "The position of the subterm")
  (function polynom=pop-plus)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a+(b+c))=(b+(a+c))."))

(defun polynom=pop-plus (l1 l2 pos)
  (infer~compute-outline 'pop-plus (list l2 l1) (list pos)))



;;
;; pop-plus directions
;;

;;
;; F-direction
;;

(tac~deftactic pop-plus-f pop-plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=pop-plus-computation (formula L1) Pos)))
   (sideconditions (polynom=pop-plus-p (formula L1) Pos))
   (description "Forward application of pop-plus."))


;;
;; A-direction
;;


(tac~deftactic pop-plus-a pop-plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=pop-plus-ap (formula L1) (formula L2) Pos))
   (description "Test application of pop-plus."))

(defun polynom=pop-plus-ap (L1 L2 Pos)
  (and (polynom=pop-plus-p L1 Pos)  
       (lam~equal-p (polynom=pop-plus-computation L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic pop-plus-b pop-plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=pop-plus-computation (formula L2) Pos)))
   (sideconditions (polynom=pop-plus-p (formula L2) Pos))
   (description "Backward application of pop-plus."))


;;
;; Computations
;;

(defun polynom=pop-plus-p (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (plus (env~lookup-object 
                 :plus
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term plus)
          (real=function-p (second (data~appl-arguments term)) plus))))


(defun polynom=pop-plus-computation (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (sum  (second (data~appl-arguments term)))
         (a    (first (data~appl-arguments term)))
         (b    (first (data~appl-arguments sum)))
         (c    (second (data~appl-arguments sum)))
         (plus (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
                     plus
                     (list b
                           (term~appl-create
                              plus
                              (list a c))))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-pop-plus (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('a-plus-left (list nil precond) (list pos)))
     (s2 ('c-plus (list nil (car s1)) (list (pos~add-end 1 pos))))
     (s3 ('a-plus-right (list conc (car s2)) (list pos))))
    (tacl~end)))




;; c-plus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic c-plus
		 (outline-mappings (((existent existent) c-plus-a)
				    ((nonexistent existent) c-plus-f)
				    ((existent nonexistent) c-plus-b)))
		 (expansion-function polynom=expand-c-plus)
		 (parameter-types position)
		 (help "(a+b) => (b+a)"))

(com~defcommand c-plus
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a+b)"
            "A line containing (b+a)"
            "The position of the subterm")
  (function polynom=c-plus)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a+b)=(b+a)."))

(defun polynom=c-plus (l1 l2 pos)
  (infer~compute-outline 'c-plus (list l2 l1) (list pos)))



;;
;; c-plus directions
;;

;;
;; F-direction
;;

(tac~deftactic c-plus-f c-plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=c-plus-computation (formula L1) Pos)))
   (sideconditions (polynom=c-plus-p (formula L1) Pos))
   (description "Forward application of c-plus."))


;;
;; A-direction
;;


(tac~deftactic c-plus-a c-plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=c-plus-ap (formula L1) (formula L2) Pos))
   (description "Test application of c-plus."))

(defun polynom=c-plus-ap (L1 L2 Pos)
  (and (polynom=c-plus-p L1 Pos)  
       (lam~equal-p (polynom=c-plus-computation L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic c-plus-b c-plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=c-plus-computation (formula L2) Pos)))
   (sideconditions (polynom=c-plus-p (formula L2) Pos))
   (description "Backward application of c-plus."))


;;
;; Computations
;;

(defun polynom=c-plus-p (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (plus (env~lookup-object 
                 :plus
                 (pds~environment omega*current-proof-plan))))
     (real=function-p term plus)))


(defun polynom=c-plus-computation (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (a    (first (data~appl-arguments term)))
         (b    (second (data~appl-arguments term)))
         (plus (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
                     plus
                     (list b a))))
     (data~replace-at-position L1 Pos newterm)))


;;
;; Expansion
;;

(defun polynom=expand-c-plus (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'c-plus-real nil nil))
    (tacl~end)))





;; c-times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic c-times
		 (outline-mappings (((existent existent) c-times-a)
				    ((nonexistent existent) c-times-f)
				    ((existent nonexistent) c-times-b)))
		 (expansion-function polynom=expand-c-times)
		 (parameter-types position)
		 (help "(a*b) => (b*a)"))

(com~defcommand c-times
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a*b)"
            "A line containing (b*a)"
            "The position of the subterm")
  (function polynom=c-times)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a*b)=(b*a)."))

(defun polynom=c-times (l1 l2 pos)
  (infer~compute-outline 'c-times (list l2 l1) (list pos)))



;;
;; c-times directions
;;

;;
;; F-direction
;;

(tac~deftactic c-times-f c-times (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=c-times-computation (formula L1) Pos)))
   (sideconditions (polynom=c-times-p (formula L1) Pos))
   (description "Forward application of c-times."))


;;
;; A-direction
;;


(tac~deftactic c-times-a c-times (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=c-times-ap (formula L1) (formula L2) Pos))
   (description "Test application of c-times."))

(defun polynom=c-times-ap (L1 L2 Pos)
  (and (polynom=c-times-p L1 Pos)
       (polynom=c-times-p L1 Pos)
       (data~equal-p (data~struct-at-position L1 (pos~add-end 1 Pos))
                    (data~struct-at-position L2 (pos~add-end 2 Pos)))
       (data~equal-p (data~struct-at-position L2 (pos~add-end 1 Pos))
                    (data~struct-at-position L1 (pos~add-end 2 Pos)))))


;;
;; B-Direction
;;

(tac~deftactic c-times-b c-times (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=c-times-computation (formula L2) Pos)))
   (sideconditions (polynom=c-times-p (formula L2) Pos))
   (description "Backward application of c-times."))


;;
;; Computations
;;

(defun polynom=c-times-p (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (times (env~lookup-object 
                 :times
                 (pds~environment omega*current-proof-plan))))
     (real=function-p term times)))


(defun polynom=c-times-computation (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (a    (first (data~appl-arguments term)))
         (b    (second (data~appl-arguments term)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
                     times
                     (list b a))))
     (data~replace-at-position L1 Pos newterm)))


;;
;; Expansion
;;

(defun polynom=expand-c-times (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'c-times-real nil nil))
    (tacl~end)))





;; a-plus-right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic a-plus-right
		 (outline-mappings (((existent existent) a-plus-right-a)
				    ((nonexistent existent) a-plus-right-f)
				    ((existent nonexistent) a-plus-right-b)))
		 (expansion-function polynom=expand-a-plus-right)
		 (parameter-types position)
		 (help "((a+b)+c) => (a+(b+c))"))

(com~defcommand a-plus-right
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing ((a+b)+c)"
            "A line containing (a+(b+c))"
            "The position of the subterm")
  (function polynom=a-plus-right)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((a+b)+c)=(a+(b+c))."))

(defun polynom=a-plus-right (l1 l2 pos)
  (infer~compute-outline 'a-plus-right (list l2 l1) (list pos)))



;;
;; a-plus-right directions
;;

;;
;; F-direction
;;

(tac~deftactic a-plus-right-f a-plus-right (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=a-plus-right-computation-f (formula L1) Pos)))
   (sideconditions (polynom=a-plus-right-fp (formula L1) Pos))
   (description "Forward application of a-plus-right."))


;;
;; A-direction
;;


(tac~deftactic a-plus-right-a a-plus-right (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=a-plus-right-ap (formula L1) (formula L2) Pos))
   (description "Test application of a-plus-right."))

(defun polynom=a-plus-right-ap (L1 L2 Pos)
  (and (polynom=a-plus-right-fp L1 Pos)  
       (lam~equal-p (polynom=a-plus-right-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic a-plus-right-b a-plus-right (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=a-plus-right-computation-b (formula L2) Pos)))
   (sideconditions (polynom=a-plus-right-bp (formula L2) Pos))
   (description "Backward application of a-plus-right."))


;;
;; Computations
;;

(defun polynom=a-plus-right-fp (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (plus (env~lookup-object 
                 :plus
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term plus)
          (real=function-p (first (data~appl-arguments term)) plus))))

(defun polynom=a-plus-right-bp (L2 Pos)
  (let ((term (data~struct-at-position L2 Pos))
        (plus (env~lookup-object 
                 :plus
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term plus)
          (real=function-p (second (data~appl-arguments term)) plus))))



(defun polynom=a-plus-right-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (sum  (first (data~appl-arguments term)))
         (a    (first (data~appl-arguments sum)))
         (b    (second (data~appl-arguments sum)))
         (c    (second (data~appl-arguments term)))
         (plus (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
                     plus
                     (list a
                           (term~appl-create
                              plus
                              (list b c))))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=a-plus-right-computation-b (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (sum  (second (data~appl-arguments term)))
         (a    (first (data~appl-arguments term)))
         (b    (first (data~appl-arguments sum)))
         (c    (second (data~appl-arguments sum)))
         (plus (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
                     plus
                     (list (term~appl-create
                              plus
                              (list a b))
                           c))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-a-plus-right (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'a-plus-real nil nil))
    (tacl~end)))





;; a-plus-left
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic a-plus-left
		 (outline-mappings (((existent existent) a-plus-left-a)
				    ((nonexistent existent) a-plus-left-f)
				    ((existent nonexistent) a-plus-left-b)))
		 (expansion-function polynom=expand-a-plus-left)
		 (parameter-types position)
		 (help "(a+(b+c)) => ((a+b)+c)"))

(com~defcommand a-plus-left
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a+(b+c))"
            "A line containing ((a+b)+c)"
            "The position of the subterm")
  (function polynom=a-plus-left)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a+(b+c))=((a+b)+c)."))

(defun polynom=a-plus-left (l1 l2 pos)
  (infer~compute-outline 'a-plus-left (list l2 l1) (list pos)))



;;
;; a-plus-left directions
;;

;;
;; F-direction
;;

(tac~deftactic a-plus-left-f a-plus-left (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=a-plus-right-computation-b (formula L1) Pos)))
   (sideconditions (polynom=a-plus-right-bp (formula L1) Pos))
   (description "Forward application of a-plus-left."))


;;
;; A-direction
;;


(tac~deftactic a-plus-left-a a-plus-left (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=a-plus-left-ap (formula L1) (formula L2) Pos))
   (description "Test application of a-plus-left."))

(defun polynom=a-plus-left-ap (L1 L2 Pos)
  (and (polynom=a-plus-right-bp L1 Pos)  
       (lam~equal-p (polynom=a-plus-right-computation-b L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic a-plus-left-b a-plus-left (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=a-plus-right-computation-f (formula L2) Pos)))
   (sideconditions (polynom=a-plus-right-fp (formula L2) Pos))
   (description "Backward application of a-plus-left."))


;;
;; Expansion
;;

(defun polynom=expand-a-plus-left (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'a-plus-real nil nil))
    (tacl~end)))





;; a-times-right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic a-times-right
		 (outline-mappings (((existent existent) a-times-right-a)
				    ((nonexistent existent) a-times-right-f)
				    ((existent nonexistent) a-times-right-b)))
		 (expansion-function polynom=expand-a-times-right)
		 (parameter-types position)
		 (help "((a*b)*c) => (a*(b*c))"))

(com~defcommand a-times-right
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing ((a*b)*c)"
            "A line containing (a*(b*c))"
            "The position of the subterm")
  (function polynom=a-times-right)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((a*b)*c)=(a*(b*c))."))

(defun polynom=a-times-right (l1 l2 pos)
  (infer~compute-outline 'a-times-right (list l2 l1) (list pos)))



;;
;; a-times-right directions
;;

;;
;; F-direction
;;

(tac~deftactic a-times-right-f a-times-right (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=a-times-right-computation-f (formula L1) Pos)))
   (sideconditions (polynom=a-times-right-fp (formula L1) Pos))
   (description "Forward application of a-times-right."))


;;
;; A-direction
;;


(tac~deftactic a-times-right-a a-times-right (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=a-times-right-ap (formula L1) (formula L2) Pos))
   (description "Test application of a-times-right."))

(defun polynom=a-times-right-ap (L1 L2 Pos)
  (and (polynom=a-times-right-fp L1 Pos)  
       (lam~equal-p (polynom=a-times-right-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic a-times-right-b a-times-right (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=a-times-right-computation-b (formula L2) Pos)))
   (sideconditions (polynom=a-times-right-bp (formula L2) Pos))
   (description "Backward application of a-times-right."))


;;
;; Computations
;;

(defun polynom=a-times-right-fp (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (times (env~lookup-object 
                 :times
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term times)
          (real=function-p (first (data~appl-arguments term)) times))))

(defun polynom=a-times-right-bp (L2 Pos)
  (let ((term (data~struct-at-position L2 Pos))
        (times (env~lookup-object 
                 :times
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term times)
          (real=function-p (second (data~appl-arguments term)) times))))



(defun polynom=a-times-right-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (sum  (first (data~appl-arguments term)))
         (a    (first (data~appl-arguments sum)))
         (b    (second (data~appl-arguments sum)))
         (c    (second (data~appl-arguments term)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
                     times
                     (list a
                           (term~appl-create
                              times
                              (list b c))))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=a-times-right-computation-b (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (sum  (second (data~appl-arguments term)))
         (a    (first (data~appl-arguments term)))
         (b    (first (data~appl-arguments sum)))
         (c    (second (data~appl-arguments sum)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
                     times
                     (list (term~appl-create
                              times
                              (list a b))
                           c))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-a-times-right (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'a-times-real nil nil))
    (tacl~end)))






;; a-times-left
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic a-times-left
		 (outline-mappings (((existent existent) a-times-left-a)
				    ((nonexistent existent) a-times-left-f)
				    ((existent nonexistent) a-times-left-b)))
		 (expansion-function polynom=expand-a-times-left)
		 (parameter-types position)
		 (help "(a*(b*c)) => ((a*b)*c)"))

(com~defcommand a-times-left
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a*(b*c))"
            "A line containing ((a*b)*c)"
            "The position of the subterm")
  (function polynom=a-times-left)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a*(b*c))=((a*b)*c)."))

(defun polynom=a-times-left (l1 l2 pos)
  (infer~compute-outline 'a-times-left (list l2 l1) (list pos)))



;;
;; a-times-left directions
;;

;;
;; F-direction
;;

(tac~deftactic a-times-left-f a-times-left (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=a-times-right-computation-b (formula L1) Pos)))
   (sideconditions (polynom=a-times-right-bp (formula L1) Pos))
   (description "Forward application of a-times-left."))


;;
;; A-direction
;;


(tac~deftactic a-times-left-a a-times-left (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=a-times-left-ap (formula L1) (formula L2) Pos))
   (description "Test application of a-times-left."))

(defun polynom=a-times-left-ap (L1 L2 Pos)
  (and (polynom=a-times-right-bp L1 Pos)  
       (lam~equal-p (polynom=a-times-right-computation-b L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic a-times-left-b a-times-left (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=a-times-right-computation-f (formula L2) Pos)))
   (sideconditions (polynom=a-times-right-fp (formula L2) Pos))
   (description "Backward application of a-times-left."))


;;
;; Expansion
;;

(defun polynom=expand-a-times-left (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'a-times-real nil nil))
    (tacl~end)))






;; 0+e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic 0+e
		 (outline-mappings (((existent existent) 0+e-a)
				    ((nonexistent existent) 0+e-f)
				    ((existent nonexistent) 0+e-b)))
		 (expansion-function polynom=expand-0+e)
		 (parameter-types position)
		 (help "(0+a) => a"))

(com~defcommand 0+e
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (0+a)"
            "A line containing a"
            "The position of the subterm")
  (function polynom=0+e)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (0+a)=a."))

(defun polynom=0+e (l1 l2 pos)
  (infer~compute-outline '0+e (list l2 l1) (list pos)))



;;
;; 0+e directions
;;

;;
;; F-direction
;;

(tac~deftactic 0+e-f 0+e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=0+e-computation-f (formula L1) Pos)))
   (sideconditions (polynom=0+e-fp (formula L1) Pos))
   (description "Forward application of 0+e."))


;;
;; A-direction
;;


(tac~deftactic 0+e-a 0+e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=0+e-ap (formula L1) (formula L2) Pos))
   (description "Test application of 0+e."))

(defun polynom=0+e-ap (L1 L2 Pos)
  (and (polynom=0+e-fp L1 Pos)  
       (lam~equal-p (polynom=0+e-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic 0+e-b 0+e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=0+e-computation-b (formula L2) Pos)))
   (sideconditions (polynom=0+e-bp (formula L2) Pos))
   (description "Backward application of 0+e."))


;;
;; Computations
;;

(defun polynom=0+e-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (plus (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num)))
     (and (real=function-p term plus)
          (lam~equal-p zero
                       (first (data~appl-arguments term))))))

(defun polynom=0+e-bp (L2 Pos)
  t)

(defun polynom=0+e-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (newterm (second (data~appl-arguments term))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=0+e-computation-b (L1 Pos)
  (let* ((a    (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (plus (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num))
         (newterm (term~appl-create plus (list zero a))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-0+e (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos '0-plus-real nil nil))
    (tacl~end)))






;; +0e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic +0e
		 (outline-mappings (((existent existent) +0e-a)
				    ((nonexistent existent) +0e-f)
				    ((existent nonexistent) +0e-b)))
		 (expansion-function polynom=expand-+0e)
		 (parameter-types position)
		 (help "(a+0) => a"))

(com~defcommand +0e
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a+0)"
            "A line containing a"
            "The position of the subterm")
  (function polynom=+0e)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a+0)=a."))

(defun polynom=+0e (l1 l2 pos)
  (infer~compute-outline '+0e (list l2 l1) (list pos)))



;;
;; +0e directions
;;

;;
;; F-direction
;;

(tac~deftactic +0e-f +0e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=+0e-computation-f (formula L1) Pos)))
   (sideconditions (polynom=+0e-fp (formula L1) Pos))
   (description "Forward application of +0e."))


;;
;; A-direction
;;


(tac~deftactic +0e-a +0e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=+0e-ap (formula L1) (formula L2) Pos))
   (description "Test application of +0e."))

(defun polynom=+0e-ap (L1 L2 Pos)
  (and (polynom=+0e-fp L1 Pos)  
       (lam~equal-p (polynom=+0e-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic +0e-b +0e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=+0e-computation-b (formula L2) Pos)))
   (sideconditions (polynom=+0e-bp (formula L2) Pos))
   (description "Backward application of +0e."))


;;
;; Computations
;;

(defun polynom=+0e-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (plus (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num)))
     (and (real=function-p term plus)
          (lam~equal-p zero
                       (second (data~appl-arguments term))))))

(defun polynom=+0e-bp (L2 Pos)
  t)

(defun polynom=+0e-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (newterm (first (data~appl-arguments term))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=+0e-computation-b (L1 Pos)
  (let* ((a    (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (plus (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num))
         (newterm (term~appl-create plus (list a zero))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-+0e (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('c-plus (list nil precond) (list pos)))
     (s2 ('0+e (list conc (car s1)) (list pos))))
    (tacl~end)))




;; 1*e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic 1*e
		 (outline-mappings (((existent existent) 1*e-a)
				    ((nonexistent existent) 1*e-f)
				    ((existent nonexistent) 1*e-b)))
		 (expansion-function polynom=expand-1*e)
		 (parameter-types position)
		 (help "(1*a) => a"))

(com~defcommand 1*e
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (1*a)"
            "A line containing a"
            "The position of the subterm")
  (function polynom=1*e)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (1*a)=a."))

(defun polynom=1*e (l1 l2 pos)
  (infer~compute-outline '1*e (list l2 l1) (list pos)))



;;
;; 1*e directions
;;

;;
;; F-direction
;;

(tac~deftactic 1*e-f 1*e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=1*e-computation-f (formula L1) Pos)))
   (sideconditions (polynom=1*e-fp (formula L1) Pos))
   (description "Forward application of 1*e."))


;;
;; A-direction
;;


(tac~deftactic 1*e-a 1*e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=1*e-ap (formula L1) (formula L2) Pos))
   (description "Test application of 1*e."))

(defun polynom=1*e-ap (L1 L2 Pos)
  (and (polynom=1*e-fp L1 Pos)  
       (lam~equal-p (polynom=1*e-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic 1*e-b 1*e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=1*e-computation-b (formula L2) Pos)))
   (sideconditions (polynom=1*e-bp (formula L2) Pos))
   (description "Backward application of 1*e."))


;;
;; Computations
;;

(defun polynom=1*e-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (one (term~constant-create 1 num)))
     (and (real=function-p term times)
          (lam~equal-p one
                       (first (data~appl-arguments term))))))

(defun polynom=1*e-bp (L2 Pos)
  t)

(defun polynom=1*e-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (newterm (second (data~appl-arguments term))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=1*e-computation-b (L1 Pos)
  (let* ((a    (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (one (term~constant-create 1 num))
         (newterm (term~appl-create times (list one a))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-1*e (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos '1-times-real nil nil))
    (tacl~end)))






;; *1e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic *1e
		 (outline-mappings (((existent existent) *1e-a)
				    ((nonexistent existent) *1e-f)
				    ((existent nonexistent) *1e-b)))
		 (expansion-function polynom=expand-*1e)
		 (parameter-types position)
		 (help "(a*1) => a"))

(com~defcommand *1e
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a*1)"
            "A line containing a"
            "The position of the subterm")
  (function polynom=*1e)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a*1)=a."))

(defun polynom=*1e (l1 l2 pos)
  (infer~compute-outline '*1e (list l2 l1) (list pos)))



;;
;; *1e directions
;;

;;
;; F-direction
;;

(tac~deftactic *1e-f *1e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=*1e-computation-f (formula L1) Pos)))
   (sideconditions (polynom=*1e-fp (formula L1) Pos))
   (description "Forward application of *1e."))


;;
;; A-direction
;;


(tac~deftactic *1e-a *1e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=*1e-ap (formula L1) (formula L2) Pos))
   (description "Test application of *1e."))

(defun polynom=*1e-ap (L1 L2 Pos)
  (and (polynom=*1e-fp L1 Pos)  
       (lam~equal-p (polynom=*1e-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic *1e-b *1e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=*1e-computation-b (formula L2) Pos)))
   (sideconditions (polynom=*1e-bp (formula L2) Pos))
   (description "Backward application of *1e."))


;;
;; Computations
;;

(defun polynom=*1e-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (one (term~constant-create 1 num)))
     (and (real=function-p term times)
          (lam~equal-p one
                       (second (data~appl-arguments term))))))

(defun polynom=*1e-bp (L2 Pos)
  t)

(defun polynom=*1e-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (newterm (first (data~appl-arguments term))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=*1e-computation-b (L1 Pos)
  (let* ((a    (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (one (term~constant-create 1 num))
         (newterm (term~appl-create times (list a one))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-*1e (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('c-times (list nil precond) (list pos)))
     (s2 ('1*e (list conc (car s1)) (list pos))))
    (tacl~end)))







;; 1*i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic 1*i
		 (outline-mappings (((existent existent) 1*i-a)
				    ((nonexistent existent) 1*i-f)
				    ((existent nonexistent) 1*i-b)))
		 (expansion-function polynom=expand-1*i)
		 (parameter-types position)
		 (help "a => (1*a)"))

(com~defcommand 1*i
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a"
            "A line containing (1*a)"
            "The position of the subterm")
  (function polynom=1*i)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite a=(1*a)."))

(defun polynom=1*i (l1 l2 pos)
  (infer~compute-outline '1*i (list l2 l1) (list pos)))



;;
;; 1*i directions
;;

;;
;; F-direction
;;

(tac~deftactic 1*i-f 1*i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=1*e-computation-b (formula L1) Pos)))
   (sideconditions (polynom=1*e-bp (formula L1) Pos))
   (description "Forward application of 1*i."))


;;
;; A-direction
;;


(tac~deftactic 1*i-a 1*i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=1*i-ap (formula L1) (formula L2) Pos))
   (description "Test application of 1*i."))

(defun polynom=1*i-ap (L1 L2 Pos)
  (and (polynom=1*e-bp L1 Pos)  
       (lam~equal-p (polynom=1*e-computation-b L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic 1*i-b 1*i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=1*e-computation-f (formula L2) Pos)))
   (sideconditions (polynom=1*e-fp (formula L2) Pos))
   (description "Backward application of 1*i."))



;;
;; Expansion
;;

(defun polynom=expand-1*i (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos '1-times-real nil nil))
    (tacl~end)))







;; ^1e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic ^1e
		 (outline-mappings (((existent existent) ^1e-a)
				    ((nonexistent existent) ^1e-f)
				    ((existent nonexistent) ^1e-b)))
		 (expansion-function polynom=expand-^1e)
		 (parameter-types position)
		 (help "(a^1) => a"))

(com~defcommand ^1e
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a^1)"
            "A line containing a"
            "The position of the subterm")
  (function polynom=^1e)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a^1)=a."))

(defun polynom=^1e (l1 l2 pos)
  (infer~compute-outline '^1e (list l2 l1) (list pos)))



;;
;; ^1e directions
;;

;;
;; F-direction
;;

(tac~deftactic ^1e-f ^1e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=^1e-computation-f (formula L1) Pos)))
   (sideconditions (polynom=^1e-fp (formula L1) Pos))
   (description "Forward application of ^1e."))


;;
;; A-direction
;;


(tac~deftactic ^1e-a ^1e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=^1e-ap (formula L1) (formula L2) Pos))
   (description "Test application of ^1e."))

(defun polynom=^1e-ap (L1 L2 Pos)
  (and (polynom=^1e-fp L1 Pos)  
       (lam~equal-p (polynom=^1e-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic ^1e-b ^1e (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=^1e-computation-b (formula L2) Pos)))
   (sideconditions (polynom=^1e-bp (formula L2) Pos))
   (description "Backward application of ^1e."))


;;
;; Computations
;;

(defun polynom=^1e-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (one (term~constant-create 1 num)))
     (and (real=function-p term power)
          (lam~equal-p one
                       (second (data~appl-arguments term))))))

(defun polynom=^1e-bp (L2 Pos)
  t)

(defun polynom=^1e-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (newterm (first (data~appl-arguments term))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=^1e-computation-b (L1 Pos)
  (let* ((a    (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (one (term~constant-create 1 num))
         (newterm (term~appl-create power (list a one))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-^1e (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'power-1-real nil nil))
    (tacl~end)))




;; ^1i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic ^1i
		 (outline-mappings (((existent existent) ^1i-a)
				    ((nonexistent existent) ^1i-f)
				    ((existent nonexistent) ^1i-b)))
		 (expansion-function polynom=expand-^1i)
		 (parameter-types position)
		 (help "a => (a^1)"))

(com~defcommand ^1i
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a"
            "A line containing (a^1)"
            "The position of the subterm")
  (function polynom=^1i)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite a=(a^1)."))

(defun polynom=^1i (l1 l2 pos)
  (infer~compute-outline '^1i (list l2 l1) (list pos)))



;;
;; ^1i directions
;;

;;
;; F-direction
;;

(tac~deftactic ^1i-f ^1i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=^1e-computation-b (formula L1) Pos)))
   (sideconditions (polynom=^1e-bp (formula L1) Pos))
   (description "Forward application of ^1i."))


;;
;; A-direction
;;


(tac~deftactic ^1i-a ^1i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=^1i-ap (formula L1) (formula L2) Pos))
   (description "Test application of ^1i."))

(defun polynom=^1i-ap (L1 L2 Pos)
  (and (polynom=^1e-bp L1 Pos)  
       (lam~equal-p (polynom=^1e-computation-b L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic ^1i-b ^1i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=^1e-computation-f (formula L2) Pos)))
   (sideconditions (polynom=^1e-fp (formula L2) Pos))
   (description "Backward application of ^1i."))



;;
;; Expansion
;;

(defun polynom=expand-^1i (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'power-1-real nil nil))
    (tacl~end)))









;; mult-power
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic mult-power
		 (outline-mappings (((existent existent) mult-power-a)
				    ((nonexistent existent) mult-power-f)
				    ((existent nonexistent) mult-power-b)))
		 (expansion-function polynom=expand-mult-power)
		 (parameter-types position)
		 (help "((a^b)*(a^c)) => (a^(b+c))"))

(com~defcommand mult-power
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing ((a^b)*(a^c))"
            "A line containing (a^(b+c))"
            "The position of the subterm")
  (function polynom=mult-power)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((a^b)*(a^c))=(a^(b+c))."))

(defun polynom=mult-power (l1 l2 pos)
  (infer~compute-outline 'mult-power (list l2 l1) (list pos)))



;;
;; mult-power directions
;;

;;
;; F-direction
;;

(tac~deftactic mult-power-f mult-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mult-power-computation-f (formula L1) Pos)))
   (sideconditions (polynom=mult-power-fp (formula L1) Pos))
   (description "Forward application of mult-power."))


;;
;; A-direction
;;


(tac~deftactic mult-power-a mult-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=mult-power-ap (formula L1) (formula L2) Pos))
   (description "Test application of mult-power."))

(defun polynom=mult-power-ap (L1 L2 Pos)
  (and (polynom=mult-power-fp L1 Pos)  
       (lam~equal-p (polynom=mult-power-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic mult-power-b mult-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mult-power-computation-b (formula L2) Pos)))
   (sideconditions (polynom=mult-power-bp (formula L2) Pos))
   (description "Backward application of mult-power."))


;;
;; Computations
;;

(defun polynom=mult-power-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos)))
     (and (data~appl-p term)
	  (let* (
	 (args (data~appl-arguments term))
	 (ex1  (first args))
	 (ex2  (second args))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
	 )
     (and (real=function-p term times)
	  (real=function-p ex1 power)
	  (real=function-p ex2 power)
	  (lam~equal-p (first (data~appl-arguments ex1))
		       (first (data~appl-arguments ex2))))))))

(defun polynom=mult-power-bp (L2 Pos)
  (let ((term (data~struct-at-position L2 Pos))
        (plus (env~lookup-object 
                 :plus
                 (pds~environment omega*current-proof-plan)))
        (power (env~lookup-object 
                 :power
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term power)
	  (real=function-p
	    (second (data~appl-arguments term))
	    plus))))


(defun polynom=mult-power-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
	 (args (data~appl-arguments term))
	 (ex1  (first args))
	 (ex2  (second args))
	 (a    (first (data~appl-arguments ex1)))
	 (b    (second (data~appl-arguments ex1)))
	 (c    (second (data~appl-arguments ex2)))	       
         (plus (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
		   power
		   (list a
			 (term~appl-create plus
					   (list b c))))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=mult-power-computation-b (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
	 (args (data~appl-arguments term))
	 (sum  (data~appl-arguments  (second args)))
	 (a    (first args))
	 (b    (first sum))
	 (c    (second sum))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
		   times
		   (list 
	            (term~appl-create
		     power (list a b))
		    (term~appl-create
		     power (list a c))))))			 
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-mult-power (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'times-power-real nil nil))
    (tacl~end)))







;; 0*elim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic 0*elim
		 (outline-mappings (((existent existent) 0*elim-a)
				    ((nonexistent existent) 0*elim-f)
				    ((existent nonexistent) 0*elim-b)))
		 (expansion-function polynom=expand-0*elim)
		 (parameter-types position term)
		 (help "(0*a) => 0"))

(com~defcommand 0*elim
  (argnames line1 line2 position term)
  (argtypes ndline ndline position term)
  (arghelps "A line containing (0*a)"
            "A line containing 0"
            "The position of the subterm"
	    "An appropriate a")
  (function polynom=0*elim)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (0*a)=0."))

(defun polynom=0*elim (l1 l2 pos term)
  (infer~compute-outline '0*elim (list l2 l1) (list pos term)))



;;
;; 0*elim directions
;;

;;
;; F-direction
;;

(tac~deftactic 0*elim-f 0*elim (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=0*elim-computation-f (formula L1) Pos)))
   (sideconditions (polynom=0*elim-fp (formula L1) Pos))
   (description "Forward application of 0*elim."))


;;
;; A-direction
;;


(tac~deftactic 0*elim-a 0*elim (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=0*elim-ap (formula L1) (formula L2) Pos))
   (description "Test application of 0*elim."))

(defun polynom=0*elim-ap (L1 L2 Pos)
  (and (polynom=0*elim-fp L1 Pos)  
       (lam~equal-p (polynom=0*elim-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic 0*elim-b 0*elim (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=0*elim-computation-b (formula L2) Pos Term)))
   (sideconditions (polynom=0*elim-bp (formula L2) Pos))
   (description "Backward application of 0*elim."))


;;
;; Computations
;;

(defun polynom=0*elim-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num)))
     (and (real=function-p term times)
          (lam~equal-p zero
                       (first (data~appl-arguments term))))))

(defun polynom=0*elim-bp (L2 Pos)
  (let* ((term (data~struct-at-position L2 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num)))
     (lam~equal-p term zero)))


(defun polynom=0*elim-computation-f (L1 Pos)
  (let* ((num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~constant-create 0 num)))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=0*elim-computation-b (L1 Pos Term)
  (let* ((num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num))
         (newterm (term~appl-create times (list zero term))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-0*elim (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos '0-times-real nil nil))
    (tacl~end)))






;; *0elim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic *0elim
		 (outline-mappings (((existent existent) *0elim-a)
				    ((nonexistent existent) *0elim-f)
				    ((existent nonexistent) *0elim-b)))
		 (expansion-function polynom=expand-*0elim)
		 (parameter-types position term)
		 (help "(a*0) => 0"))

(com~defcommand *0elim
  (argnames line1 line2 position term)
  (argtypes ndline ndline position term)
  (arghelps "A line containing (a*0)"
            "A line containing 0"
            "The position of the subterm"
	    "An appropriate a")
  (function polynom=*0elim)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a*0)=0."))

(defun polynom=*0elim (l1 l2 pos term)
  (infer~compute-outline '*0elim (list l2 l1) (list pos term)))



;;
;; *0elim directions
;;

;;
;; F-direction
;;

(tac~deftactic *0elim-f *0elim (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=*0elim-computation-f (formula L1) Pos)))
   (sideconditions (polynom=*0elim-fp (formula L1) Pos))
   (description "Forward application of *0elim."))


;;
;; A-direction
;;


(tac~deftactic *0elim-a *0elim (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=*0elim-ap (formula L1) (formula L2) Pos))
   (description "Test application of *0elim."))

(defun polynom=*0elim-ap (L1 L2 Pos)
  (and (polynom=*0elim-fp L1 Pos)  
       (lam~equal-p (polynom=*0elim-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic *0elim-b *0elim (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=*0elim-computation-b (formula L2) Pos Term)))
   (sideconditions (polynom=*0elim-bp (formula L2) Pos))
   (description "Backward application of *0elim."))


;;
;; Computations
;;

(defun polynom=*0elim-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num)))
     (and (real=function-p term times)
          (lam~equal-p zero
                       (second (data~appl-arguments term))))))

(defun polynom=*0elim-bp (L2 Pos)
  (let* ((term (data~struct-at-position L2 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num)))
     (lam~equal-p term zero)))


(defun polynom=*0elim-computation-f (L1 Pos)
  (let* ((num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~constant-create 0 num)))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=*0elim-computation-b (L1 Pos Term)
  (let* ((num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num))
         (newterm (term~appl-create times (list term zero))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-*0elim (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('c-times (list nil (cadr outline)) (list pos)))
     (s2 ('apply-rewrite (list (car outline) (car s1)) (list pos '0-times-real nil nil))))
    (tacl~end)))






;; add-monomials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic add-monomials
		 (outline-mappings (((existent existent) add-monomials-a)
				    ((nonexistent existent) add-monomials-f)
				    ((existent nonexistent) add-monomials-b)))
		 (expansion-function polynom=expand-add-monomials)
		 (parameter-types position term term)
		 (help "((x*a)+(y*a)) => ((x+y)*a)"))

(com~defcommand add-monomials
  (argnames line1 line2 position term1 term2)
  (argtypes ndline ndline position term term)
  (arghelps "A line containing ((x*a)+(y*a))"
            "A line containing ((x+y)*a)"
            "The position of the subterm"
	    "An appropriate x"
	    "An appropriate y")
  (function polynom=add-monomials)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((x*a)+(y*a))=((x+y)*a) and simplify (x+y)."))

(defun polynom=add-monomials (l1 l2 pos x y)
  (infer~compute-outline 'add-monomials (list l2 l1) (list pos x y)))



;;
;; add-monomials directions
;;

;;
;; F-direction
;;

(tac~deftactic add-monomials-f add-monomials (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=add-monomials-computation-f (formula L1) Pos)))
   (sideconditions (polynom=add-monomials-fp (formula L1) Pos))
   (description "Forward application of add-monomials."))


;;
;; A-direction
;;


(tac~deftactic add-monomials-a add-monomials (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=add-monomials-ap (formula L1) (formula L2) Pos))
   (description "Test application of add-monomials."))

(defun polynom=add-monomials-ap (L1 L2 Pos)
  (and (polynom=add-monomials-fp L1 Pos)  
       (lam~equal-p (polynom=add-monomials-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic add-monomials-b add-monomials (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=add-monomials-computation-b (formula L2) Pos Term1 Term2)))
   (sideconditions (polynom=add-monomials-bp (formula L2) Pos Term1 Term2))
   (description "Backward application of add-monomials."))


;;
;; Computations
;;

(defun polynom=add-monomials-fp (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (plus (env~lookup-object 
                 :plus
                 (pds~environment omega*current-proof-plan))))
    (and (real=function-p term plus)
	 (let* ((args (data~appl-arguments term))
		(arg1 (first args))
		(arg2 (second args))
                (times (env~lookup-object 
                       :times
                       (pds~environment omega*current-proof-plan)))
		(number1p (and (data~primitive-p arg1)
			       (numberp (keim~name arg1))))
		(number2p (and (data~primitive-p arg2)
			       (numberp (keim~name arg2))))
		(prod1p (and (real=function-p arg1 times)
		             (data~primitive-p
			      (first (data~appl-arguments arg1)))
			     (numberp (keim~name
			      (first (data~appl-arguments arg1))))))
		(prod2p (and (real=function-p arg2 times)
		             (data~primitive-p
			      (first (data~appl-arguments arg2)))
			     (numberp (keim~name
			      (first (data~appl-arguments arg2))))))
		(monom1 (cond (prod1p
			       (second (data~appl-arguments arg1)))
			      (number1p
			       nil)
			      (t
			       arg1)))
		(monom2 (cond (prod2p
			       (second (data~appl-arguments arg2)))
			      (number2p
			       nil)
			      (t
			       arg2))))
	   (or (and number1p number2p)
	       (lam~equal-p monom1 monom2))))))
		
(defun polynom=add-monomials-bp (L2 Pos term1 term2)
  (let* ((term (data~struct-at-position L2 Pos))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
	 (prodp (and (real=function-p term times)
		      (data~primitive-p
		       (first (data~appl-arguments term)))
		      (numberp (keim~name
		       (first (data~appl-arguments term))))))
	 (numberp (and (data~primitive-p term)
		       (numberp (keim~name term)))))
    (and (data~primitive-p term1)
	 (data~primitive-p term2)
	 (cond (prodp
	        (= (+ (keim~name term1)
	              (keim~name term2))
	           (keim~name (first (data~appl-arguments term)))))
	       (numberp
	        (= (+ (keim~name term1)
	              (keim~name term2))
	           (keim~name term)))
	       (t
		(= 1 (+ (keim~name term1) (keim~name term2))))))))
     


(defun polynom=add-monomials-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
	 (args (data~appl-arguments term))
	 (arg1 (first args))
	 (arg2 (second args))
         (times (env~lookup-object 
                :times
                (pds~environment omega*current-proof-plan)))
         (num (env~lookup-object 
                :num
                (pds~environment omega*current-proof-plan)))
	 (number1p (and (data~primitive-p arg1)
	       	        (numberp (keim~name arg1))))
	 (number2p (and (data~primitive-p arg2)
			(numberp (keim~name arg2))))
	 (prod1p (and (real=function-p arg1 times)
		      (data~primitive-p
		       (first (data~appl-arguments arg1)))
		      (numberp (keim~name
		       (first (data~appl-arguments arg1))))))
	 (prod2p (and (real=function-p arg2 times)
		       (data~primitive-p
		        (first (data~appl-arguments arg2)))
		       (numberp (keim~name
		        (first (data~appl-arguments arg2))))))
	 (monom  (cond (prod1p
		        (second (data~appl-arguments arg1)))
		       (number1p
		        nil)
		       (t
		        arg1)))
	 (coeff1 (cond (prod1p
		        (keim~name (first (data~appl-arguments arg1))))
		       (number1p
		        (keim~name arg1))
		       (t
		        1)))
	 (coeff2 (cond (prod2p
		        (keim~name (first (data~appl-arguments arg2))))
		       (number2p
		        (keim~name arg2))
		       (t
		        1)))
	 (coeff (+ coeff1 coeff2))
	 (newterm (cond ((and number1p number2p)
			 (term~constant-create coeff num))
		        ((= 1 coeff)
			 monom)
			(t
			 (term~appl-create
			  times
			  (list (term~constant-create coeff num)
				monom))))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=add-monomials-computation-b (L1 Pos term1 term2)
  (let* ((term (data~struct-at-position L1 Pos))
         (times (env~lookup-object 
                :times
                (pds~environment omega*current-proof-plan)))
	 (prodp (and (real=function-p term times)
		      (data~primitive-p
		       (first (data~appl-arguments term)))
		      (numberp (keim~name
		       (first (data~appl-arguments term))))))
	 (numberp (and (data~primitive-p term)
		       (numberp (keim~name term))))
	 (monom (cond (prodp
		       (second (data~appl-arguments term)))
		      (numberp
		       nil)
		      (t
		       term)))
	 (args  (cond (numberp
		       (list term1 term2))
		      (t
		       (list
			(term~appl-create
			 times
			 (list term1 monom))
			(term~appl-create
			 times
			 (list term2 monom))))))
         (plus  (env~lookup-object 
                :plus
                (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create plus args)))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-add-monomials (outline parameters)
  (tacl~init outline)
  (let* ((l1 (pds~node-formula (cadr outline)))
         (l2 (pds~node-formula (car outline)))
         (pos (car parameters))
	 (term1 (data~struct-at-position l1 pos))
	 (args (data~appl-arguments term1))
	 (arg1 (first args))
	 (arg2 (second args))
         (arg3 (data~struct-at-position l2 pos)) 
         (times (env~lookup-object 
                :times
                (pds~environment omega*current-proof-plan)))
	 (numberp (and (data~primitive-p arg1)
	       	        (numberp (keim~name arg1))))
	 (prod1p (and (real=function-p arg1 times)
		      (data~primitive-p
		       (first (data~appl-arguments arg1)))
		      (numberp (keim~name
		       (first (data~appl-arguments arg1))))))
	 (prod2p (and (real=function-p arg2 times)
		       (data~primitive-p
		        (first (data~appl-arguments arg2)))
		       (numberp (keim~name
		        (first (data~appl-arguments arg2))))))
	 (prod3p (and (real=function-p arg3 times)
		       (data~primitive-p
		        (first (data~appl-arguments arg3)))
		       (numberp (keim~name
		        (first (data~appl-arguments arg3)))))))
    (if numberp
        (tacl~apply 'simplify-num
                     outline
                     (list pos (data~struct-at-position l1 pos)))
      (let* ((p1 (if prod1p
                     (cadr outline)
                   (car (tacl~apply '1*i
                                    (list nil (cadr outline))
                                    (list (pos~add-end 1 pos))))))
             (p2 (if prod2p
                     p1
                   (car (tacl~apply '1*i
                                    (list nil p1)
                                    (list (pos~add-end 2 pos))))))
             (c (if prod3p
                    (car outline)
                  (cadr (tacl~apply '1*e
                                    (list (car outline) nil)
                                    (list pos))))))
        (tacl~sequence
         (s1 ('cummulate-right (list nil p2) (list pos)))
         (s2 ('simplify-num (list c (car s1))
                            (list (pos~add-end 1 pos)
                                  (data~struct-at-position (pds~node-formula (car s1))
                                                           (pos~add-end 1 pos)))))))))
  (tacl~end))




;; minus2plus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic minus2plus
		 (outline-mappings (((existent existent) minus2plus-a)
				    ((nonexistent existent) minus2plus-f)
				    ((existent nonexistent) minus2plus-b)))
		 (expansion-function polynom=expand-minus2plus)
		 (parameter-types position)
		 (help "(a-b) => (a+((-1)*b))"))

(com~defcommand minus2plus
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a-b)"
            "A line containing (a+((-1)*b))"
            "The position of the subterm")
  (function polynom=minus2plus)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a-b)=(a+((-1)*b))."))

(defun polynom=minus2plus (l1 l2 pos)
  (infer~compute-outline 'minus2plus (list l2 l1) (list pos)))



;;
;; minus2plus directions
;;

;;
;; F-direction
;;

(tac~deftactic minus2plus-f minus2plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=minus2plus-computation-f (formula L1) Pos)))
   (sideconditions (polynom=minus2plus-fp (formula L1) Pos))
   (description "Forward application of minus2plus."))


;;
;; A-direction
;;


(tac~deftactic minus2plus-a minus2plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=minus2plus-ap (formula L1) (formula L2) Pos))
   (description "Test application of minus2plus."))

(defun polynom=minus2plus-ap (L1 L2 Pos)
  (and (polynom=minus2plus-fp L1 Pos)  
       (lam~equal-p (polynom=minus2plus-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic minus2plus-b minus2plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=minus2plus-computation-b (formula L2) Pos)))
   (sideconditions (polynom=minus2plus-bp (formula L2) Pos))
   (description "Backward application of minus2plus."))


;;
;; Computations
;;

(defun polynom=minus2plus-fp (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (minus (env~lookup-object 
                 :minus
                 (pds~environment omega*current-proof-plan))))
    (real=function-p term minus)))

(defun polynom=minus2plus-bp (L2 Pos)
  (let* ((term (data~struct-at-position L2 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (plus  (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (mone (term~constant-create -1 num)))
    (and (real=function-p term plus)
	 (let ((prod (second (data~appl-arguments term))))
	   (and (polynom=function-p prod times)
		(lam~equal-p (first (data~appl-args prod))
			     mone))))))


(defun polynom=minus2plus-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (plus  (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (mone (term~constant-create -1 num))
	 (args (data~appl-arguments term))
         (newterm (term~appl-create
		   plus
		   (list (first args)
			 (term~appl-create
			  times
			  (list mone (second args)))))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=minus2plus-computation-b (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (args (data~appl-arguments term))
	 (a    (first args))
	 (b    (second (data~appl-arguments (second args))))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create minus (list a b))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-minus2plus (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'minus2plus nil nil))
    (tacl~end)))









;; pop-times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic pop-times
		 (outline-mappings (((existent existent) pop-times-a)
				    ((nonexistent existent) pop-times-f)
				    ((existent nonexistent) pop-times-b)))
		 (expansion-function polynom=expand-pop-times)
		 (parameter-types position)
		 (help "(a*(b*c)) => (b*(a*c))"))

(com~defcommand pop-times
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a*(b*c))"
            "A line containing (b*(a*c))"
            "The position of the subterm")
  (function polynom=pop-times)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a*(b*c))=(b*(a*c))."))

(defun polynom=pop-times (l1 l2 pos)
  (infer~compute-outline 'pop-times (list l2 l1) (list pos)))



;;
;; pop-times directions
;;

;;
;; F-direction
;;

(tac~deftactic pop-times-f pop-times (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=pop-times-computation (formula L1) Pos)))
   (sideconditions (polynom=pop-times-p (formula L1) Pos))
   (description "Forward application of pop-times."))


;;
;; A-direction
;;


(tac~deftactic pop-times-a pop-times (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=pop-times-ap (formula L1) (formula L2) Pos))
   (description "Test application of pop-times."))

(defun polynom=pop-times-ap (L1 L2 Pos)
  (and (polynom=pop-times-p L1 Pos)  
       (lam~equal-p (polynom=pop-times-computation L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic pop-times-b pop-times (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=pop-times-computation (formula L2) Pos)))
   (sideconditions (polynom=pop-times-p (formula L2) Pos))
   (description "Backward application of pop-times."))


;;
;; Computations
;;

(defun polynom=pop-times-p (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (times (env~lookup-object 
                 :times
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term times)
          (real=function-p (second (data~appl-arguments term)) times))))


(defun polynom=pop-times-computation (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (sum  (second (data~appl-arguments term)))
         (a    (first (data~appl-arguments term)))
         (b    (first (data~appl-arguments sum)))
         (c    (second (data~appl-arguments sum)))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
                     times
                     (list b
                           (term~appl-create
                              times
                              (list a c))))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-pop-times (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('a-times-left (list nil precond) (list pos)))
     (s2 ('c-times (list nil (car s1)) (list (pos~add-end 1 pos))))
     (s3 ('a-times-right (list conc (car s2)) (list pos))))
    (tacl~end)))







;; plus2minus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic plus2minus
		 (outline-mappings (((existent existent) plus2minus-a)
				    ((nonexistent existent) plus2minus-f)
				    ((existent nonexistent) plus2minus-b)))
		 (expansion-function polynom=expand-plus2minus)
		 (parameter-types position)
		 (help "(a+((-1)*b)) => (a-b)"))

(com~defcommand plus2minus
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a+((-1)*b))"
            "A line containing (a-b)"
            "The position of the subterm")
  (function polynom=plus2minus)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a+((-1)*b))=(a-b)."))

(defun polynom=plus2minus (l1 l2 pos)
  (infer~compute-outline 'plus2minus (list l2 l1) (list pos)))



;;
;; plus2minus directions
;;

;;
;; F-direction
;;

(tac~deftactic plus2minus-f plus2minus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=minus2plus-computation-b (formula L1) Pos)))
   (sideconditions (polynom=minus2plus-bp (formula L1) Pos))
   (description "Forward application of plus2minus."))


;;
;; A-direction
;;


(tac~deftactic plus2minus-a plus2minus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=plus2minus-ap (formula L1) (formula L2) Pos))
   (description "Test application of plus2minus."))

(defun polynom=plus2minus-ap (L1 L2 Pos)
  (and (polynom=minus2plus-bp L1 Pos)  
       (lam~equal-p (polynom=minus2plus-computation-b L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic plus2minus-b plus2minus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=minus2plus-computation-f (formula L2) Pos)))
   (sideconditions (polynom=minus2plus-fp (formula L2) Pos))
   (description "Backward application of plus2minus."))


;;
;; Expansion
;;

(defun polynom=expand-plus2minus (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'minus2plus nil nil))
    (tacl~end)))






;; +0i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic +0i
		 (outline-mappings (((existent existent) +0i-a)
				    ((nonexistent existent) +0i-f)
				    ((existent nonexistent) +0i-b)))
		 (expansion-function polynom=expand-+0i)
		 (parameter-types position)
		 (help "a => (a+0)"))

(com~defcommand +0i
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a"
            "A line containing (a+0)"
            "The position of the subterm")
  (function polynom=+0i)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite a=(a+0)."))

(defun polynom=+0i (l1 l2 pos)
  (infer~compute-outline '+0i (list l2 l1) (list pos)))



;;
;; +0i directions
;;

;;
;; F-direction
;;

(tac~deftactic +0i-f +0i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=+0e-computation-b (formula L1) Pos)))
   (sideconditions (polynom=+0e-bp (formula L1) Pos))
   (description "Forward application of +0i."))


;;
;; A-direction
;;


(tac~deftactic +0i-a +0i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=+0i-ap (formula L1) (formula L2) Pos))
   (description "Test application of +0i."))

(defun polynom=+0i-ap (L1 L2 Pos)
  (and (polynom=+0e-bp L1 Pos)  
       (lam~equal-p (polynom=+0e-computation-b L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic +0i-b +0i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=+0e-computation-f (formula L2) Pos)))
   (sideconditions (polynom=+0e-fp (formula L2) Pos))
   (description "Backward application of +0i."))


;;
;; Expansion
;;

(defun polynom=expand-+0i (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('0+i (list nil precond) (list pos)))
     (s2 ('c-plus (list conc (car s1)) (list pos))))
    (tacl~end)))





;; 0+i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic 0+i
		 (outline-mappings (((existent existent) 0+i-a)
				    ((nonexistent existent) 0+i-f)
				    ((existent nonexistent) 0+i-b)))
		 (expansion-function polynom=expand-0+i)
		 (parameter-types position)
		 (help "a => (0+a)"))

(com~defcommand 0+i
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a"
            "A line containing (0+a)"
            "The position of the subterm")
  (function polynom=0+i)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite a=(0+a)."))

(defun polynom=0+i (l1 l2 pos)
  (infer~compute-outline '0+i (list l2 l1) (list pos)))



;;
;; 0+i directions
;;

;;
;; F-direction
;;

(tac~deftactic 0+i-f 0+i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=0+e-computation-b (formula L1) Pos)))
   (sideconditions (polynom=0+e-bp (formula L1) Pos))
   (description "Forward application of 0+i."))


;;
;; A-direction
;;


(tac~deftactic 0+i-a 0+i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=0+i-ap (formula L1) (formula L2) Pos))
   (description "Test application of 0+i."))

(defun polynom=0+i-ap (L1 L2 Pos)
  (and (polynom=0+e-bp L1 Pos)  
       (lam~equal-p (polynom=0+e-computation-b L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic 0+i-b 0+i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=0+e-computation-f (formula L2) Pos)))
   (sideconditions (polynom=0+e-fp (formula L2) Pos))
   (description "Backward application of 0+i."))


;;
;; Expansion
;;

(defun polynom=expand-0+i (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos '0-plus-real nil nil))
    (tacl~end)))








;; 0*intro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic 0*intro
		 (outline-mappings (((existent existent) 0*intro-a)
				    ((nonexistent existent) 0*intro-f)
				    ((existent nonexistent) 0*intro-b)))
		 (expansion-function polynom=expand-0*intro)
		 (parameter-types position term)
		 (help "0 => (0*a)"))

(com~defcommand 0*intro
  (argnames line1 line2 position term)
  (argtypes ndline ndline position term)
  (arghelps "A line containing 0"
            "A line containing (0*a)"
            "The position of the subterm"
	    "An appropriate a")
  (function polynom=0*intro)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite 0=(0*a)."))

(defun polynom=0*intro (l1 l2 pos term)
  (infer~compute-outline '0*intro (list l2 l1) (list pos term)))



;;
;; 0*intro directions
;;

;;
;; F-direction
;;

(tac~deftactic 0*intro-f 0*intro (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=0*elim-computation-b (formula L1) Pos Term)))
   (sideconditions (polynom=0*elim-bp (formula L1) Pos))
   (description "Forward application of 0*intro."))


;;
;; A-direction
;;


(tac~deftactic 0*intro-a 0*intro (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=0*intro-ap (formula L1) (formula L2) Pos))
   (description "Test application of 0*intro."))

(defun polynom=0*intro-ap (L1 L2 Pos)
  (and (polynom=0*elim-fp L2 Pos)  
       (lam~equal-p (polynom=0*elim-computation-f L2 Pos) L1)))


;;
;; B-Direction
;;

(tac~deftactic 0*intro-b 0*intro (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=0*elim-computation-f (formula L2) Pos)))
   (sideconditions (polynom=0*elim-fp (formula L2) Pos))
   (description "Backward application of 0*intro."))


;;
;; Expansion
;;

(defun polynom=expand-0*intro (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos '0-times-real nil nil))
    (tacl~end)))





;; *0intro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic *0intro
		 (outline-mappings (((existent existent) *0intro-a)
				    ((nonexistent existent) *0intro-f)
				    ((existent nonexistent) *0intro-b)))
		 (expansion-function polynom=expand-*0intro)
		 (parameter-types position term)
		 (help "0 => (a*0)"))

(com~defcommand *0intro
  (argnames line1 line2 position term)
  (argtypes ndline ndline position term)
  (arghelps "A line containing 0"
            "A line containing (a*0)"
            "The position of the subterm"
	    "An appropriate a")
  (function polynom=*0intro)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite 0=(a*0)."))

(defun polynom=*0intro (l1 l2 pos term)
  (infer~compute-outline '*0intro (list l2 l1) (list pos term)))



;;
;; *0intro directions
;;

;;
;; F-direction
;;

(tac~deftactic *0intro-f *0intro (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=*0elim-computation-b (formula L1) Pos Term)))
   (sideconditions (polynom=*0elim-bp (formula L1) Pos))
   (description "Forward application of *0intro."))


;;
;; A-direction
;;


(tac~deftactic *0intro-a *0intro (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=*0intro-ap (formula L1) (formula L2) Pos))
   (description "Test application of *0intro."))

(defun polynom=*0intro-ap (L1 L2 Pos)
  (and (polynom=*0elim-fp L2 Pos)  
       (lam~equal-p (polynom=*0elim-computation-f L2 Pos) L1)))


;;
;; B-Direction
;;

(tac~deftactic *0intro-b *0intro (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=*0elim-computation-f (formula L2) Pos)))
   (sideconditions (polynom=*0elim-fp (formula L2) Pos))
   (description "Backward application of *0intro."))


;;
;; Expansion
;;

(defun polynom=expand-*0intro (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('c-times (list (car outline) nil) (list pos)))
     (s2 ('apply-rewrite (list (cadr s1) (cadr outline)) (list pos '0-times-real nil nil))))
    (tacl~end)))







;; cross-swap-plus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic cross-swap-plus
		 (outline-mappings (((existent existent) cross-swap-plus-a)
				    ((nonexistent existent) cross-swap-plus-f)
				    ((existent nonexistent) cross-swap-plus-b)))
		 (expansion-function polynom=expand-cross-swap-plus)
		 (parameter-types position)
		 (help "((a+b)+(c+d)) => ((a+c)+(b+d))"))

(com~defcommand cross-swap-plus
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing ((a+b)+(c+d))"
            "A line containing ((a+c)+(b+d))"
            "The position of the subterm")
  (function polynom=cross-swap-plus)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((a+b)+(c+d))=((a+c)+(b+d))."))

(defun polynom=cross-swap-plus (l1 l2 pos)
  (infer~compute-outline 'cross-swap-plus (list l2 l1) (list pos)))



;;
;; cross-swap-plus directions
;;

;;
;; F-direction
;;

(tac~deftactic cross-swap-plus-f cross-swap-plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=cross-swap-plus-computation (formula L1) Pos)))
   (sideconditions (polynom=cross-swap-plus-p (formula L1) Pos))
   (description "Forward application of cross-swap-plus."))


;;
;; A-direction
;;


(tac~deftactic cross-swap-plus-a cross-swap-plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=cross-swap-plus-ap (formula L1) (formula L2) Pos))
   (description "Test application of cross-swap-plus."))

(defun polynom=cross-swap-plus-ap (L1 L2 Pos)
  (and (polynom=cross-swap-plus-p L1 Pos)  
       (lam~equal-p (polynom=cross-swap-plus-computation L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic cross-swap-plus-b cross-swap-plus (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=cross-swap-plus-computation (formula L2) Pos)))
   (sideconditions (polynom=cross-swap-plus-p (formula L2) Pos))
   (description "Backward application of cross-swap-plus."))


;;
;; Computations
;;

(defun polynom=cross-swap-plus-p (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (plus (env~lookup-object 
                 :plus
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term plus)
	  (let ((args (data~appl-arguments term)))
	    (and (real=function-p (first args) plus)
		 (real=function-p (second args) plus))))))

(defun polynom=cross-swap-plus-computation (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (plus (env~lookup-object 
                  :plus
                  (pds~environment omega*current-proof-plan)))
         (args  (data~appl-arguments term))
	 (args1 (data~appl-arguments (first args)))
	 (args2 (data~appl-arguments (second args)))
	 (a (first args1))
	 (b (second args1))
	 (c (first args2))
	 (d (second args2))
         (newterm (term~appl-create
		   plus
		   (list (term~appl-create plus (list a c))
			 (term~appl-create plus (list b d))))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-cross-swap-plus (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('a-plus-right (list nil precond) (list pos)))
     (s2 ('a-plus-left (list nil (car s1)) (list (pos~add-end 2 pos))))
     (s3 ('c-plus (list nil (car s2)) (list (pos~add-end 1 (pos~add-end 2 pos)))))
     (s4 ('a-plus-right (list nil (car s3)) (list (pos~add-end 2 pos))))
     (s5 ('a-plus-left (list conc (car s4)) (list pos))))
    (tacl~end)))








;; split-monomials-plus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic split-monomials-plus
		 (outline-mappings (((existent existent) split-monomials-plus-a)
				    ((nonexistent existent) split-monomials-plus-f)
				    ((existent nonexistent) split-monomials-plus-b)))
		 (expansion-function polynom=expand-split-monomials-plus)
		 (parameter-types position term term)
		 (help "((x*a)+(y*a)) => ((x+y)*a)"))

(com~defcommand split-monomials-plus
  (argnames line1 line2 position term1 term2)
  (argtypes ndline ndline position term term)
  (arghelps "A line containing ((x+y)*a)"
            "A line containing ((x*a)+(y*a))"
            "The position of the subterm"
	    "An appropriate x"
	    "An appropriate y")
  (function polynom=split-monomials-plus)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((x*a)+(y*a))=((x+y)*a) and split (x+y)."))

(defun polynom=split-monomials-plus (l1 l2 pos x y)
  (infer~compute-outline 'split-monomials-plus (list l2 l1) (list pos x y)))



;;
;; split-monomials-plus directions
;;

;;
;; F-direction
;;

(tac~deftactic split-monomials-plus-f split-monomials-plus (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=add-monomials-computation-b (formula L1) Pos Term1 Term2)))
   (sideconditions (polynom=add-monomials-bp (formula L1) Pos Term1 Term2))
   (description "Forward application of split-monomials-plus."))


;;
;; A-direction
;;


(tac~deftactic split-monomials-plus-a split-monomials-plus (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=split-monomials-plus-ap (formula L1) (formula L2) Pos))
   (description "Test application of split-monomials-plus."))

(defun polynom=split-monomials-plus-ap (L1 L2 Pos)
  (and (polynom=add-monomials-fp L2 Pos)  
       (lam~equal-p (polynom=add-monomials-computation-f L2 Pos) L1)))


;;
;; B-Direction
;;

(tac~deftactic split-monomials-plus-b split-monomials-plus (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=add-monomials-computation-f (formula L2) Pos)))
   (sideconditions (polynom=add-monomials-fp (formula L2) Pos))
   (description "Backward application of split-monomials-plus."))


;;
;; Expansion
;;

(defun polynom=expand-split-monomials-plus (outline parameters)
  (tacl~init outline)
  (let* ((l1 (pds~node-formula (cadr outline)))
         (l2 (pds~node-formula (car outline)))
         (pos (car parameters))
	 (term (data~struct-at-position l2 pos))
	 (args (data~appl-arguments term))
	 (arg1 (first args))
	 (arg2 (second args))
         (arg3 (data~struct-at-position l1 pos)) 
         (times (env~lookup-object 
                :times
                (pds~environment omega*current-proof-plan)))
	 (numberp (and (data~primitive-p arg1)
	       	        (numberp (keim~name arg1))))
	 (prod1p (and (real=function-p arg1 times)
		      (data~primitive-p
		       (first (data~appl-arguments arg1)))
		      (numberp (keim~name
		       (first (data~appl-arguments arg1))))))
	 (prod2p (and (real=function-p arg2 times)
		       (data~primitive-p
		        (first (data~appl-arguments arg2)))
		       (numberp (keim~name
		        (first (data~appl-arguments arg2))))))
	 (prod3p (and (real=function-p arg3 times)
		       (data~primitive-p
		        (first (data~appl-arguments arg3)))
		       (numberp (keim~name
		        (first (data~appl-arguments arg3)))))))
    (if numberp
        (tacl~apply 'simplify-num
                     outline
                     (list pos (data~struct-at-position l1 pos)))
      (let* ((c1 (if prod1p
                     (car outline)
                   (cadr (tacl~apply '1*e
                                     (list (car outline) nil)
                                     (list (pos~add-end 1 pos))))))
             (c2 (if prod2p
                     c1
                   (cadr (tacl~apply '1*e
                                     (list c1 nil)
                                     (list (pos~add-end 2 pos))))))
             (p (if prod3p
                    (cadr outline)
                  (car (tacl~apply '1*i
                                    (list nil (cadr outline))
                                    (list pos))))))
        (tacl~sequence
         (s1 ('distribute-right (list c2 nil) (list pos)))
         (s2 ('expand-num (list (cadr s1) p)
                          (list (pos~add-end 1 pos)
                                (data~struct-at-position (pds~node-formula (cadr s1))
                                                         (pos~add-end 1 pos)))))))))
  (tacl~end))




 




;; mult-monomials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic mult-monomials
		 (outline-mappings (((existent existent) mult-monomials-a)
				    ((nonexistent existent) mult-monomials-f)
				    ((existent nonexistent) mult-monomials-b)))
		 (expansion-function polynom=expand-mult-monomials)
		 (parameter-types position term term)
		 (help "((x*a)*(y*a)) => ((x*y)*(a*b))"))

(com~defcommand mult-monomials
  (argnames line1 line2 position term1 term2)
  (argtypes ndline ndline position term term)
  (arghelps "A line containing ((x*a)*(y*b))"
            "A line containing ((x*y)*(a*b))"
            "The position of the subterm"
	    "An appropriate x"
	    "An appropriate y")
  (function polynom=mult-monomials)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((x*a)*(y*b))=((x*y)*(a*b)) and simplify (x*y)."))

(defun polynom=mult-monomials (l1 l2 pos x y)
  (infer~compute-outline 'mult-monomials (list l2 l1) (list pos x y)))



;;
;; mult-monomials directions
;;

;;
;; F-direction
;;

(tac~deftactic mult-monomials-f mult-monomials (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mult-monomials-computation-f (formula L1) Pos)))
   (sideconditions (polynom=mult-monomials-fp (formula L1) Pos))
   (description "Forward application of mult-monomials."))


;;
;; A-direction
;;


(tac~deftactic mult-monomials-a mult-monomials (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=mult-monomials-ap (formula L1) (formula L2) Pos))
   (description "Test application of mult-monomials."))

(defun polynom=mult-monomials-ap (L1 L2 Pos)
  (and (polynom=mult-monomials-fp L1 Pos)  
       (lam~equal-p (polynom=mult-monomials-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic mult-monomials-b mult-monomials (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mult-monomials-computation-b (formula L2) Pos Term1 Term2)))
   (sideconditions (polynom=mult-monomials-bp (formula L2) Pos Term1 Term2))
   (description "Backward application of mult-monomials."))


;;
;; Computations
;;

(defun polynom=mult-monomials-fp (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (times (env~lookup-object 
                 :times
                 (pds~environment omega*current-proof-plan))))
    (real=function-p term times)))
		


(defun polynom=mult-monomials-bp (L2 Pos term1 term2)
  (let* ((term (data~struct-at-position L2 Pos))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
	 (numberp (and (data~primitive-p term)
		       (numberp (keim~name term))))
	 (prodp (and (real=function-p term times)
		     (data~primitive-p
		     (first (data~appl-arguments term)))
		     (numberp (keim~name
		      (first (data~appl-arguments term))))))
         (coeff (cond (numberp
                       (keim~name term))
                      (prodp
                       (keim~name (data~struct-at-position L2 (pos~add-end 1 pos))))
                      (t
                       1))))
    (= coeff (* (keim~name term1) (keim~name term2)))))



(defun polynom=mult-monomials-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
	 (args (data~appl-arguments term))
	 (arg1 (first args))
	 (arg2 (second args))
         (times (env~lookup-object 
                :times
                (pds~environment omega*current-proof-plan)))
         (num (env~lookup-object 
                :num
                (pds~environment omega*current-proof-plan)))
	 (number1p (and (data~primitive-p arg1)
	       	        (numberp (keim~name arg1))))
	 (number2p (and (data~primitive-p arg2)
			(numberp (keim~name arg2))))
	 (prod1p (and (real=function-p arg1 times)
		      (data~primitive-p
		       (first (data~appl-arguments arg1)))
		      (numberp (keim~name
		       (first (data~appl-arguments arg1))))))
	 (prod2p (and (real=function-p arg2 times)
		       (data~primitive-p
		        (first (data~appl-arguments arg2)))
		       (numberp (keim~name
		        (first (data~appl-arguments arg2))))))
	 (monom1 (cond (prod1p
		        (second (data~appl-arguments arg1)))
		       (number1p
		        nil)
		       (t
		        arg1)))
	 (monom2 (cond (prod2p
		        (second (data~appl-arguments arg2)))
		       (number2p
		        nil)
		       (t
		        arg2)))
	 (monom  (cond (number1p
			(cond (number2p
			       nil)
			      (t
			       monom2)))
		       (t
			(cond (number2p
			       monom1)
			      (t
			       (term~appl-create
				times
				(list monom1 monom2)))))))
  	 (coeff1 (cond (prod1p
		        (keim~name (first (data~appl-arguments arg1))))
		       (number1p
		        (keim~name arg1))
		       (t
		        1)))
	 (coeff2 (cond (prod2p
		        (keim~name (first (data~appl-arguments arg2))))
		       (number2p
		        (keim~name arg2))
		       (t
		        1)))
	 (coeff (* coeff1 coeff2))
	 (newterm (cond ((and number1p number2p)
			 (term~constant-create coeff num))
		        ((= 1 coeff)
			 monom)
			(t
			 (term~appl-create
			  times
			  (list (term~constant-create coeff num)
				monom))))))
     (data~replace-at-position L1 Pos newterm)))


(defun polynom=mult-monomials-computation-b (L1 Pos term1 term2)
  (let* ((term (data~struct-at-position L1 Pos))
         (times (env~lookup-object 
                :times
                (pds~environment omega*current-proof-plan)))
	 (prod1p (and (real=function-p term times)
		      (data~primitive-p
		       (first (data~appl-arguments term)))
		      (numberp (keim~name
		       (first (data~appl-arguments term))))))
	 (numberp (and (data~primitive-p term)
		       (numberp (keim~name term))))
	 (monom (cond (prod1p
		       (second (data~appl-arguments term)))
		      (numberp
		       nil)
		      (t
		       term)))
	 (prod2p (real=function-p monom times))
	 (monom1 (if prod2p
		     (first (data~appl-arguments monom))
		   monom))
	 (monom2 (if prod2p
		     (second (data~appl-arguments monom))
		   nil))
	 (arg1   (cond (numberp
			term1)
		       ((= 1 (keim~name term1))
			monom1)
		       (t
			(term~appl-create times (list term1 monom1)))))
	 (arg2   (cond ((or numberp (not prod2p))
			term2)
		       ((= 1 (keim~name term2))
			monom2)
		       (t
			(term~appl-create times (list term2 monom2)))))
         (newterm (term~appl-create times (list arg1 arg2))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-mult-monomials (outline parameters)
  (tacl~init outline)
  (let* ((l1 (pds~node-formula (cadr outline)))
         (l2 (pds~node-formula (car outline)))
         (pos (car parameters))
	 (term (data~struct-at-position l1 pos))
	 (args (data~appl-arguments term))
	 (arg1 (first args))
	 (arg2 (second args))
         (arg3 (data~struct-at-position l2 pos)) 
         (times (env~lookup-object 
                :times
                (pds~environment omega*current-proof-plan)))
	 (number1p (and (data~primitive-p arg1)
	       	        (numberp (keim~name arg1))))
	 (number2p (and (data~primitive-p arg2)
	       	        (numberp (keim~name arg2))))
	 (number3p (and (data~primitive-p arg3)
	       	        (numberp (keim~name arg3))))
	 (prod1p (and (real=function-p arg1 times)
		      (data~primitive-p
		       (first (data~appl-arguments arg1)))
		      (numberp (keim~name
		       (first (data~appl-arguments arg1))))))
	 (prod2p (and (real=function-p arg2 times)
		       (data~primitive-p
		        (first (data~appl-arguments arg2)))
		       (numberp (keim~name
		        (first (data~appl-arguments arg2))))))
	 (prod3p (and (real=function-p arg3 times)
		       (data~primitive-p
		        (first (data~appl-arguments arg3)))
		       (numberp (keim~name
		        (first (data~appl-arguments arg3)))))))
    (if number3p
        (tacl~apply 'simplify-num
                    outline
                    (list pos (data~struct-at-position l1 pos)))
      (let* ((c (if prod3p
                     (car outline)
                   (cadr (tacl~apply '1*e
                                     (list (car outline) nil)
                                     (list pos)))))
             (p1 (if (or prod1p number1p)
                      (cadr outline)
                    (car (tacl~apply '1*i
                                     (list nil (cadr outline))
                                     (list (pos~add-end 1 pos))))))
             (p2 (if (or prod2p number2p)
                      p1
                    (car (tacl~apply '1*i
                                     (list nil p1)
                                     (list (pos~add-end 2 pos)))))))
        (cond
         (number1p
          (tacl~sequence
           (s1 ('a-times-left (list nil p2) (list pos)))
           (s2 ('simplify-num (list c (car s1))
                              (list (pos~add-end 1 pos)
                                    (data~struct-at-position (pds~node-formula (car s1))
                                                             (pos~add-end 1 pos)))))))
         (number2p
          (tacl~sequence
           (s1 ('c-times (list nil p2) (list pos)))
           (s2 ('a-times-left (list nil (car s1)) (list pos)))
           (s3 ('simplify-num (list c (car s2))
                              (list (pos~add-end 1 pos)
                                    (data~struct-at-position (pds~node-formula (car s2))
                                                             (pos~add-end 1 pos)))))))
         (t
          (tacl~sequence
           (s1 ('cross-swap-times (list nil p2) (list pos)))
           (s2 ('simplify-num (list c (car s1))
                              (list (pos~add-end 1 pos)
                                    (data~struct-at-position (pds~node-formula (car s1))
                                                             (pos~add-end 1 pos)))))))))))
  (tacl~end))








;; split-monomials-times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic split-monomials-times
		 (outline-mappings (((existent existent) split-monomials-times-a)
				    ((nonexistent existent) split-monomials-times-f)
				    ((existent nonexistent) split-monomials-times-b)))
		 (expansion-function polynom=expand-split-monomials-times)
		 (parameter-types position term term)
		 (help "((x*y)*(a*b)) => ((x*a)*(y*b))"))

(com~defcommand split-monomials-times
  (argnames line1 line2 position term1 term2)
  (argtypes ndline ndline position term term)
  (arghelps "A line containing ((x*y)*(a*b))"
            "A line containing ((x*a)*(y*b))"
            "The position of the subterm"
	    "An appropriate x"
	    "An appropriate y")
  (function polynom=split-monomials-times)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((x*y)*(a*b))=((x*a)*(y*b)) and split (x*y)."))

(defun polynom=split-monomials-times (l1 l2 pos x y)
  (infer~compute-outline 'split-monomials-times (list l2 l1) (list pos x y)))



;;
;; split-monomials-times directions
;;

;;
;; F-direction
;;

(tac~deftactic split-monomials-times-f split-monomials-times (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mult-monomials-computation-b (formula L1) Pos Term1 Term2)))
   (sideconditions (polynom=mult-monomials-bp (formula L1) Pos Term1 Term2))
   (description "Forward application of split-monomials-times."))


;;
;; A-direction
;;


(tac~deftactic split-monomials-times-a split-monomials-times (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=mult-monomials-ap (formula L2) (formula L1) Pos))
   (description "Test application of split-monomials-times."))


;;
;; B-Direction
;;

(tac~deftactic split-monomials-times-b split-monomials-times (in base)
   (parameters (POS pos+position "A position")
	       (TERM1 term+term "A number")
	       (TERM2 term+term "A number"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mult-monomials-computation-f (formula L2) Pos)))
   (sideconditions (polynom=mult-monomials-fp (formula L2) Pos))
   (description "Backward application of split-monomials-times."))


;;
;; Expansion
;;

(defun polynom=expand-split-monomials-times (outline parameters)
  (tacl~init outline)
  (let* ((l1 (pds~node-formula (cadr outline)))
         (l2 (pds~node-formula (car outline)))
         (pos (car parameters))
	 (term (data~struct-at-position l2 pos))
	 (args (data~appl-arguments term))
	 (arg1 (first args))
	 (arg2 (second args))
         (arg3 (data~struct-at-position l1 pos)) 
         (times (env~lookup-object 
                :times
                (pds~environment omega*current-proof-plan)))
	 (number1p (and (data~primitive-p arg1)
	       	        (numberp (keim~name arg1))))
	 (number2p (and (data~primitive-p arg2)
	       	        (numberp (keim~name arg2))))
	 (number3p (and (data~primitive-p arg3)
	       	        (numberp (keim~name arg3))))
	 (prod1p (and (real=function-p arg1 times)
		      (data~primitive-p
		       (first (data~appl-arguments arg1)))
		      (numberp (keim~name
		       (first (data~appl-arguments arg1))))))
	 (prod2p (and (real=function-p arg2 times)
		       (data~primitive-p
		        (first (data~appl-arguments arg2)))
		       (numberp (keim~name
		        (first (data~appl-arguments arg2))))))
	 (prod3p (and (real=function-p arg3 times)
		       (data~primitive-p
		        (first (data~appl-arguments arg3)))
		       (numberp (keim~name
		        (first (data~appl-arguments arg3)))))))
    (if number3p
        (tacl~apply 'expand-num
                    outline
                    (list pos (data~struct-at-position l2 pos)))
      (let* ((p (if prod3p
                     (cadr outline)
                   (car (tacl~apply '1*i
                                     (list nil (cadr outline))
                                     (list pos)))))
             (c1 (if (or prod1p number1p)
                      (car outline)
                    (cadr (tacl~apply '1*e
                                     (list (car outline) nil)
                                     (list (pos~add-end 1 pos))))))
             (c2 (if (or prod2p number2p)
                      c1
                    (cadr (tacl~apply '1*e
                                     (list c1 nil)
                                     (list (pos~add-end 2 pos)))))))
        (cond
         (number1p
          (tacl~sequence
           (s1 ('a-times-right (list c2 nil) (list pos)))
           (s2 ('expand-num (list (cadr s1) p)
                              (list (pos~add-end 1 pos)
                                    (data~struct-at-position (pds~node-formula (cadr s1))
                                                             (pos~add-end 1 pos)))))))
         (number2p
          (tacl~sequence
           (s1 ('c-times (list c2 nil) (list pos)))
           (s2 ('a-times-right (list (cadr s1) nil) (list pos)))
           (s3 ('expand-num (list (cadr s2) p)
                              (list (pos~add-end 1 pos)
                                    (data~struct-at-position (pds~node-formula (cadr s2))
                                                             (pos~add-end 1 pos)))))))
         (t
          (tacl~sequence
           (s1 ('cross-swap-times (list c2 nil) (list pos)))
           (s2 ('expand-num (list (cadr s1) p)
                              (list (pos~add-end 1 pos)
                                    (data~struct-at-position (pds~node-formula (cadr s1))
                                                             (pos~add-end 1 pos)))))))))))
  (tacl~end))





;; *1i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic *1i
		 (outline-mappings (((existent existent) *1i-a)
				    ((nonexistent existent) *1i-f)
				    ((existent nonexistent) *1i-b)))
		 (expansion-function polynom=expand-*1i)
		 (parameter-types position)
		 (help "a => (a*1)"))

(com~defcommand *1i
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing a"
            "A line containing (a*1)"
            "The position of the subterm")
  (function polynom=*1i)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite a=(a*1)."))

(defun polynom=*1i (l1 l2 pos)
  (infer~compute-outline '*1i (list l2 l1) (list pos)))



;;
;; *1i directions
;;

;;
;; F-direction
;;

(tac~deftactic *1i-f *1i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=*1e-computation-b (formula L1) Pos)))
   (sideconditions (polynom=*1e-bp (formula L1) Pos))
   (description "Forward application of *1i."))


;;
;; A-direction
;;


(tac~deftactic *1i-a *1i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=*1i-ap (formula L1) (formula L2) Pos))
   (description "Test application of *1i."))

(defun polynom=*1i-ap (L1 L2 Pos)
  (and (polynom=*1e-bp L1 Pos)  
       (lam~equal-p (polynom=*1e-computation-b L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic *1i-b *1i (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=*1e-computation-f (formula L2) Pos)))
   (sideconditions (polynom=*1e-fp (formula L2) Pos))
   (description "Backward application of *1i."))



;;
;; Expansion
;;

(defun polynom=expand-*1i (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('1*i (list nil precond) (list pos)))
     (s2 ('c-times (list conc (car s1)) (list pos))))
    (tacl~end)))









;; cross-swap-times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic cross-swap-times
		 (outline-mappings (((existent existent) cross-swap-times-a)
				    ((nonexistent existent) cross-swap-times-f)
				    ((existent nonexistent) cross-swap-times-b)))
		 (expansion-function polynom=expand-cross-swap-times)
		 (parameter-types position)
		 (help "((a*b)*(c*d)) => ((a*c)*(b*d))"))

(com~defcommand cross-swap-times
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing ((a*b)*(c*d))"
            "A line containing ((a*c)*(b*d))"
            "The position of the subterm")
  (function polynom=cross-swap-times)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((a*b)*(c*d))=((a*c)*(b*d))."))

(defun polynom=cross-swap-times (l1 l2 pos)
  (infer~compute-outline 'cross-swap-times (list l2 l1) (list pos)))



;;
;; cross-swap-times directions
;;

;;
;; F-direction
;;

(tac~deftactic cross-swap-times-f cross-swap-times (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=cross-swap-times-computation (formula L1) Pos)))
   (sideconditions (polynom=cross-swap-times-p (formula L1) Pos))
   (description "Forward application of cross-swap-times."))


;;
;; A-direction
;;


(tac~deftactic cross-swap-times-a cross-swap-times (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=cross-swap-times-ap (formula L1) (formula L2) Pos))
   (description "Test application of cross-swap-times."))

(defun polynom=cross-swap-times-ap (L1 L2 Pos)
  (and (polynom=cross-swap-times-p L1 Pos)  
       (lam~equal-p (polynom=cross-swap-times-computation L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic cross-swap-times-b cross-swap-times (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=cross-swap-times-computation (formula L2) Pos)))
   (sideconditions (polynom=cross-swap-times-p (formula L2) Pos))
   (description "Backward application of cross-swap-times."))


;;
;; Computations
;;

(defun polynom=cross-swap-times-p (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
        (times (env~lookup-object 
                 :times
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term times)
	  (let ((args (data~appl-arguments term)))
	    (and (real=function-p (first args) times)
		 (real=function-p (second args) times))))))

(defun polynom=cross-swap-times-computation (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (args  (data~appl-arguments term))
	 (args1 (data~appl-arguments (first args)))
	 (args2 (data~appl-arguments (second args)))
	 (a (first args1))
	 (b (second args1))
	 (c (first args2))
	 (d (second args2))
         (newterm (term~appl-create
		   times
		   (list (term~appl-create times (list a c))
			 (term~appl-create times (list b d))))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-cross-swap-times (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (pos (car parameters)))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('a-times-right (list nil precond) (list pos)))
     (s2 ('a-times-left (list nil (car s1)) (list (pos~add-end 2 pos))))
     (s3 ('c-times (list nil (car s2)) (list (pos~add-end 1 (pos~add-end 2 pos)))))
     (s4 ('a-times-right (list nil (car s3)) (list (pos~add-end 2 pos))))
     (s5 ('a-times-left (list conc (car s4)) (list pos))))
    (tacl~end)))







;; split-power
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic split-power
		 (outline-mappings (((existent existent) split-power-a)
				    ((nonexistent existent) split-power-f)
				    ((existent nonexistent) split-power-b)))
		 (expansion-function polynom=expand-split-power)
		 (parameter-types position)
		 (help "(a^(b+c)) => ((a^b)*(a^c))"))

(com~defcommand split-power
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a^(b+c))"
            "A line containing ((a^b)*(a^c))"
            "The position of the subterm")
  (function polynom=split-power)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a^(b+c))=((a^b)*(a^c))."))

(defun polynom=split-power (l1 l2 pos)
  (infer~compute-outline 'split-power (list l2 l1) (list pos)))



;;
;; split-power directions
;;

;;
;; F-direction
;;

(tac~deftactic split-power-f split-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mult-power-computation-b (formula L1) Pos)))
   (sideconditions (polynom=mult-power-bp (formula L1) Pos))
   (description "Forward application of split-power."))


;;
;; A-direction
;;


(tac~deftactic split-power-a split-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=mult-power-ap (formula L2) (formula L1) Pos))
   (description "Test application of split-power."))


;;
;; B-Direction
;;

(tac~deftactic split-power-b split-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mult-power-computation-f (formula L2) Pos)))
   (sideconditions (polynom=mult-power-fp (formula L2) Pos))
   (description "Backward application of split-power."))


;;
;; Expansion
;;

(defun polynom=expand-split-power (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'times-power-real nil nil))
    (tacl~end)))





;; 0^e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic 0^e
		 (outline-mappings (((existent existent) 0^e-a)
				    ((nonexistent existent) 0^e-f)
				    ((existent nonexistent) 0^e-b)))
		 (expansion-function polynom=expand-0^e)
		 (parameter-types position term)
		 (help "(0^a) => 0"))

(com~defcommand 0^e
  (argnames line1 line2 position term)
  (argtypes ndline ndline position term)
  (arghelps "A line containing (0^a)"
            "A line containing 0"
            "The position of the subterm"
	    "An appropriate a")
  (function polynom=0^e)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (0^a)=0."))

(defun polynom=0^e (l1 l2 pos term)
  (infer~compute-outline '0^e (list l2 l1) (list pos term)))



;;
;; 0^e directions
;;

;;
;; F-direction
;;

(tac~deftactic 0^e-f 0^e (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=0^e-computation-f (formula L1) Pos)))
   (sideconditions (polynom=0^e-fp (formula L1) Pos))
   (description "Forward application of 0^e."))


;;
;; A-direction
;;


(tac~deftactic 0^e-a 0^e (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=0^e-ap (formula L1) (formula L2) Pos))
   (description "Test application of 0^e."))

(defun polynom=0^e-ap (L1 L2 Pos)
  (and (polynom=0^e-fp L1 Pos)  
       (lam~equal-p (polynom=0^e-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic 0^e-b 0^e (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=0^e-computation-b (formula L2) Pos Term)))
   (sideconditions (polynom=0^e-bp (formula L2) Pos))
   (description "Backward application of 0^e."))


;;
;; Computations
;;

(defun polynom=0^e-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num)))
     (and (real=function-p term power)
          (lam~equal-p zero
                       (first (data~appl-arguments term))))))

(defun polynom=0^e-bp (L2 Pos)
  (let* ((term (data~struct-at-position L2 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num)))
     (lam~equal-p term zero)))


(defun polynom=0^e-computation-f (L1 Pos)
  (let* ((num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~constant-create 0 num)))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=0^e-computation-b (L1 Pos Term)
  (let* ((num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num))
         (newterm (term~appl-create power (list zero term))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-0^e (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos '0-power-real nil nil))
    (tacl~end)))





;; 0^i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic 0^i
		 (outline-mappings (((existent existent) 0^i-a)
				    ((nonexistent existent) 0^i-f)
				    ((existent nonexistent) 0^i-b)))
		 (expansion-function polynom=expand-0^i)
		 (parameter-types position term)
		 (help "0 => (0^a)"))

(com~defcommand 0^i
  (argnames line1 line2 position term)
  (argtypes ndline ndline position term)
  (arghelps "A line containing 0"
            "A line containing (0^a)"
            "The position of the subterm"
	    "An appropriate a")
  (function polynom=0^i)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite 0=(0^a)."))

(defun polynom=0^i (l1 l2 pos term)
  (infer~compute-outline '0^i (list l2 l1) (list pos term)))



;;
;; 0^i directions
;;

;;
;; F-direction
;;

(tac~deftactic 0^i-f 0^i (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=0^e-computation-b (formula L1) Pos Term)))
   (sideconditions (polynom=0^e-bp (formula L1) Pos))
   (description "Forward application of 0^i."))


;;
;; A-direction
;;


(tac~deftactic 0^i-a 0^i (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=0^e-ap (formula L2) (formula L1) Pos))
   (description "Test application of 0^i."))


;;
;; B-Direction
;;

(tac~deftactic 0^i-b 0^i (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=0^e-computation-f (formula L2) Pos)))
   (sideconditions (polynom=0^e-fp (formula L2) Pos))
   (description "Backward application of 0^i."))


;;
;; Expansion
;;

(defun polynom=expand-0^i (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos '0-power-real nil nil))
    (tacl~end)))



;; 1^e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic 1^e
		 (outline-mappings (((existent existent) 1^e-a)
				    ((nonexistent existent) 1^e-f)
				    ((existent nonexistent) 1^e-b)))
		 (expansion-function polynom=expand-1^e)
		 (parameter-types position term)
		 (help "(1^a) => 1"))

(com~defcommand 1^e
  (argnames line1 line2 position term)
  (argtypes ndline ndline position term)
  (arghelps "A line containing (1^a)"
            "A line containing 1"
            "The position of the subterm"
	    "An appropriate a")
  (function polynom=1^e)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (1^a)=1."))

(defun polynom=1^e (l1 l2 pos term)
  (infer~compute-outline '1^e (list l2 l1) (list pos term)))



;;
;; 1^e directions
;;

;;
;; F-direction
;;

(tac~deftactic 1^e-f 1^e (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=1^e-computation-f (formula L1) Pos)))
   (sideconditions (polynom=1^e-fp (formula L1) Pos))
   (description "Forward application of 1^e."))


;;
;; A-direction
;;


(tac~deftactic 1^e-a 1^e (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=1^e-ap (formula L1) (formula L2) Pos))
   (description "Test application of 1^e."))

(defun polynom=1^e-ap (L1 L2 Pos)
  (and (polynom=1^e-fp L1 Pos)  
       (lam~equal-p (polynom=1^e-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic 1^e-b 1^e (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=1^e-computation-b (formula L2) Pos Term)))
   (sideconditions (polynom=1^e-bp (formula L2) Pos))
   (description "Backward application of 1^e."))


;;
;; Computations
;;

(defun polynom=1^e-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (one (term~constant-create 1 num)))
     (and (real=function-p term power)
          (lam~equal-p one
                       (first (data~appl-arguments term))))))

(defun polynom=1^e-bp (L2 Pos)
  (let* ((term (data~struct-at-position L2 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (one (term~constant-create 1 num)))
     (lam~equal-p term one)))


(defun polynom=1^e-computation-f (L1 Pos)
  (let* ((num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~constant-create 1 num)))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=1^e-computation-b (L1 Pos Term)
  (let* ((num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (one (term~constant-create 1 num))
         (newterm (term~appl-create power (list one term))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-1^e (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos '1-power-real nil nil))
    (tacl~end)))





;; 1^i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic 1^i
		 (outline-mappings (((existent existent) 1^i-a)
				    ((nonexistent existent) 1^i-f)
				    ((existent nonexistent) 1^i-b)))
		 (expansion-function polynom=expand-1^i)
		 (parameter-types position term)
		 (help "1 => (1^a)"))

(com~defcommand 1^i
  (argnames line1 line2 position term)
  (argtypes ndline ndline position term)
  (arghelps "A line containing 1"
            "A line containing (1^a)"
            "The position of the subterm"
	    "An appropriate a")
  (function polynom=1^i)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite 1=(1^a)."))

(defun polynom=1^i (l1 l2 pos term)
  (infer~compute-outline '1^i (list l2 l1) (list pos term)))



;;
;; 1^i directions
;;

;;
;; F-direction
;;

(tac~deftactic 1^i-f 1^i (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=1^e-computation-b (formula L1) Pos Term)))
   (sideconditions (polynom=1^e-bp (formula L1) Pos))
   (description "Forward application of 1^i."))


;;
;; A-direction
;;


(tac~deftactic 1^i-a 1^i (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=1^e-ap (formula L2) (formula L1) Pos))
   (description "Test application of 1^i."))


;;
;; B-Direction
;;

(tac~deftactic 1^i-b 1^i (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=1^e-computation-f (formula L2) Pos)))
   (sideconditions (polynom=1^e-fp (formula L2) Pos))
   (description "Backward application of 1^i."))


;;
;; Expansion
;;

(defun polynom=expand-1^i (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos '1-power-real nil nil))
    (tacl~end)))





;; ^0e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic ^0e
		 (outline-mappings (((existent existent) ^0e-a)
				    ((nonexistent existent) ^0e-f)
				    ((existent nonexistent) ^0e-b)))
		 (expansion-function polynom=expand-^0e)
		 (parameter-types position term)
		 (help "(a^0) => 0"))

(com~defcommand ^0e
  (argnames line1 line2 position term)
  (argtypes ndline ndline position term)
  (arghelps "A line containing (a^0)"
            "A line containing 0"
            "The position of the subterm"
	    "An appropriate a")
  (function polynom=^0e)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a^0)=0."))

(defun polynom=^0e (l1 l2 pos term)
  (infer~compute-outline '^0e (list l2 l1) (list pos term)))



;;
;; ^0e directions
;;

;;
;; F-direction
;;

(tac~deftactic ^0e-f ^0e (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=^0e-computation-f (formula L1) Pos)))
   (sideconditions (polynom=^0e-fp (formula L1) Pos))
   (description "Forward application of ^0e."))


;;
;; A-direction
;;


(tac~deftactic ^0e-a ^0e (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=^0e-ap (formula L1) (formula L2) Pos))
   (description "Test application of ^0e."))

(defun polynom=^0e-ap (L1 L2 Pos)
  (and (polynom=^0e-fp L1 Pos)  
       (lam~equal-p (polynom=^0e-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic ^0e-b ^0e (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=^0e-computation-b (formula L2) Pos Term)))
   (sideconditions (polynom=^0e-bp (formula L2) Pos))
   (description "Backward application of ^0e."))


;;
;; Computations
;;

(defun polynom=^0e-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num)))
     (and (real=function-p term power)
          (lam~equal-p zero
                       (second (data~appl-arguments term))))))

(defun polynom=^0e-bp (L2 Pos)
  (let* ((term (data~struct-at-position L2 Pos))
         (num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (one (term~constant-create 1 num)))
     (lam~equal-p term one)))


(defun polynom=^0e-computation-f (L1 Pos)
  (let* ((num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~constant-create 1 num)))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=^0e-computation-b (L1 Pos Term)
  (let* ((num  (env~lookup-object 
                  :num
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (zero (term~constant-create 0 num))
         (newterm (term~appl-create power (list term zero))))
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-^0e (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'power-0-real nil nil))
    (tacl~end)))





;; ^0i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic ^0i
		 (outline-mappings (((existent existent) ^0i-a)
				    ((nonexistent existent) ^0i-f)
				    ((existent nonexistent) ^0i-b)))
		 (expansion-function polynom=expand-^0i)
		 (parameter-types position term)
		 (help "0 => (a^0)"))

(com~defcommand ^0i
  (argnames line1 line2 position term)
  (argtypes ndline ndline position term)
  (arghelps "A line containing 0"
            "A line containing (a^0)"
            "The position of the subterm"
	    "An appropriate a")
  (function polynom=^0i)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite 0=(a^0)."))

(defun polynom=^0i (l1 l2 pos term)
  (infer~compute-outline '^0i (list l2 l1) (list pos term)))



;;
;; ^0i directions
;;

;;
;; F-direction
;;

(tac~deftactic ^0i-f ^0i (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=^0e-computation-b (formula L1) Pos Term)))
   (sideconditions (polynom=^0e-bp (formula L1) Pos))
   (description "Forward application of ^0i."))


;;
;; A-direction
;;


(tac~deftactic ^0i-a ^0i (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=^0e-ap (formula L2) (formula L1) Pos))
   (description "Test application of ^0i."))


;;
;; B-Direction
;;

(tac~deftactic ^0i-b ^0i (in base)
   (parameters (POS pos+position "A position")
	       (TERM term+term   "A term"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=^0e-computation-f (formula L2) Pos)))
   (sideconditions (polynom=^0e-fp (formula L2) Pos))
   (description "Backward application of ^0i."))


;;
;; Expansion
;;

(defun polynom=expand-^0i (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'power-0-real nil nil))
    (tacl~end)))





;; mult-base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic mult-base
		 (outline-mappings (((existent existent) mult-base-a)
				    ((nonexistent existent) mult-base-f)
				    ((existent nonexistent) mult-base-b)))
		 (expansion-function polynom=expand-mult-base)
		 (parameter-types position)
		 (help "((a^c)*(b^c)) => ((a*b)^c)"))

(com~defcommand mult-base
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing ((a^c)*(b^c))"
            "A line containing ((a*b)^c)"
            "The position of the subterm")
  (function polynom=mult-base)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((a^c)*(b^c))=((a*b)^c)."))

(defun polynom=mult-base (l1 l2 pos)
  (infer~compute-outline 'mult-base (list l2 l1) (list pos)))



;;
;; mult-base directions
;;

;;
;; F-direction
;;

(tac~deftactic mult-base-f mult-base (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mult-base-computation-f (formula L1) Pos)))
   (sideconditions (polynom=mult-base-fp (formula L1) Pos))
   (description "Forward application of mult-base."))


;;
;; A-direction
;;


(tac~deftactic mult-base-a mult-base (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=mult-base-ap (formula L1) (formula L2) Pos))
   (description "Test application of mult-base."))

(defun polynom=mult-base-ap (L1 L2 Pos)
  (and (polynom=mult-base-fp L1 Pos)  
       (lam~equal-p (polynom=mult-base-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic mult-base-b mult-base (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mult-base-computation-b (formula L2) Pos)))
   (sideconditions (polynom=mult-base-bp (formula L2) Pos))
   (description "Backward application of mult-base."))


;;
;; Computations
;;

(defun polynom=mult-base-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos)))
     (and (data~appl-p term)
	  (let* (
	 (args (data~appl-arguments term))
	 (ex1  (first args))
	 (ex2  (second args))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
	 )
     (and (real=function-p term times)
	  (real=function-p ex1 power)
	  (real=function-p ex2 power)
	  (lam~equal-p (second (data~appl-arguments ex1))
		       (second (data~appl-arguments ex2))))))))

(defun polynom=mult-base-bp (L2 Pos)
  (let ((term (data~struct-at-position L2 Pos))
        (times (env~lookup-object 
                 :times
                 (pds~environment omega*current-proof-plan)))
        (power (env~lookup-object 
                 :power
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term power)
	  (real=function-p
	    (first (data~appl-arguments term))
	    times))))


(defun polynom=mult-base-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
	 (args (data~appl-arguments term))
	 (ex1  (first args))
	 (ex2  (second args))
	 (a    (first (data~appl-arguments ex1)))
	 (b    (first (data~appl-arguments ex2)))
	 (c    (second (data~appl-arguments ex1)))	       
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
		   power
		   (list (term~appl-create times
					   (list a b))
                         c))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=mult-base-computation-b (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
	 (args (data~appl-arguments term))
	 (prod (data~appl-arguments  (first args)))
	 (c    (second args))
	 (a    (first prod))
	 (b    (second prod))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
		   times
		   (list 
	            (term~appl-create
		     power (list a c))
		    (term~appl-create
		     power (list b c))))))			 
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-mult-base (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'times-power-2-real nil nil))
    (tacl~end)))







;; split-base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic split-base
		 (outline-mappings (((existent existent) split-base-a)
				    ((nonexistent existent) split-base-f)
				    ((existent nonexistent) split-base-b)))
		 (expansion-function polynom=expand-split-base)
		 (parameter-types position)
		 (help "((a*b)^c) => ((a^c)*(b^c))"))

(com~defcommand split-base
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing ((a*b)^c)"
            "A line containing ((a^c)*(b^c))"
            "The position of the subterm")
  (function polynom=split-base)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((a*b)^c)=((a^c)*(b^c))."))

(defun polynom=split-base (l1 l2 pos)
  (infer~compute-outline 'split-base (list l2 l1) (list pos)))



;;
;; split-base directions
;;

;;
;; F-direction
;;

(tac~deftactic split-base-f split-base (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mult-base-computation-b (formula L1) Pos)))
   (sideconditions (polynom=mult-base-bp (formula L1) Pos))
   (description "Forward application of split-base."))


;;
;; A-direction
;;


(tac~deftactic split-base-a split-base (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=mult-base-ap (formula L2) (formula L1) Pos))
   (description "Test application of split-base."))


;;
;; B-Direction
;;

(tac~deftactic split-base-b split-base (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mult-base-computation-f (formula L2) Pos)))
   (sideconditions (polynom=mult-base-fp (formula L2) Pos))
   (description "Backward application of split-base."))


;;
;; Expansion
;;

(defun polynom=expand-split-base (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'times-power-2-real nil nil))
    (tacl~end)))





;; mult-exp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic mult-exp
		 (outline-mappings (((existent existent) mult-exp-a)
				    ((nonexistent existent) mult-exp-f)
				    ((existent nonexistent) mult-exp-b)))
		 (expansion-function polynom=expand-mult-exp)
		 (parameter-types position)
		 (help "((a^b)^c) => (a^(b*c))"))

(com~defcommand mult-exp
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing ((a^b)^c)"
            "A line containing (a^(b*c))"
            "The position of the subterm")
  (function polynom=mult-exp)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite ((a^b)^c)=(a^(b*c))."))

(defun polynom=mult-exp (l1 l2 pos)
  (infer~compute-outline 'mult-exp (list l2 l1) (list pos)))



;;
;; mult-exp directions
;;

;;
;; F-direction
;;

(tac~deftactic mult-exp-f mult-exp (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mult-exp-computation-f (formula L1) Pos)))
   (sideconditions (polynom=mult-exp-fp (formula L1) Pos))
   (description "Forward application of mult-exp."))


;;
;; A-direction
;;


(tac~deftactic mult-exp-a mult-exp (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=mult-exp-ap (formula L1) (formula L2) Pos))
   (description "Test application of mult-exp."))

(defun polynom=mult-exp-ap (L1 L2 Pos)
  (and (polynom=mult-exp-fp L1 Pos)  
       (lam~equal-p (polynom=mult-exp-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic mult-exp-b mult-exp (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mult-exp-computation-b (formula L2) Pos)))
   (sideconditions (polynom=mult-exp-bp (formula L2) Pos))
   (description "Backward application of mult-exp."))


;;
;; Computations
;;

(defun polynom=mult-exp-fp (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan))))
     (and (real=function-p term power)
	  (real=function-p (first (data~appl-arguments term))
				 power))))

(defun polynom=mult-exp-bp (L2 Pos)
  (let ((term (data~struct-at-position L2 Pos))
        (times (env~lookup-object 
                 :times
                 (pds~environment omega*current-proof-plan)))
        (power (env~lookup-object 
                 :power
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term power)
	  (real=function-p
	    (second (data~appl-arguments term))
	    times))))


(defun polynom=mult-exp-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
	 (args (data~appl-arguments term))
	 (ex   (first args))
	 (a    (first (data~appl-arguments ex)))
	 (b    (second (data~appl-arguments ex)))	       
	 (c    (second args))
         (times (env~lookup-object 
                  :times
                  (pds~environment omega*current-proof-plan)))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
		   power
		   (list a
		         (term~appl-create times
					   (list b c))))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=mult-exp-computation-b (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
	 (args (data~appl-arguments term))
	 (prod (data~appl-arguments  (second args)))
	 (a    (first args))
	 (b    (first prod))
	 (c    (second prod))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan)))
         (newterm (term~appl-create
		   power
		   (list
	            (term~appl-create
		     power (list a b))
		    c))))			 
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-mult-exp (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'power-power-real nil nil))
    (tacl~end)))







;; split-exp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic split-exp
		 (outline-mappings (((existent existent) split-exp-a)
				    ((nonexistent existent) split-exp-f)
				    ((existent nonexistent) split-exp-b)))
		 (expansion-function polynom=expand-split-exp)
		 (parameter-types position)
		 (help "(a^(b*c)) => ((a^b)^c)"))

(com~defcommand split-exp
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a^(b*c))"
            "A line containing ((a^b)^c)"
            "The position of the subterm")
  (function polynom=split-exp)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a^(b*c))=((a^b)^c)."))

(defun polynom=split-exp (l1 l2 pos)
  (infer~compute-outline 'split-exp (list l2 l1) (list pos)))



;;
;; split-exp directions
;;

;;
;; F-direction
;;

(tac~deftactic split-exp-f split-exp (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=mult-exp-computation-b (formula L1) Pos)))
   (sideconditions (polynom=mult-exp-bp (formula L1) Pos))
   (description "Forward application of split-exp."))


;;
;; A-direction
;;


(tac~deftactic split-exp-a split-exp (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=mult-exp-ap (formula L2) (formula L1) Pos))
   (description "Test application of split-exp."))


;;
;; B-Direction
;;

(tac~deftactic split-exp-b split-exp (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=mult-exp-computation-f (formula L2) Pos)))
   (sideconditions (polynom=mult-exp-fp (formula L2) Pos))
   (description "Backward application of split-exp."))


;;
;; Expansion
;;

(defun polynom=expand-split-exp (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'power-power-real nil nil))
    (tacl~end)))






;; inflate-power
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic inflate-power
		 (outline-mappings (((existent existent) inflate-power-a)
				    ((nonexistent existent) inflate-power-f)
				    ((existent nonexistent) inflate-power-b)))
		 (expansion-function polynom=expand-inflate-power)
		 (parameter-types position)
		 (help "(a^n) => (a*(a^(n-1)))"))

(com~defcommand inflate-power
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a^n)"
            "A line containing (a*(a^(n-1)))"
            "The position of the subterm")
  (function polynom=inflate-power)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a^n)=(a*(a^(n-1)))."))

(defun polynom=inflate-power (l1 l2 pos)
  (infer~compute-outline 'inflate-power (list l2 l1) (list pos)))



;;
;; inflate-power directions
;;

;;
;; F-direction
;;

(tac~deftactic inflate-power-f inflate-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=inflate-power-computation-f (formula L1) Pos)))
   (sideconditions (polynom=inflate-power-fp (formula L1) Pos))
   (description "Forward application of inflate-power."))


;;
;; A-direction
;;


(tac~deftactic inflate-power-a inflate-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=inflate-power-ap (formula L1) (formula L2) Pos))
   (description "Test application of inflate-power."))

(defun polynom=inflate-power-ap (L1 L2 Pos)
  (and (polynom=inflate-power-fp L1 Pos)  
       (lam~equal-p (polynom=inflate-power-computation-f L1 Pos) L2)))


;;
;; B-Direction
;;

(tac~deftactic inflate-power-b inflate-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=inflate-power-computation-b (formula L2) Pos)))
   (sideconditions (polynom=inflate-power-bp (formula L2) Pos))
   (description "Backward application of inflate-power."))


;;
;; Computations
;;

(defun polynom=inflate-power-fp (L1 Pos)
  (let ((term (data~struct-at-position L1 Pos))
         (power (env~lookup-object 
                  :power
                  (pds~environment omega*current-proof-plan))))
     (and (real=function-p term power)
	  (let ((arg (second (data~appl-arguments term))))
	    (and (data~primitive-p arg)
		 (numberp (keim~name arg))
		 (let ((n (keim~name arg)))
		   (and (= (floor n) n) (> n 1))))))))

(defun polynom=inflate-power-bp (L2 Pos)
  (let ((term (data~struct-at-position L2 Pos))
        (power (env~lookup-object 
                 :power
                 (pds~environment omega*current-proof-plan)))
        (times (env~lookup-object 
                 :times
                 (pds~environment omega*current-proof-plan))))
     (and (real=function-p term times)
	  (let ((a (first (data~appl-arguments term)))
		(b (second (data~appl-arguments term))))
	    (or (lam~equal-p a b)
		(and (real=function-p b power)
		     (lam~equal-p a (first (data~appl-arguments b)))
	             (let ((arg (second (data~appl-arguments b))))
	               (and (data~primitive-p arg)
		            (numberp (keim~name arg))
		            (let ((n (keim~name arg)))
		              (and (= (floor n) n) (> n 1)))))))))))

	    
(defun polynom=inflate-power-computation-f (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (a    (first (data~appl-arguments term)))
	 (n    (keim~name (second (data~appl-arguments term))))
         (power (env~lookup-object 
                 :power
                 (pds~environment omega*current-proof-plan)))
         (times (env~lookup-object 
                 :times
                 (pds~environment omega*current-proof-plan)))
         (num   (env~lookup-object 
                 :num
                 (pds~environment omega*current-proof-plan)))
         (newterm (if (= 2 n)
		      (term~appl-create times (list a a))
		    (term~appl-create
		     times
		     (list a
			   (term~appl-create
			    power
			    (list a
				  (term~constant-create (- n 1) num))))))))
     (data~replace-at-position L1 Pos newterm)))

(defun polynom=inflate-power-computation-b (L1 Pos)
  (let* ((term (data~struct-at-position L1 Pos))
         (power (env~lookup-object 
                 :power
                 (pds~environment omega*current-proof-plan)))
         (num   (env~lookup-object 
                 :num
                 (pds~environment omega*current-proof-plan)))
         (a    (first (data~appl-arguments term)))
	 (b    (second (data~appl-arguments term)))
	 (n    (if (real=function-p b power)
		   (keim~name (second
			       (data~appl-arguments
				b)))
		 1))
         (newterm (term~appl-create power
				    (list a
				          (term~constant-create (+ n 1) num)))))  
     (data~replace-at-position L1 Pos newterm)))
   

;;
;; Expansion
;;

(defun polynom=expand-inflate-power (outline parameters)
  (let* ((l1 (pds~node-formula (cadr outline)))
         (pos (car parameters))
	 (term (data~struct-at-position l1 pos))
	 (exp (keim~name (second (data~appl-arguments term))))
         (num (env~lookup-object 
               :num
               (pds~environment omega*current-proof-plan)))
         (plus (env~lookup-object 
               :plus
               (pds~environment omega*current-proof-plan)))
         (arg1 (term~constant-create 1 num))
         (arg2 (term~constant-create (- exp 1) num))
         (sum (term~appl-create plus (list arg1 arg2))))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('expand-num (list nil (cadr outline)) (list (pos~add-end 2 pos) sum)))
     (s2 ('split-power (list nil (car s1)) (list pos)))
     (s3 ('^1e (list (car outline) (car s2)) (list (pos~add-end 1 pos)))))
    (tacl~end)))





;; deflate-power
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic deflate-power
		 (outline-mappings (((existent existent) deflate-power-a)
				    ((nonexistent existent) deflate-power-f)
				    ((existent nonexistent) deflate-power-b)))
		 (expansion-function polynom=expand-deflate-power)
		 (parameter-types position)
		 (help "(a*(a^(n-1))) => (a^n)"))

(com~defcommand deflate-power
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A line containing (a*(a^(n-1)))"
            "A line containing (a^n)"
            "The position of the subterm")
  (function polynom=deflate-power)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Rewrite (a*(a^(n-1)))=(a^n)."))

(defun polynom=deflate-power (l1 l2 pos)
  (infer~compute-outline 'deflate-power (list l2 l1) (list pos)))



;;
;; deflate-power directions
;;

;;
;; F-direction
;;

(tac~deftactic deflate-power-f deflate-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (polynom=inflate-power-computation-b (formula L1) Pos)))
   (sideconditions (polynom=inflate-power-bp (formula L1) Pos))
   (description "Forward application of deflate-power."))


;;
;; A-direction
;;


(tac~deftactic deflate-power-a deflate-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (polynom=inflate-power-ap (formula L2) (formula L1) Pos))
   (description "Test application of deflate-power."))


;;
;; B-Direction
;;

(tac~deftactic deflate-power-b deflate-power (in base)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (polynom=inflate-power-computation-f (formula L2) Pos)))
   (sideconditions (polynom=inflate-power-fp (formula L2) Pos))
   (description "Backward application of deflate-power."))


;;
;; Expansion
;;

(defun polynom=expand-deflate-power (outline parameters)
  (let* ((l2 (pds~node-formula (car outline)))
         (pos (car parameters))
	 (term (data~struct-at-position l2 pos))
	 (exp (keim~name (second (data~appl-arguments term))))
         (num (env~lookup-object 
               :num
               (pds~environment omega*current-proof-plan)))
         (plus (env~lookup-object 
               :plus
               (pds~environment omega*current-proof-plan)))
         (arg1 (term~constant-create 1 num))
         (arg2 (term~constant-create (- exp 1) num))
         (sum (term~appl-create plus (list arg1 arg2))))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('simplify-num (list (car outline) nil) (list (pos~add-end 2 pos) sum)))
     (s2 ('mult-power (list (cadr s1) nil) (list pos)))
     (s3 ('^1i (list (cadr s2) (cadr outline)) (list (pos~add-end 1 pos)))))
    (tacl~end)))

; TACO TACTIC div2times

; TACO Variables

;phi x y

; TACO Theory Constants

;times div num

; TACO Parameters

;(pos position)

; TACO Patterns

;(nonexistent existent)
;(existent existent)

; TACO Theory

;real

; TACO Premises

;(l1 (formula phi (div x y) pos))

; TACO Conclusions

;(l2 (formula phi (times x
;{term~constant-create (/ 1 (keim~name ?y)) ?num})
;pos))

; TACO Constraints

;{and (data~primitive-p ?y)
;     (numberp (keim~name ?y))}

; TACO General Help

;Rewrite x/y = x*(1/y).

; TACO Argument Help

;(l2 "A Line containg x*(1/y)")
;(l1 "A Line containing x/y")
;(pos "The position of the term")

; TACO Expansion

;(inference bla (l2 l1))

; TACO Code

(infer~deftactic div2times
 (outline-mappings
  (((nonexistent existent)
    div2times-1)
   ((existent existent)
    div2times-2)))
 (expansion-function taco=expand-div2times)
 (parameter-types position)
 (help "Rewrite x/y = x*(1/y)."))


(com~defcommand div2times
 (argnames l2 l1 pos)
 (argtypes ndline ndline position)
 (arghelps "a line containg x*(1/y)" "a line containing x/y" "the position of the term")
 (function taco=div2times)
 (frag-cats tactics)
 (defaults)
 (log-p t)
 (help "Rewrite x/y = x*(1/y)."))


(defun taco=div2times
 (l2 l1 pos)
 (infer~compute-outline 'div2times
  (list l2 l1)
  (list pos)))


(tac~deftactic div2times-1 div2times
 (in real)
 (parameters
  (pos pos+position "the position of the term"))
 (premises l1)
 (conclusions l2)
 (computations
  (l2
   (taco=div2times-1-l2
    (formula l1)
    pos)))
 (sideconditions
  (taco=div2times-1-p
   (formula l1)
   pos))
 (description "Apply tactic div2times to pattern (nonexistent existent)."))


(defun taco=div2times-1-l2
 (taco-l1 taco-pos)
 (let*
  ((taco-num
    (env~lookup-object :num
     (pds~environment omega*current-proof-plan)))
   (taco-x0
    (data~struct-at-position taco-l1 taco-pos))
   (taco-x1
    (data~appl-arguments taco-x0))
   (taco-y
    (nth 1 taco-x1))
   (taco-x4
    (term~constant-create (/ 1 (keim~name taco-y )) taco-num))
   (taco-x
    (nth 0 taco-x1))
   (taco-x3
    (list taco-x taco-x4))
   (taco-times
    (env~lookup-object :times
     (pds~environment omega*current-proof-plan)))
   (taco-x2
    (data~appl-create taco-times taco-x3)))
  (data~replace-at-position taco-l1 taco-pos taco-x2)))


(defun taco=div2times-1-p
 (taco-l1 taco-pos)
 (let*
  ((taco-x0
    (data~struct-at-position taco-l1 taco-pos))
   (taco-div
    (env~lookup-object :div
     (pds~environment omega*current-proof-plan))))
  (and
   (data~appl-p taco-x0)
   (term~taco-equal taco-div
    (data~appl-function taco-x0))
   (let*
    ((taco-x1
      (data~appl-arguments taco-x0)))
    (and
     (listp taco-x1)
     (=
      (list-length taco-x1)
      2)
     (let*
      ((taco-y
        (nth 1 taco-x1)))
      (and (data~primitive-p taco-y )
     (numberp (keim~name taco-y )))))))))


(tac~deftactic div2times-2 div2times
 (in real)
 (parameters
  (pos pos+position "the position of the term"))
 (premises l1)
 (conclusions l2)
 (computations)
 (sideconditions
  (taco=div2times-2-p
   (formula l2)
   (formula l1)
   pos))
 (description "Apply tactic div2times to pattern (existent existent)."))


(defun taco=div2times-2-p
 (taco-l2 taco-l1 taco-pos)
 (let*
  ((taco-x2
    (data~struct-at-position taco-l2 taco-pos))
   (taco-x0
    (data~struct-at-position taco-l1 taco-pos))
   (taco-num
    (env~lookup-object :num
     (pds~environment omega*current-proof-plan)))
   (taco-div
    (env~lookup-object :div
     (pds~environment omega*current-proof-plan)))
   (taco-times
    (env~lookup-object :times
     (pds~environment omega*current-proof-plan))))
  (and
   (data~appl-p taco-x2)
   (data~appl-p taco-x0)
   (term~taco-equal taco-times
    (data~appl-function taco-x2))
   (term~taco-equal taco-div
    (data~appl-function taco-x0))
   (let*
    ((taco-x3
      (data~appl-arguments taco-x2))
     (taco-x1
      (data~appl-arguments taco-x0)))
    (and
     (listp taco-x3)
     (=
      (list-length taco-x3)
      2)
     (listp taco-x1)
     (=
      (list-length taco-x1)
      2)
     (let*
      ((taco-x4
        (nth 1 taco-x3))
       (taco-x
        (nth 0 taco-x3))
       (taco-y
        (nth 1 taco-x1)))
      (and
       (and (data~primitive-p taco-y )
     (numberp (keim~name taco-y )))
       (term~taco-equal taco-x4
        (term~constant-create (/ 1 (keim~name taco-y )) taco-num))
       (term~taco-equal taco-x
        (nth 0 taco-x1))
       (term~taco-equal taco-x4
        (term~constant-create (/ 1 (keim~name taco-y )) taco-num)))))))))
; TACO Protected

(defun taco=expand-div2times (outline parameters)
  (let* ((l1 (pds~node-formula (cadr outline)))
         (pos (car parameters))
	 (term (data~struct-at-position l1 pos))
         (num (env~lookup-object 
               :num
               (pds~environment omega*current-proof-plan)))
         (divide (env~lookup-object 
                 :divide
                 (pds~environment omega*current-proof-plan)))
         (arg1 (term~constant-create 1 num))
         (arg2 (second (data~appl-arguments term)))
         (frac (term~appl-create divide (list arg1 arg2))))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('apply-rewrite (list nil (cadr outline)) (list pos 'div2times nil nil)))
     (s2 ('simplify-num (list (car outline) (car s1)) (list (pos~add-end 2 pos) frac))))
    (tacl~end)))
; TACO END


; TACO TACTIC times2div

; TACO Variables

;phi x y

; TACO Theory Constants

;times div num

; TACO Parameters

;(pos position)

; TACO Patterns

;(existent nonexistent)
;(existent existent)

; TACO Theory

;real

; TACO Premises

;(l1 (formula phi (times x
;{term~constant-create (/ 1 (keim~name ?y)) ?num})
;pos))

; TACO Conclusions

;(l2 (formula phi (div x y) pos))

; TACO Constraints

;{and (data~primitive-p ?y)
;     (numberp (keim~name ?y))}

; TACO General Help

;Rewrite x/y = x*(1/y).

; TACO Argument Help

;(l1 "A Line containg x*(1/y)")
;(l2 "A Line containing x/y")
;(pos "The position of the term")

; TACO Expansion

;(inference bla (l2 l1))

; TACO Code

(infer~deftactic times2div
 (outline-mappings
  (((existent nonexistent)
    times2div-1)
   ((existent existent)
    times2div-2)))
 (expansion-function taco=expand-times2div)
 (parameter-types position)
 (help "Rewrite x/y = x*(1/y)."))


(com~defcommand times2div
 (argnames l2 l1 pos)
 (argtypes ndline ndline position)
 (arghelps "a line containing x/y" "a line containg x*(1/y)" "the position of the term")
 (function taco=times2div)
 (frag-cats tactics)
 (defaults)
 (log-p t)
 (help "Rewrite x/y = x*(1/y)."))


(defun taco=times2div
 (l2 l1 pos)
 (infer~compute-outline 'times2div
  (list l2 l1)
  (list pos)))


(tac~deftactic times2div-1 times2div
 (in real)
 (parameters
  (pos pos+position "the position of the term"))
 (premises l1)
 (conclusions l2)
 (computations
  (l1
   (taco=times2div-1-l1
    (formula l2)
    pos)))
 (sideconditions
  (taco=times2div-1-p
   (formula l2)
   pos))
 (description "Apply tactic times2div to pattern (existent nonexistent)."))


(defun taco=times2div-1-l1
 (taco-l2 taco-pos)
 (let*
  ((taco-num
    (env~lookup-object :num
     (pds~environment omega*current-proof-plan)))
   (taco-x3
    (data~struct-at-position taco-l2 taco-pos))
   (taco-x4
    (data~appl-arguments taco-x3))
   (taco-y
    (nth 1 taco-x4))
   (taco-x2
    (term~constant-create (/ 1 (keim~name taco-y )) taco-num))
   (taco-x
    (nth 0 taco-x4))
   (taco-x1
    (list taco-x taco-x2))
   (taco-times
    (env~lookup-object :times
     (pds~environment omega*current-proof-plan)))
   (taco-x0
    (data~appl-create taco-times taco-x1)))
  (data~replace-at-position taco-l2 taco-pos taco-x0)))


(defun taco=times2div-1-p
 (taco-l2 taco-pos)
 (let*
  ((taco-x3
    (data~struct-at-position taco-l2 taco-pos))
   (taco-div
    (env~lookup-object :div
     (pds~environment omega*current-proof-plan))))
  (and
   (data~appl-p taco-x3)
   (term~taco-equal taco-div
    (data~appl-function taco-x3))
   (let*
    ((taco-x4
      (data~appl-arguments taco-x3)))
    (and
     (listp taco-x4)
     (=
      (list-length taco-x4)
      2)
     (let*
      ((taco-y
        (nth 1 taco-x4)))
      (and (data~primitive-p taco-y )
     (numberp (keim~name taco-y )))))))))


(tac~deftactic times2div-2 times2div
 (in real)
 (parameters
  (pos pos+position "the position of the term"))
 (premises l1)
 (conclusions l2)
 (computations)
 (sideconditions
  (taco=times2div-2-p
   (formula l2)
   (formula l1)
   pos))
 (description "Apply tactic times2div to pattern (existent existent)."))


(defun taco=times2div-2-p
 (taco-l2 taco-l1 taco-pos)
 (let*
  ((taco-x3
    (data~struct-at-position taco-l2 taco-pos))
   (taco-x0
    (data~struct-at-position taco-l1 taco-pos))
   (taco-num
    (env~lookup-object :num
     (pds~environment omega*current-proof-plan)))
   (taco-div
    (env~lookup-object :div
     (pds~environment omega*current-proof-plan)))
   (taco-times
    (env~lookup-object :times
     (pds~environment omega*current-proof-plan))))
  (and
   (data~appl-p taco-x3)
   (data~appl-p taco-x0)
   (term~taco-equal taco-div
    (data~appl-function taco-x3))
   (term~taco-equal taco-times
    (data~appl-function taco-x0))
   (let*
    ((taco-x4
      (data~appl-arguments taco-x3))
     (taco-x1
      (data~appl-arguments taco-x0)))
    (and
     (listp taco-x4)
     (=
      (list-length taco-x4)
      2)
     (listp taco-x1)
     (=
      (list-length taco-x1)
      2)
     (let*
      ((taco-y
        (nth 1 taco-x4))
       (taco-x
        (nth 0 taco-x4))
       (taco-x2
        (nth 1 taco-x1)))
      (and
       (and (data~primitive-p taco-y )
     (numberp (keim~name taco-y )))
       (term~taco-equal taco-x2
        (term~constant-create (/ 1 (keim~name taco-y )) taco-num))
       (term~taco-equal taco-x
        (nth 0 taco-x1))
       (term~taco-equal taco-x2
        (term~constant-create (/ 1 (keim~name taco-y )) taco-num)))))))))
; TACO Protected

(defun taco=expand-times2div (outline parameters)
  (let* ((l1 (pds~node-formula (car outline)))
         (pos (car parameters))
	 (term (data~struct-at-position l1 pos))
         (num (env~lookup-object 
               :num
               (pds~environment omega*current-proof-plan)))
         (divide (env~lookup-object 
                 :divide
                 (pds~environment omega*current-proof-plan)))
         (arg1 (term~constant-create 1 num))
         (arg2 (second (data~appl-arguments term)))
         (frac (term~appl-create divide (list arg1 arg2))))
    (tacl~init outline)
    (tacl~sequence
     (s1 ('apply-rewrite (list (car outline) nil) (list pos 'div2times nil nil)))
     (s2 ('expand-num (list (cadr s1) (cadr outline)) (list (pos~add-end 2 pos) frac))))
    (tacl~end)))
; TACO END




; TACO TACTIC sqrt2power

; TACO Variables

;phi x

; TACO Theory Constants

;sqrt power num

; TACO Parameters

;(pos position)

; TACO Patterns

;(nonexistent existent)
;(existent existent)
;(existent nonexistent)

; TACO Theory

;real

; TACO Premises

;(l1 (formula phi (sqrt x) pos))

; TACO Conclusions

;(l2 (formula phi (power x 1/2) pos))

; TACO General Help

;Rewrite sqrt(x) = x^(1/2).

; TACO Argument Help

;(l2 "A Line containg x^(1/2)")
;(l1 "A Line containing sqrt(x)")
;(pos "The position of the term")

; TACO Expansion

;(inference bla (l2 l1))

; TACO Code

(infer~deftactic sqrt2power
 (outline-mappings
  (((nonexistent existent)
    sqrt2power-1)
   ((existent existent)
    sqrt2power-2)
   ((existent nonexistent)
    sqrt2power-3)))
 (expansion-function taco=expand-sqrt2power)
 (parameter-types position)
 (help "Rewrite sqrt(x) = x^(1/2)."))


(com~defcommand sqrt2power
 (argnames l2 l1 pos)
 (argtypes ndline ndline position)
 (arghelps "a line containg x^(1/2)" "a line containing sqrt(x)" "the position of the term")
 (function taco=sqrt2power)
 (frag-cats tactics)
 (defaults)
 (log-p t)
 (help "Rewrite sqrt(x) = x^(1/2)."))


(defun taco=sqrt2power
 (l2 l1 pos)
 (infer~compute-outline 'sqrt2power
  (list l2 l1)
  (list pos)))


(tac~deftactic sqrt2power-1 sqrt2power
 (in real)
 (parameters
  (pos pos+position "the position of the term"))
 (premises l1)
 (conclusions l2)
 (computations
  (l2
   (taco=sqrt2power-1-l2
    (formula l1)
    pos)))
 (sideconditions
  (taco=sqrt2power-1-p
   (formula l1)
   pos))
 (description "Apply tactic sqrt2power to pattern (nonexistent existent)."))


(defun taco=sqrt2power-1-l2
 (taco-l1 taco-pos)
 (let*
  ((taco-num
    (env~lookup-object :num
     (pds~environment omega*current-proof-plan)))
   (taco-x4
    (term~constant-create 1/2 taco-num))
   (taco-x0
    (data~struct-at-position taco-l1 taco-pos))
   (taco-x1
    (data~appl-arguments taco-x0))
   (taco-x
    (nth 0 taco-x1))
   (taco-x3
    (list taco-x taco-x4))
   (taco-power
    (env~lookup-object :power
     (pds~environment omega*current-proof-plan)))
   (taco-x2
    (data~appl-create taco-power taco-x3)))
  (data~replace-at-position taco-l1 taco-pos taco-x2)))


(defun taco=sqrt2power-1-p
 (taco-l1 taco-pos)
 (let*
  ((taco-x0
    (data~struct-at-position taco-l1 taco-pos))
   (taco-sqrt
    (env~lookup-object :sqrt
     (pds~environment omega*current-proof-plan))))
  (and
   (data~appl-p taco-x0)
   (term~taco-equal taco-sqrt
    (data~appl-function taco-x0))
   (let*
    ((taco-x1
      (data~appl-arguments taco-x0)))
    (and
     (listp taco-x1)
     (=
      (list-length taco-x1)
      1))))))


(tac~deftactic sqrt2power-2 sqrt2power
 (in real)
 (parameters
  (pos pos+position "the position of the term"))
 (premises l1)
 (conclusions l2)
 (computations)
 (sideconditions
  (taco=sqrt2power-2-p
   (formula l2)
   (formula l1)
   pos))
 (description "Apply tactic sqrt2power to pattern (existent existent)."))


(defun taco=sqrt2power-2-p
 (taco-l2 taco-l1 taco-pos)
 (let*
  ((taco-x2
    (data~struct-at-position taco-l2 taco-pos))
   (taco-x0
    (data~struct-at-position taco-l1 taco-pos))
   (taco-num
    (env~lookup-object :num
     (pds~environment omega*current-proof-plan)))
   (taco-power
    (env~lookup-object :power
     (pds~environment omega*current-proof-plan)))
   (taco-sqrt
    (env~lookup-object :sqrt
     (pds~environment omega*current-proof-plan))))
  (and
   (data~appl-p taco-x2)
   (data~appl-p taco-x0)
   (term~taco-equal taco-power
    (data~appl-function taco-x2))
   (term~taco-equal taco-sqrt
    (data~appl-function taco-x0))
   (let*
    ((taco-x3
      (data~appl-arguments taco-x2))
     (taco-x1
      (data~appl-arguments taco-x0))
     (taco-x4
      (term~constant-create 1/2 taco-num)))
    (and
     (listp taco-x3)
     (=
      (list-length taco-x3)
      2)
     (listp taco-x1)
     (=
      (list-length taco-x1)
      1)
     (term~taco-equal taco-x4
      (nth 1 taco-x3))
     (let*
      ((taco-x
        (nth 0 taco-x3)))
      (term~taco-equal taco-x
       (nth 0 taco-x1))))))))


(tac~deftactic sqrt2power-3 sqrt2power
 (in real)
 (parameters
  (pos pos+position "the position of the term"))
 (premises l1)
 (conclusions l2)
 (computations
  (l1
   (taco=sqrt2power-3-l1
    (formula l2)
    pos)))
 (sideconditions
  (taco=sqrt2power-3-p
   (formula l2)
   pos))
 (description "Apply tactic sqrt2power to pattern (existent nonexistent)."))


(defun taco=sqrt2power-3-l1
 (taco-l2 taco-pos)
 (let*
  ((taco-x2
    (data~struct-at-position taco-l2 taco-pos))
   (taco-x3
    (data~appl-arguments taco-x2))
   (taco-x
    (nth 0 taco-x3))
   (taco-x1
    (list taco-x))
   (taco-sqrt
    (env~lookup-object :sqrt
     (pds~environment omega*current-proof-plan)))
   (taco-x0
    (data~appl-create taco-sqrt taco-x1)))
  (data~replace-at-position taco-l2 taco-pos taco-x0)))


(defun taco=sqrt2power-3-p
 (taco-l2 taco-pos)
 (let*
  ((taco-x2
    (data~struct-at-position taco-l2 taco-pos))
   (taco-num
    (env~lookup-object :num
     (pds~environment omega*current-proof-plan)))
   (taco-power
    (env~lookup-object :power
     (pds~environment omega*current-proof-plan))))
  (and
   (data~appl-p taco-x2)
   (term~taco-equal taco-power
    (data~appl-function taco-x2))
   (let*
    ((taco-x3
      (data~appl-arguments taco-x2))
     (taco-x4
      (term~constant-create 1/2 taco-num)))
    (and
     (listp taco-x3)
     (=
      (list-length taco-x3)
      2)
     (term~taco-equal taco-x4
      (nth 1 taco-x3)))))))
; TACO Protected

(defun taco=expand-sqrt2power (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'sqrt2power nil nil))
    (tacl~end)))
; TACO END



; TACO TACTIC power2sqrt

; TACO Variables

;phi x

; TACO Theory Constants

;sqrt power num

; TACO Parameters

;(pos position)

; TACO Patterns

;(nonexistent existent)
;(existent existent)
;(existent nonexistent)

; TACO Theory

;real

; TACO Premises

;(l1 (formula phi (power x 1/2) pos))

; TACO Conclusions

;(l2 (formula phi (sqrt x) pos))

; TACO General Help

;Rewrite sqrt(x) = x^(1/2).

; TACO Argument Help

;(l2 "A Line containg sqrt(x)")
;(l1 "A Line containing x^(1/2)")
;(pos "The position of the term")

; TACO Expansion

;(inference bla (l2 l1))

; TACO Code

(infer~deftactic power2sqrt
 (outline-mappings
  (((nonexistent existent)
    power2sqrt-1)
   ((existent existent)
    power2sqrt-2)
   ((existent nonexistent)
    power2sqrt-3)))
 (expansion-function taco=expand-power2sqrt)
 (parameter-types position)
 (help "Rewrite sqrt(x) = x^(1/2)."))


(com~defcommand power2sqrt
 (argnames l2 l1 pos)
 (argtypes ndline ndline position)
 (arghelps "a line containg sqrt(x)" "a line containing x^(1/2)" "the position of the term")
 (function taco=power2sqrt)
 (frag-cats tactics)
 (defaults)
 (log-p t)
 (help "Rewrite sqrt(x) = x^(1/2)."))


(defun taco=power2sqrt
 (l2 l1 pos)
 (infer~compute-outline 'power2sqrt
  (list l2 l1)
  (list pos)))


(tac~deftactic power2sqrt-1 power2sqrt
 (in real)
 (parameters
  (pos pos+position "the position of the term"))
 (premises l1)
 (conclusions l2)
 (computations
  (l2
   (taco=power2sqrt-1-l2
    (formula l1)
    pos)))
 (sideconditions
  (taco=power2sqrt-1-p
   (formula l1)
   pos))
 (description "Apply tactic power2sqrt to pattern (nonexistent existent)."))


(defun taco=power2sqrt-1-l2
 (taco-l1 taco-pos)
 (let*
  ((taco-x0
    (data~struct-at-position taco-l1 taco-pos))
   (taco-x1
    (data~appl-arguments taco-x0))
   (taco-x
    (nth 0 taco-x1))
   (taco-x4
    (list taco-x))
   (taco-sqrt
    (env~lookup-object :sqrt
     (pds~environment omega*current-proof-plan)))
   (taco-x3
    (data~appl-create taco-sqrt taco-x4)))
  (data~replace-at-position taco-l1 taco-pos taco-x3)))


(defun taco=power2sqrt-1-p
 (taco-l1 taco-pos)
 (let*
  ((taco-x0
    (data~struct-at-position taco-l1 taco-pos))
   (taco-num
    (env~lookup-object :num
     (pds~environment omega*current-proof-plan)))
   (taco-power
    (env~lookup-object :power
     (pds~environment omega*current-proof-plan))))
  (and
   (data~appl-p taco-x0)
   (term~taco-equal taco-power
    (data~appl-function taco-x0))
   (let*
    ((taco-x1
      (data~appl-arguments taco-x0))
     (taco-x2
      (term~constant-create 1/2 taco-num)))
    (and
     (listp taco-x1)
     (=
      (list-length taco-x1)
      2)
     (term~taco-equal taco-x2
      (nth 1 taco-x1)))))))


(tac~deftactic power2sqrt-2 power2sqrt
 (in real)
 (parameters
  (pos pos+position "the position of the term"))
 (premises l1)
 (conclusions l2)
 (computations)
 (sideconditions
  (taco=power2sqrt-2-p
   (formula l2)
   (formula l1)
   pos))
 (description "Apply tactic power2sqrt to pattern (existent existent)."))


(defun taco=power2sqrt-2-p
 (taco-l2 taco-l1 taco-pos)
 (let*
  ((taco-x3
    (data~struct-at-position taco-l2 taco-pos))
   (taco-x0
    (data~struct-at-position taco-l1 taco-pos))
   (taco-num
    (env~lookup-object :num
     (pds~environment omega*current-proof-plan)))
   (taco-power
    (env~lookup-object :power
     (pds~environment omega*current-proof-plan)))
   (taco-sqrt
    (env~lookup-object :sqrt
     (pds~environment omega*current-proof-plan))))
  (and
   (data~appl-p taco-x3)
   (data~appl-p taco-x0)
   (term~taco-equal taco-sqrt
    (data~appl-function taco-x3))
   (term~taco-equal taco-power
    (data~appl-function taco-x0))
   (let*
    ((taco-x4
      (data~appl-arguments taco-x3))
     (taco-x1
      (data~appl-arguments taco-x0))
     (taco-x2
      (term~constant-create 1/2 taco-num)))
    (and
     (listp taco-x4)
     (=
      (list-length taco-x4)
      1)
     (listp taco-x1)
     (=
      (list-length taco-x1)
      2)
     (term~taco-equal taco-x2
      (nth 1 taco-x1))
     (let*
      ((taco-x
        (nth 0 taco-x4)))
      (term~taco-equal taco-x
       (nth 0 taco-x1))))))))


(tac~deftactic power2sqrt-3 power2sqrt
 (in real)
 (parameters
  (pos pos+position "the position of the term"))
 (premises l1)
 (conclusions l2)
 (computations
  (l1
   (taco=power2sqrt-3-l1
    (formula l2)
    pos)))
 (sideconditions
  (taco=power2sqrt-3-p
   (formula l2)
   pos))
 (description "Apply tactic power2sqrt to pattern (existent nonexistent)."))


(defun taco=power2sqrt-3-l1
 (taco-l2 taco-pos)
 (let*
  ((taco-num
    (env~lookup-object :num
     (pds~environment omega*current-proof-plan)))
   (taco-x2
    (term~constant-create 1/2 taco-num))
   (taco-x3
    (data~struct-at-position taco-l2 taco-pos))
   (taco-x4
    (data~appl-arguments taco-x3))
   (taco-x
    (nth 0 taco-x4))
   (taco-x1
    (list taco-x taco-x2))
   (taco-power
    (env~lookup-object :power
     (pds~environment omega*current-proof-plan)))
   (taco-x0
    (data~appl-create taco-power taco-x1)))
  (data~replace-at-position taco-l2 taco-pos taco-x0)))


(defun taco=power2sqrt-3-p
 (taco-l2 taco-pos)
 (let*
  ((taco-x3
    (data~struct-at-position taco-l2 taco-pos))
   (taco-sqrt
    (env~lookup-object :sqrt
     (pds~environment omega*current-proof-plan))))
  (and
   (data~appl-p taco-x3)
   (term~taco-equal taco-sqrt
    (data~appl-function taco-x3))
   (let*
    ((taco-x4
      (data~appl-arguments taco-x3)))
    (and
     (listp taco-x4)
     (=
      (list-length taco-x4)
      1))))))
; TACO Protected

(defun taco=expand-power2sqrt (outline parameters)
  (let ((pos (car parameters)))
    (tacl~init outline)
    (tacl~apply 'apply-rewrite outline (list pos 'sqrt2power nil nil))
    (tacl~end)))
; TACO END



;; by-computation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic by-computation
		      (outline-mappings (((existent list) by-computation-a)))
		      (expansion-function real=expand-by-computation)
		      (help "Calls Maple to justify a line."))



(defun by-computation-a (conc prems param)
  (declare (ignore param))
  (when (real=by-computation-p prems conc)
    (values t t )))

;(infer~deftactic by-computation
;                 (outline-mappings (((existent existent) by-computation-a)))
;                 (expansion-function real=expand-by-computation)
;                 (help "Calls Maple to justify a line."))
;(tac~deftactic by-computation-a by-computation (in real)
;   (premises L1)
;   (conclusions L2)
;   (sideconditions (real=by-computation-p (formula L1) (formula L2)))
;   (description "Application of by-computation."))

(com~defcommand by-computation
  (argnames line1 line2)
  (argtypes ndline ndline-list)
  (arghelps "A line an arithmetic term to justify."
            "A list containing premises to be used.")
  (function real=by-computation)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help "Calls Maple to justify a line."))


(defun real=by-computation (l1 l2)
  (infer~compute-outline 'by-computation (list l1 l2) (list )))



(defun real=by-computation-p (prems conc)
  (let* ((assume (format nil "(assume ~{~A~})" (mapcar #'(lambda (formula) (string-downcase (post~print formula nil)))
						       prems)))
	 (is (format nil "(is ~A)"  (string-downcase (post~print conc nil))))
	 (answer (maple~call-maple (list "simplify" assume is) :syntax 'post2maple)))
    (maple~restart) ;forget the assume
    (when (string-equal answer "true") T)))


;; primefacs-product
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic expand-primefacs-product
		      (outline-mappings (((nonexistent list) expand-primefacs-product-f)))
		      (passkey :node)
		 ;     (parameter-types ndline-list)
		      (expansion-function real=expand-expand-primefacs-product)
		      (help "Tactic for method Expand-Primefacs-product-m."))

(defun expand-primefacs-product-f (conc prems param)
  (declare (ignore param conc))
  (multiple-value-bind (newconc newprems)
      (real=expand-primefacs-product-p (car prems))
    (when (and newconc
	       (subsetp newprems (rest prems))
	       (subsetp (rest prems) newprems ))
      (values (list newconc) ))))

(defun real=expand-primefacs-product-p (premline)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (num (env~lookup-object 'num env))
	 (prem (if (node~p premline) (node~formula premline) premline))
	 (a (term~variable-create 'a num))
	 (n (term~variable-create 'n num))
	 (b (term~variable-create 'b num))
	 (timesnb (term~appl-create (env~lookup-object 'times env)
				    (list n b)))
	 (subst (or (term~alpha-match (term~appl-create (env~lookup-object '= env)
							(list a timesnb))
				      prem)
		    (term~alpha-match (term~appl-create (env~lookup-object '= env)
							(list timesnb a))
				      prem))))
    (when (and subst (integerp (keim~name (subst~apply subst n))))
      (let ((sort-premsa
             (car (potac=wellsorted-check-and-return-theorems
                   (term~appl-create (env~lookup-object 'int env) (list (subst~apply subst a)))
                   (remove-duplicates (mapcan #'pds~node-supports (pds~open-nodes omega*current-proof-plan))))))
	    (sort-premsb
	     (car (potac=wellsorted-check-and-return-theorems
		   (term~appl-create (env~lookup-object 'int env) (list (subst~apply subst b)))
		   (remove-duplicates (mapcan #'pds~node-supports (pds~open-nodes omega*current-proof-plan)))))))
	(when (and sort-premsb sort-premsa)
		(multiple-value-bind
		    (success primes)
		    (methhelp=call-maple-to-get-prime-fac (subst~apply subst n))
		(when success
		  (values (batac=assemble-conjunction
			   (mapcar #'(lambda (prime)
				       (term~appl-create (env~lookup-object 'prime-divisor env)
							 (list (post~read-object prime env :existing-term)
							       (subst~apply subst a))))
				  primes))
			  (append sort-premsb sort-premsa)))))))))
	
(com~defcommand  expand-primefacs-product
  (argnames line1 )
  (argtypes ndline )
  (arghelps "A premise of the form a=n*b" )
  (function real=expand-primefacs-product)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help ""))


(defun real=expand-primefacs-product (l1)
  (multiple-value-bind (newconc newprems)
      (real=expand-primefacs-product-p l1)
    (infer~compute-outline 'expand-primefacs-product (list nil (cons l1 newprems)) nil)))



(defun  real=expand-expand-primefacs-product (conclusions premises parameters)
  (let* ((prem (car premises))
	 (sortprems (rest premises))
	 (primdiv-def (th~find-assumption "prime-divisor" (prob~theory omega*current-proof-plan)))
	 (primdiv-definiendum (th~definition-constant primdiv-def))
	 (primdiv-definiens (data~copy (th~ass-node primdiv-def) :downto '(term+constant type+primitive)))
	 (divisor-def (th~find-assumption "divisor" (prob~theory omega*current-proof-plan)))
	 (divisor-definiendum (th~definition-constant divisor-def))
	 (divisor-definiens (data~copy (th~ass-node divisor-def) :downto '(term+constant type+primitive)))
	 (env (pds~environment omega*current-proof-plan))
	 (num (env~lookup-object 'num env))
	 (a (term~variable-create 'a num))
	 (n (term~variable-create 'n num))
	 (b (term~variable-create 'b num))
	 (timesnb (term~appl-create (env~lookup-object 'times env)
				    (list n b)))
	 (subst (or (term~alpha-match (term~appl-create (env~lookup-object '= env)(list a timesnb)) (node~formula prem))
		    (term~alpha-match (term~appl-create (env~lookup-object '= env)(list timesnb a)) (node~formula prem))))
	 (well-b-form (term~appl-create (env~lookup-object 'int env)
					(list (subst~apply subst a))))
	 (well-b-args (potac=wellsorted-check-and-return-theorems well-b-form sortprems))
	 (well-b (pdsn~open-node-create  well-b-form
					 (mapcan #'pdsn~hyps (car well-b-args))
					 (pds~new-node-name omega*current-proof-plan))))
    (pds~insert-node! well-b omega*current-proof-plan)
    (tacl~init (append conclusions premises))
    (tacl~apply 'wellsorted (list well-b (car well-b-args)) (list (cadr well-b-args)))
    (let ((concs (if (logic~conjunction-p (node~formula (car conclusions)))
		     (second (tacl~apply 'andi* (list (car conclusions) nil) nil))
		   conclusions)))
      (mapc #'(lambda (conc)
		(let* ((andi* (tacl~sequence	   
			       (defni-primdiv ('defni (list conc nil) (list primdiv-definiendum primdiv-definiens (pos~add-front 0))))
			       (andi          ('andi  (list (second defni-primdiv) nil nil) nil))
			       (prime         ('prime (list (third andi)) nil))
			       (defni-divisor ('defni (list (second andi) nil) (list divisor-definiendum divisor-definiens (pos~add-front 0))))
			       (andi*         ('andi* (list (second defni-divisor) nil) nil))))
		       (well-num (first (second andi*)))
		       (well-num-args (potac=wellsorted-check-and-return-theorems well-num nil))
		       (number (term~appl-create
				(env~lookup-object 'times env)
				(list
				 (subst~apply subst b)
				 (post~read-object (/ (keim~name (subst~apply subst n))
						      (keim~name (car (data~appl-arguments (node~formula well-num)))))
						   env :existing-term))))
		       (exi (tacl~sequence	   
			     (well-n    ('wellsorted (list well-num nil) (list (cadr well-num-args))))
			     (weaken    ('hypweaken  (list (second (second andi*)) well-b) nil))
			     (exi       ('existsi-sort (list (third (second andi*)) nil nil)
						       (list number (list (pos~list-position '(2 2))))))))
		       (well-mul-args (potac=wellsorted-check-and-return-theorems (third exi) sortprems))
		       (well-mul (pdsn~open-node-create  (node~formula (third exi))
							 (mapcan #'pdsn~hyps (car well-mul-args))
							 (pds~new-node-name omega*current-proof-plan))))

		  (pds~insert-node! well-mul omega*current-proof-plan)
		  (tacl~sequence	   
		   (well-m    ('wellsorted (list well-mul (car well-mul-args)) (list (cadr well-mul-args))))
		   (hypweaken ('hypweaken  (list (third exi) well-mul) nil))
		   (compu     ('by-computation (list (second exi) (list prem)) nil)))))
	    concs))
    (tacl~end)))
	  


;(tacl~apply 'defni (list equiv nil) (list definiendum definiens (pos~add-front 0)))))























;; expand-collectdivs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic expand-collectdivs
		      (outline-mappings (((nonexistent list) expand-collectdivs-f)))
		      (parameter-types term)
		      (expansion-function real=expand-expand-collectdivs)
		      (help "Tactic for method collectdivs."))

(defun expand-collectdivs-f (conc prems param)
  (declare (ignore concs))
  (values (list (realtac=compute-collected-div prems (first param))) nil))

(defun realtac=compute-collected-div (assumps term)
  (multiple-value-bind
      (success divs used-assumps)
      (realtac=find-different-divisors assumps term)
    (if (null success)
	nil
      (let* ((composed-div (methhelp=connect-by-times divs))
	     (int-obj (env~lookup-object 'int (pds~environment omega*current-proof-plan)))
	     (times-obj (env~lookup-object 'times (pds~environment omega*current-proof-plan)))
	     (=-obj (env~lookup-object '= (pds~environment omega*current-proof-plan)))
	     (exists-sort-obj (env~lookup-object 'exists-sort (pds~environment omega*current-proof-plan)))
	     (new-var (term~variable-create 'x (post~read-object 'num (pds~environment omega*current-proof-plan) :existing-type)))) 
	(term~appl-create exists-sort-obj
			  (list (term~abstr-create (list new-var)
						   (term~appl-create =-obj
								     (list term (term~appl-create times-obj
												  (list composed-div new-var)))))
				int-obj))))))

(defun realtac=find-different-divisors (assumplist term)
  (let* ((divisor-item (env~lookup-object 'divisor (th~env 'rational)))
	 (prime-divisor-item (env~lookup-object 'prime-divisor (pds~environment omega*current-proof-plan)))
	 (divs-and-assumps (apply #'append (mapcar #'(lambda (assump)
						       (let* ((formula assump))
							 (if (and (data~appl-p formula)
								  (or (keim~equal (data~appl-function formula) divisor-item)
								      (keim~equal (data~appl-function formula) prime-divisor-item))
								  (= (length (data~appl-arguments formula)) 2))
							     (let* ((arg1 (first (data~appl-arguments formula)))
								    (arg2 (second (data~appl-arguments formula))))
							       (if (keim~equal arg2 term)
								   (list (list arg1 assump))
								 nil))
							   nil)))
						   assumplist)))
	 (divs-and-assumps-clear (remove-duplicates divs-and-assumps :test #'(lambda (it1 it2)
									       (keim~equal (first it1) (first it2))))))
    (if divs-and-assumps
	(values 't
		(mapcar #'first divs-and-assumps-clear)
		(mapcar #'second divs-and-assumps-clear))
      (values nil nil nil))))


(com~defcommand expand-collectdivs
  (argnames primdivslist termi)
  (argtypes ndline-list term )
  (arghelps "A list of assumptions of the form (prime-divisor x t)" "The term t")
  (function real=expand-collectdivs)
  (frag-cats tactics real)
  (defaults)
  (log-p t)
  (help ""))

(defun real=expand-collectdivs (listi termi)
  (infer~compute-outline 'expand-collectdivs (list nil listi) (list termi)))

(defun real=expand-expand-collectdivs (conclusions premises parameters)
  )




