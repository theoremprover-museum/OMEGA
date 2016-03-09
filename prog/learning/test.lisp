;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; EXAMPLE 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun func1 (formula parameters)
  (declare (ignore formula))
  (car parameters))

(defun func2 (formula parameters)
  (declare (ignore formula))
  (pos~butlast (car parameters)))

'(& (assoc-l func1)
    (assoc-r func2))

(learn~create-tactic+method '(& (assoc-l func1)
				(assoc-r func2))
			    '((position pos+position pos))
			    'my-group
			    :b
			    :tac-name 'simtac
			    :meth-name 'simmeth
			    )

'(& (* (assoc-r func1) tfunc1)
    (&
     (^ (invr-i func2) (invl-i func2))
     (idl-i func3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; EXAMPLE 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; (this was learned from test-ex1 and ;;;;;;;;;;
;;;;;;;;  test-ex2 in the my-group problems) ;;;;;;;;;;
;;;;;;;; (the functions are written by hand) ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun func1 (formula parameters)
  (declare (ignore formula))
  (pos~add-end 1 (car parameters)))


(defun func2 (formula parameters)
  (declare (ignore formula))
  (pos~add-end 1 (car parameters))) 

(defun func3 (formula parameters)
  (declare (ignore formula))
  (pos~butlast (car parameters)))

(defun tfunc1 (formula parameters)
  (my-grouptac=compute-invr-i-b-p formula (car parameters)))
    
(learn~compile-learned-stuff
 (learn~create-tactic+method '(& (* (assoc-r func1) tfunc1)
				 (&
				  (^ (invr-i func2) (invl-i func2))
				  (idl-i func3)))
			     '((position pos+position pos))
			     'my-group
			     :b
			     :tac-name 'simtac
			     :meth-name 'simmeth
			     ))


