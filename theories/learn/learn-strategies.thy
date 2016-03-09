(in-package :omega)

(setf one-simplify-r  '(  (star  ASSOC-R-M-B )
		       (disj  INVL-I-M-B  INVR-I-M-B )
		       IDL-I-M-B  ))

(lea~produce-method one-simplify-r  :method 'one-simplify-r-m-b :inference 'one-simplify-r-m :theory 'learn)

(setf one-simplify-l '( (star  ASSOC-L-M-B )
		       (disj  INVL-I-M-B  INVR-I-M-B )
		       IDR-I-M-B  ))

(lea~produce-method one-simplify-l :method 'one-simplify-l-m-b :inference 'one-simplify-l-m :theory 'learn)

(setf rec-simplify-r  '( (star-max  ONE-SIMPLIFY-r-M-B )))

(lea~produce-method rec-simplify-r  :method 'rec-simplify-r-m-b :inference 'rec-simplify-r-m :theory 'learn)

(setf rec-simplify-l '( (star-max  ONE-SIMPLIFY-l-M-B )))

(lea~produce-method rec-simplify-l :method 'rec-simplify-l-m-b :inference 'rec-simplify-l-m :theory 'learn)


;(setf rec-simplify-complete '(  (FORALLI*-M IMPI-M-B)  (star  FORALLI-SORT-M-B ) (star  ONE-SIMPLIFY-M-B )  REFLEX-M-B  ))
;(lea~produce-method rec-simplify-complete :method 'rec-simplify-complete-m-b :inference 'rec-simplify-complete-m :theory 'learn)

(cri~def-control-rule learned-first
		      (kind methods)
		      (if (always-true))
		      (then
		       (prefer (;rec-simplify-complete-m-b
				rec-simplify-r-m-b	
				rec-simplify-l-m-b	
				;one-simplify-m-b
				))))


(strat~define-strategy-ks
 (name grp-simplify)
 (refinement-algorithm PPlanner)
 (condition tryassocloop?)
 (methods ( ;rec-simplify-complete-m-b
	    foralli*-m
	    ImpI-m-b
	    foralli-sort-m-b
	    rec-simplify-r-m-b
	    rec-simplify-l-m-b
	    reflex-m-b
	    
	   invr-i-m-b
	   invr-i-m-f
	   invr-i-m-a

	   invl-i-m-b
	   invl-i-m-f
	   invl-i-m-a

	   idr-i-m-b
	   idr-i-m-f
	   idr-i-m-a

	   idl-i-m-b
	   idl-i-m-f
	   idl-i-m-a


	   idr-e-m-b
	   idr-e-m-f
	   idr-e-m-a

	   idl-e-m-b
	   idl-e-m-f
	   idl-e-m-a

	   invr-e-m-b
	   invr-e-m-f
	   invr-e-m-a

	   invl-e-m-b
	   invl-e-m-f
	   invl-e-m-a

	   assoc-l-m-b
	   assoc-l-m-a
	   assoc-l-m-f

	   assoc-r-m-b
	   assoc-r-m-a
	   assoc-r-m-f

	   truei-m-b	
	   ))
 (normalization-methods )
 (restriction-methods (	   truei-m-b
			   ;foralli*-m
			   ImpI-m-b
			   ;foralli-sort-m-b
			   reflex-m-b
		       ))
 (control-rules  (assoc-l-b-pos assoc-r-b-pos invr-pos invl-pos idl-pos idr-pos learned-first))
 (loop-detection 5)
 (randomization-rules nil)
 (selection waterfall)
 (termination-check no-further-goal-p)
 (print "MJ-strat grp-simplify"))

(defun tryassocloop? (forget) T) ;try the strategy for every proof situation
(defun no-further-goal-p () nil) ;never stop, MULTI stops when there are no open goals

(strat~define-strategy-ks
 (name grp-strat)
 (refinement-algorithm PPlanner)
 (condition tryassocloop?)
 (methods ( ;rec-simplify-complete-m-b
	    foralli*-m
	    ImpI-m-b
	    foralli-sort-m-b
	    ;rec-simplify-m-b
	    ;one-simplify-l-m-b		
	    ;one-simplify-m-b
	    reflex-m-b
	    
	   invr-i-m-b
	   invr-i-m-f
	   invr-i-m-a

	   invl-i-m-b
	   invl-i-m-f
	   invl-i-m-a

	   idr-i-m-b
	   idr-i-m-f
	   idr-i-m-a

	   idl-i-m-b
	   idl-i-m-f
	   idl-i-m-a


	   idr-e-m-b
	   idr-e-m-f
	   idr-e-m-a

	   idl-e-m-b
	   idl-e-m-f
	   idl-e-m-a

	   invr-e-m-b
	   invr-e-m-f
	   invr-e-m-a

	   invl-e-m-b
	   invl-e-m-f
	   invl-e-m-a

	   assoc-l-m-b
	   assoc-l-m-a
	   assoc-l-m-f

	   assoc-r-m-b
	   assoc-r-m-a
	   assoc-r-m-f

	   ;truei-m-b	
	   ))
 (normalization-methods )
 (restriction-methods (	   ;truei-m-b
			   ;foralli*-m
			   ImpI-m-b
			   ;foralli-sort-m-b
			   reflex-m-b
		       ))
 (control-rules  (assoc-l-b-pos assoc-r-b-pos invr-pos invl-pos idl-pos idr-pos)) ;learned-first))
 (loop-detection 5)
 (randomization-rules nil)
 (selection waterfall)
 (termination-check no-further-goal-p)
 (print "MJ-strat"))

(setf sod*current-strategies '(grp-simplify))


