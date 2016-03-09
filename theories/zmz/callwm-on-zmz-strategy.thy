(in-package :omega)

(defvar strathelp*waldmeister-others-part1
"  
NAME            ZX

MODE            PROOF

SORTS           DOM 
                BOOL
                SET

SIGNATURE       s: DOM -> DOM
                p: DOM -> DOM
                0: -> DOM
")

(defvar strathelp*waldmeister-others-part2
"
                +: DOM DOM -> DOM
                *: DOM DOM -> DOM
                -: DOM -> DOM
                op: DOM DOM -> DOM

                equal: DOM DOM -> BOOL
                true, false: -> BOOL
                if_then_else: BOOL BOOL BOOL -> BOOL
                and: BOOL BOOL -> BOOL
                or: BOOL BOOL -> BOOL

                _: DOM SET -> SET
                nil: -> SET
                is_elem: DOM SET -> BOOL

                ZX: -> SET

                closed: SET -> BOOL
                closed0: SET SET -> BOOL
                closed1: SET DOM SET -> BOOL
                closed2: SET DOM DOM -> BOOL
                assoc: SET -> BOOL
                assoc0: SET SET -> BOOL
                assoc1: SET DOM SET -> BOOL
                assoc2: DOM DOM SET -> BOOL
                assoc3: DOM DOM DOM -> BOOL
                unit: SET -> BOOL
                unit0: SET SET -> BOOL
                unit1: DOM SET -> BOOL
                unit2: DOM DOM -> BOOL
                inv: SET -> BOOL
                inv0: SET SET -> BOOL
                inv1: DOM SET -> BOOL
                inv2: DOM DOM -> BOOL
                comm: SET -> BOOL
                comm0: SET SET -> BOOL
                comm1: DOM SET -> BOOL
                comm2: DOM DOM -> BOOL
                minu: SET -> BOOL
                minu0: SET SET -> BOOL
                minu1: SET DOM SET -> BOOL
                minu2: SET DOM DOM SET -> BOOL
                minu3: DOM DOM DOM SET -> BOOL
                minu4: DOM DOM DOM DOM -> BOOL

ORDERING        LPO
                minu > minu0 > minu1 > minu2 > minu3 > minu4
                > comm > comm0 > comm1 > comm2 
                > inv > inv0 > inv1 > inv2 
                > unit > unit0 > unit1 > unit2 
                > assoc > assoc0 > assoc1 > assoc2 > assoc3
                > closed > closed0 > closed1 > closed2 
                > ZX > _ > nil
")

(defvar strathelp*waldmeister-others-part3
"
                > is_elem > if_then_else > and > or > equal > true > false
                
VARIABLES       u,v,x,y,z: DOM
                P, Q: BOOL
                S, T: SET

EQUATIONS       % 0. Model Boolean operations and sets
                if_then_else(true,P,Q) = P
                if_then_else(false,P,Q) = Q
                and(true,Q) = Q
                and(false,Q) = false
                and(P,true) = P
                and(P,false) = false
                or(true,Q) = true
                or(false,Q) = Q
                or(P,true) = true
                or(P,false) = P
                is_elem(x,nil) = false
                is_elem(x,_(y,S)) = if_then_else(equal(x,y),true,is_elem(x,S))
")

(defvar strathelp*waldmeister-others-part4
"

                % 2. Symmetrically define addition
                +(x,0) = x
                +(x,s(y)) = s(+(x,y))
                +(x,p(y)) = p(+(x,y))
                +(0,y) = y
                +(s(x),y) = s(+(x,y))
                +(p(x),y) = p(+(x,y))

                % 4. Define inverse
                -(0) = 0
                -(p(x)) = s(-(x))
                -(s(x)) = p(-(x))

                % 5. Symmetrically define multiplication
                *(x,0) = 0
                *(x,s(y)) = +(x,*(x,y))
                *(x,p(y)) = +(-(x),*(x,y))
                *(0,y) = 0
                *(s(x),y) = +(*(x,y),y)
                *(p(x),y) = +(*(x,y),-(y))

                % 6. Add some further ring properties
                +(+(x,y),z) = +(x,+(y,z))
                +(x,y) = +(y,x)
                +(x,-(x)) = 0
                +(-(x),x) = 0
                -(-(x)) = x
                +(-(x),+(x,y)) = y
                +(x,+(-(x),y)) = y
                -(+(x,y)) = +(-(x),-(y))
                *(*(x,y),z) = *(x,*(y,z))
                *(x,+(y,z)) = +(*(x,y),*(x,z))
                *(+(x,y),z) = +(*(x,z),*(y,z))
")               

(defvar strathelp*waldmeister-spec-closure
"                
                % 4.1 Predicates to express closure
                closed2(S,x,y) = is_elem(op(x,y),S)
                closed1(S,x,nil) = true
                closed1(S,x,_(y,T)) = and(closed2(S,x,y),closed1(S,x,T))
                closed0(S,nil) = true
                closed0(S,_(x,T)) = and(closed1(S,x,T),closed0(S,T))
                closed(S) = closed0(S,S)
")

(defvar strathelp*waldmeister-spec-associativity
"                
                % 4.2 Predicates to express associativity
                assoc3(x,y,z) = equal(op(op(x,y),z),op(x,op(y,z)))
                assoc2(x,y,nil) = true
                assoc2(x,y,_(z,T)) = and(assoc3(x,y,z),assoc2(x,y,T))
                assoc1(S,x,nil) = true
                assoc1(S,x,_(y,T)) = and(assoc2(x,y,S),assoc1(S,x,T))
                assoc0(S,nil) = true
                assoc0(S,_(x,T)) = and(assoc1(S,x,T),assoc0(S,T))
                assoc(S) = assoc0(S,S)
")

(defvar strathelp*waldmeister-spec-commutativity
"
                % 4.5 Predicates to express commutativity
                comm2(x,y) = equal(op(x,y),op(y,x))
                comm1(x,nil) = true
                comm1(x,_(y,T)) = and(comm2(x,y),comm1(x,T))
                comm0(S,nil) = true
                comm0(S,_(x,T)) = and(comm1(x,S),comm0(S,T))
                comm(S) = comm0(S,S)
")

(defvar strathelp*waldmeister-spec-unit
"
                % 4.3 Predicates to express existence of unit
                unit2(x,y) = and(equal(op(x,y),y),equal(op(y,x),y))
                unit1(x,nil) = true
                unit1(x,_(y,T)) = and(unit2(x,y),unit1(x,T))
                unit0(S,nil) = false
                unit0(S,_(x,T)) = or(unit1(x,S),unit0(S,T))
                unit(S) = unit0(S,S)
")                

(defun strathelp*waldmeister-spec-inverse (unit-string)
  (concatenate 'string 
"                
                % 4.4 Predicates to express existence of inverse
                inv2(x,y) = and(equal(op(x,y)," unit-string "),equal(op(y,x)," unit-string "))
                inv1(x,nil) = false
                inv1(x,_(y,T)) = or(inv2(x,y),inv1(x,T))
                inv0(S,nil) = true
                inv0(S,_(x,T)) = and(inv1(x,S),inv0(S,T))
                inv(S) = inv0(S,S)
"
		))

(defvar strathelp*waldmeister-spec-divisors
"
                % 4.6 Predicates to express existence of minuends
                minu4(x,y,u,v) = and(equal(op(x,u),y),equal(op(v,x),y))
                minu3(x,y,u,nil) = false
                minu3(x,y,u,_(v,T)) = or(minu4(x,y,u,v),minu3(x,y,u,T))
                minu2(S,x,y,nil) = false
                minu2(S,x,y,_(u,T)) = or(minu3(x,y,u,S),minu2(S,x,y,T))
                minu1(S,x,nil) = true
                minu1(S,x,_(y,T)) = and(minu2(S,x,y,S),minu1(S,x,T))
                minu0(S,nil) = true
                minu0(S,_(x,T)) = and(minu1(S,x,S),minu0(S,T))
                minu(S) = minu0(S,S)
")




(defvar strathelp*waldmeister-iso-part1
"
NAME            WPROB

MODE            PROOF

SORTS           DOM 

SIGNATURE       0: -> DOM
                s,p: DOM -> DOM
                -: DOM -> DOM
                +,*: DOM DOM -> DOM

                op1,op2: DOM DOM -> DOM
                h, j: DOM -> DOM

ORDERING        LPO
                h > j > op2 > op1 > * > - > + > p > s > 0
                
VARIABLES       x,y,z: DOM

EQUATIONS       % 1. Model integers 
                s(p(x)) = x
                p(s(x)) = x

                % 2. Model restclasses")

(defvar strathelp*waldmeister-iso-part2
"

                % 3. Symmetrically define addition
                +(x,0) = x
                +(x,s(y)) = s(+(x,y))
                +(x,p(y)) = p(+(x,y))
                +(0,y) = y
                +(s(x),y) = s(+(x,y))
                +(p(x),y) = p(+(x,y))

                % 4. Define inverse
                -(0) = 0
                -(p(x)) = s(-(x))
                -(s(x)) = p(-(x))

                % 5. Symmetrically define multiplication
                *(x,0) = 0
                *(x,s(y)) = +(x,*(x,y))
                *(x,p(y)) = +(-(x),*(x,y))
                *(0,y) = 0
                *(s(x),y) = +(*(x,y),y)
                *(p(x),y) = +(*(x,y),-(y))

                % 6. Add some further ring properties
                +(+(x,y),z) = +(x,+(y,z))
                +(x,y) = +(y,x)
                +(x,-(x)) = 0
                +(-(x),x) = 0
                -(-(x)) = x
                +(-(x),+(x,y)) = y
                +(x,+(-(x),y)) = y
                -(+(x,y)) = +(-(x),-(y))
                *(*(x,y),z) = *(x,*(y,z))
                *(x,+(y,z)) = +(*(x,y),*(x,z))
                *(+(x,y),z) = +(*(x,z),*(y,z))
               
                % 7. Define operations to be studied")

(defvar strathelp*waldmeister-iso-part3
"
                % 8. Claim isomorphism
                h(op1(x,y)) = op2(h(x),h(y))
                j(op2(x,y)) = op1(j(x),j(y))
                h(j(x)) = x
                j(h(x)) = x

CONCLUSION      % 9. Derive that then constants are identified")







#| ------------------------------------------------- The CallWMOnRsidueCLassProblems Strategy --------------------------- |#




;; The strategy is applicable on all residue Class Problems Except Isomorphism-Problems
(defun residueclass-task-for-wm-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (if (or (and (close-goal-p task)  ;; applies the check function of TRYANDERROR Strategy
		   (null (strathelp=isomorphism-problem-p (node~formula (agenda~task-node task)))))
	      (strathelp=not-isomorphism-problem-p (node~formula (agenda~task-node task))))
	  't
	nil)
    nil))

(defun strathelp=not-isomorphism-problem-p (formula)
  (and (logic~negation-p formula)
       (data~appl-p (first (data~appl-arguments formula)))
       (keim~equal (data~appl-function (first (data~appl-arguments formula)))
		   (data~schema-range (env~lookup-object 'isomorphic (th~env 'zmz))))))

(defun strathelp=isomorphism-problem-p (formula)
  (and (data~appl-p formula)
       (keim~equal (data~appl-function formula)
		   (data~schema-range (env~lookup-object 'isomorphic (th~env 'zmz))))))

(strat~define-strategy-ks
 (name WMOnResidueClass)
 (refinement-algorithm ATP)
 (condition residueclass-task-for-wm-p)
 (apply-atp-function call-wm-on-residue-class-problem)
 (check-atp-out wm-success-p)
 (parameter-types )
 (print "Strategy-KS WMOnResidueClass: Offer to call Waldmeister on residue class problem ~A"))

(defun wm-success-p (wm-out)
  (if (or (null wm-out)
	  (null (listp wm-out)))
      nil
    (multiple-value-bind
	(proof-flag statistics proved-goal)
	(strathelp=check-for-proof wm-out)
      (if proof-flag
	  't
	nil))))


(defun strathelp=check-for-proof (out-strings)
  (let* ((line-strings (remove-if #'(lambda (stringi)
				      (string= stringi ""))
				  out-strings)))
    (or (find "        |   this proves the goal   |" line-strings :test #'string=)
	(find "Waldmeister states: Goal proved." line-strings :test #'string=)
	(find "Waldmeister states: One goal proved." line-strings :test #'string=))))




(defun call-wm-on-residue-class-problem (task)
  (let* ((time-ressource 10)
	 (node (agenda~task-node task))
	 (formula (node~formula node))
	 (out-dir "/tmp/"))
    (if (and (logic~negation-p formula)
	     (data~appl-p (first (data~appl-arguments formula)))
	     (keim~equal (data~schema-range (env~lookup-object 'isomorphic (pds~environment omega*current-proof-plan)))
			 (data~appl-function (first (data~appl-arguments formula)))))
	;; not-isomorphic problem
	(strathelp=test-iso-problem node time-ressource out-dir)
      ;; other problem 
      (strathelp=test-other-problem node time-ressource out-dir))))






#| ------------------------------------------------------- Auxiliary Functions ----------------------------------------------------- |#





;; Waldmeister on problems concerning isomorphy 
(defun strathelp=test-iso-problem (node time-ressource out-dir)  
  (let* ((complete-input-string (strathelp=create-wm-iso-input-file node))
	 (in-file (merge-pathnames (format nil "waldi.in") out-dir))
	 (temp-out-file (merge-pathnames (format nil "temp.out") out-dir))
	 (out-file (merge-pathnames (format nil "waldi.out") out-dir))
	 (inter? atptop*interactivity-allowed))
    
    (atptop~print-string-in-file complete-input-string in-file)
    (setq atptop*interactivity-allowed nil)
    
    (let* ((call-flag (atptop~call-with-time-ressource (format nil "~A --auto --noproof ~A --expert -zb stop -a 0 >! ~A;mv ~A ~A &"
		                                       ;;(format nil "~A --noproof ~A --expert -zb stop -a 0 >! ~A;mv ~A ~A &"
		                                       ;;(format nil "~A --noproof ~A --expert -gj -zb stop -a 0 >! ~A;mv ~A ~A &"
							       (wald~program)
							       in-file
							       temp-out-file 
							       temp-out-file
							       out-file)
						       out-file
						       "WALDMEISTER"
						       out-dir
						       time-ressource)))

      (setq atptop*interactivity-allowed inter?)
      
      (if (null call-flag)
	  ;; WM Failed to prove the problem in the given time ressource
	  nil
	(strathelp=read-file-as-strings out-file)))))

;; Waldmeister on problems not concerning isomorphy 
(defun strathelp=test-other-problem (node time-ressource out-dir)
  (let* ((complete-input-string (strathelp=create-wm-other-problem-input-file node))
	 (in-file (merge-pathnames (format nil "waldi.in") out-dir))
	 (temp-out-file (merge-pathnames (format nil "temp.out") out-dir))
	 (out-file (merge-pathnames (format nil "waldi.out") out-dir))
	 (inter? atptop*interactivity-allowed))
    
    (atptop~print-string-in-file complete-input-string in-file)
    (setq atptop*interactivity-allowed nil)
      
    (let* ((call-flag (atptop~call-with-time-ressource ;; (format nil "~A --auto --noproof ~A --expert -zb stop -a 0 >! ~A;mv ~A ~A &"
		       (format nil "~A --noproof ~A --expert -zb stop -a 0 >! ~A;mv ~A ~A &"
			       ;;(format nil "~A --noproof ~A --expert -gj -zb stop -a 0 >! ~A;mv ~A ~A &"
			       (wald~program)
			       in-file
			       temp-out-file 
			       temp-out-file
			       out-file)
		       out-file
		       "WALDMEISTER"
		       out-dir
		       time-ressource)))

      (setq atptop*interactivity-allowed inter?)
      
      (if (null call-flag)
	  ;; WM Failed to prove the problem in the given time ressource
	  nil
	(strathelp=read-file-as-strings out-file)))))





(defun strathelp=create-wm-iso-input-file (node)
  (let* ((not-formula (node~formula node))
	 (iso-formula (first (data~appl-arguments not-formula)))
	 (args (data~appl-arguments iso-formula))
	 (op1 (second args))
	 (op2 (fourth args))
	 (set1 (first args))
	 (mod-number (keim~name (zmztac=class-factor set1)))
	 ;; wir gehen hier davon aus, dass beide Strukturen gemaess der gleichen MOD-NUMBER SIND
	 ;; sonst muessten wir naemlich im WM Eingabe File alles ganz anders kodieren!
	 (succ-string (strathelp=get-mod-string mod-number "s"))
	 (pred-string (strathelp=get-mod-string mod-number "p"))
	 (op1-string (strathelp=get-convert-operation-string op1 "op1"))
	 (op2-string (strathelp=get-convert-operation-string op2 "op2"))
	 (all-the-goals-string (strathelp=get-all-goal-strings mod-number)))
    (values (format nil "~A~%~A~%~A~A~%~A~%~A~%~A~A"
		    strathelp*waldmeister-iso-part1
		    succ-string
		    pred-string
		    strathelp*waldmeister-iso-part2
		    op1-string
		    op2-string
		    strathelp*waldmeister-iso-part3
		    all-the-goals-string)
	    mod-number)))


(defun strathelp=get-mod-string (number letter)
  (let* ((real-string (strathelp=recursive-number number letter "x")))
    (format nil "~A~A = x" "                " real-string)))

(defun strathelp=get-convert-operation-string (op letter)
  (let* ((converted-operation-string (strathelp=convert-operation op)))
    (format nil "                ~A(x,y) = ~A" letter converted-operation-string)))

(defun strathelp=get-all-goal-strings (mod-number)
  (let* ((current-string ""))
    (do* ((counter1 0 (incf counter1)))
	((= counter1 mod-number)
	 current-string)
      (do* ((counter2 (+ counter1 1) (incf counter2)))
	  ((= counter2 mod-number)
	   nil)
	(setf current-string (format nil "~A~%                ~A = ~A"
				     current-string
				     (strathelp=recursive-number counter1 "s" "0")
				     (strathelp=recursive-number counter2 "s" "0")
				     ))))))




(defun strathelp=create-wm-other-problem-input-file (node)
  (let* ((formula (node~formula node)))
    
    (multiple-value-bind
	(signifier set operation unit)
	(strathelp=get-information-from-formula formula)
      
      (let* ((mod-number (zmztac=class-factor set))
	     (vars (strathelp=variables-from-set set))
	     (vars-decl-string (strathelp=vars-decl-string vars))
	     (vars-order-string (strathelp=vars-order-string vars))
	     (vars-succ-string (strathelp=vars-as-successors vars))
	     (model-set-string (strathelp=model-set-string set))
	     (def-set-string (strathelp=set-string vars))
	     (operation-string (strathelp=operation-string operation))
	     (kind-string (strathelp=kind-string signifier unit))
	     (conclusion-string (strathelp=conclusion-string signifier)))
	
	(values (format nil "~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A"
			strathelp*waldmeister-others-part1
			vars-decl-string
			strathelp*waldmeister-others-part2
			vars-order-string
			strathelp*waldmeister-others-part3
			vars-succ-string
			model-set-string
			def-set-string
			strathelp*waldmeister-others-part4
			operation-string
			kind-string
			conclusion-string)
		signifier
		mod-number)))))


(defun strathelp=get-information-from-formula (formula)
  (let* ((env (pds~environment omega*current-proof-plan)))
    
    (if (logic~negation-p formula)

	(multiple-value-bind
	    (signifier set operation unit)
	    (strathelp=get-information-from-formula (first (data~appl-arguments formula)))

	  (values (cond ((equal signifier 'unit) 'not-unit)
			((equal signifier 'closed) 'not-closed)
			((equal signifier 'associative) 'not-associative)
			((equal signifier 'commutative) 'not-commutative)
			((equal signifier 'inverse) 'not-inverse)
			((equal signifier 'divisors) 'not-divisors))
		  set
		  operation
		  unit))
      
      (if (logic~existential-quantification-p formula)
	  ;; -> formula is exists-unit
	  (let* ((range (data~abstr-range (first (data~appl-arguments formula)))))
	    (values 'unit
		    (first (data~appl-arguments range))
		    (second (data~appl-arguments range))
		    nil))
	(let* ((function (data~appl-function formula)))
	  (cond ((keim~equal function (data~schema-range (env~lookup-object 'closed-under env)))
		 (values 'closed
			 (first (data~appl-arguments formula))
			 (second (data~appl-arguments formula))
			 nil))
		((keim~equal function (data~schema-range (env~lookup-object 'associative env)))
		 (values 'associative
			 (first (data~appl-arguments formula))
			 (second (data~appl-arguments formula))
			 nil))
		((keim~equal function (data~schema-range (env~lookup-object 'commutative env)))
		 (values 'commutative
			 (first (data~appl-arguments formula))
			 (second (data~appl-arguments formula))
			 nil))
		((keim~equal function (data~schema-range (env~lookup-object 'inverse-exist env)))
		 (values 'inverse
			 (first (data~appl-arguments formula))
			 (second (data~appl-arguments formula))
			 (third (data~appl-arguments formula))))
		((keim~equal function (data~schema-range (env~lookup-object 'divisors-exist env)))
		 (values 'divisors
			 (first (data~appl-arguments formula))
			 (second (data~appl-arguments formula))
			 nil))))))))
		

(defun strathelp=kind-string (signifier unit)
  (cond ((or (equal signifier 'closed)
	     (equal signifier 'not-closed))
	 strathelp*waldmeister-spec-closure)
	((or (equal signifier 'associative)
	     (equal signifier 'not-associative))
	 strathelp*waldmeister-spec-associativity)
	((or (equal signifier 'commutative)
	     (equal signifier 'not-commutative))
	 strathelp*waldmeister-spec-commutativity)
	((or (equal signifier 'unit)
	     (equal signifier 'not-unit))
	 strathelp*waldmeister-spec-unit)
	((or (equal signifier 'inverse)
	     (equal signifier 'not-inverse))
	 (let* ((unit-number (keim~name (second (data~appl-arguments unit))))
		(unit-string (string-downcase (strathelp=recursive-number unit-number 's '0))))
	   (strathelp*waldmeister-spec-inverse unit-string)))
	((or (equal signifier 'divisors)
	     (equal signifier 'not-divisors))
	 strathelp*waldmeister-spec-divisors)
	))


(defun strathelp=operation-string (operation)
  (format nil "                op(x,y) = ~A" (strathelp=convert-operation operation)))
	     
(defun strathelp=conclusion-string (signifier)
  (cond ((equal signifier 'closed)
	 "CONCLUSION      closed(ZX) = true")
	((equal signifier 'associative)
	 "CONCLUSION      assoc(ZX) = true")
	((equal signifier 'commutative)
	 "CONCLUSION      comm(ZX) = true")
	((equal signifier 'unit)
	 "CONCLUSION      unit(ZX) = true")
	((equal signifier 'inverse)
	 "CONCLUSION      inv(ZX) = true")
	((equal signifier 'divisors)
	 "CONCLUSION      minu(ZX) = true")
	((equal signifier 'not-closed)
	 "CONCLUSION      closed(ZX) = false")
	((equal signifier 'not-associative)
	 "CONCLUSION      assoc(ZX) = false")
	((equal signifier 'not-commutative)
	 "CONCLUSION      comm(ZX) = false")
	((equal signifier 'not-unit)
	 "CONCLUSION      unit(ZX) = false")
	((equal signifier 'not-inverse)
	 "CONCLUSION      inv(ZX) = false")
	((equal signifier 'not-divisors)
	 "CONCLUSION      minu(ZX) = false")))

	     
(defun strathelp=model-set-string (set)
  (let* ((class-factor (keim~name (zmztac=class-factor set)))
	 (back1 (do* ((counter 1 (incf counter))
		      (back-string (format nil "~A~%~A"
					   "                % MODEL SET"
					   "                 equal(x,x) = true")))
		    ((= counter class-factor)
		     back-string)
		  (setf back-string (format nil "~A~%                 equal(x,~A) = false"
					    back-string
					    (string-downcase (strathelp=recursive-number counter 's 'x))))
		  (setf back-string (format nil "~A~%                 equal(x,~A) = false"
					    back-string
					    (string-downcase (strathelp=recursive-number counter 'p 'x)))))))
	 
    (setf back1 (format nil "~A~%                 ~A = x" back1 (string-downcase (strathelp=recursive-number class-factor 's 'x))))
    (setf back1 (format nil "~A~%                 ~A = x" back1 (string-downcase (strathelp=recursive-number class-factor 'p 'x))))
    back1))
	     
(defun strathelp=variables-from-set (set)
  (let* ((class-factor (keim~name (zmztac=class-factor set))))
    (do* ((counter 0 (incf counter))
	  (vars nil))
	((= counter class-factor)
	 vars)
      (setf vars (append vars (list (make-symbol (format nil "a~A" counter))))))))
	
(defun strathelp=vars-decl-string (vars)
  (do* ((rest-vars vars (rest rest-vars))
	(back-string nil))
      ((null rest-vars)
       (format nil "~A: -> DOM" back-string))
    (let* ((head-var (first rest-vars)))
      (if (null back-string)
	  (setf back-string (format nil "                ~A" head-var))
	(setf back-string (format nil "~A,~A" back-string head-var))))))

(defun strathelp=vars-order-string (vars)
  (do* ((rest-vars (reverse vars) (rest rest-vars))
	(back-string nil))
      ((null rest-vars)
       back-string)
    (let* ((head-var (first rest-vars)))
      (if (null back-string)
	  (setf back-string (format nil "                > op > * > - > + > p > s > 0 > ~A" head-var))
	(setf back-string (format nil "~A > ~A" back-string head-var))))))

(defun strathelp=vars-as-successors (vars)
  (do* ((rest-vars vars (rest rest-vars))
	(last nil)
	(back-string nil))
      ((null rest-vars)
       back-string)
    (let* ((head-var (first rest-vars)))
      (if (null back-string)
	  (setf back-string (format nil "                ~A = 0" head-var))
	(setf back-string (format nil "~A~%                 ~A = s(~A)" back-string head-var last)))
      (setf last head-var))))

(defun strathelp=set-string (vars)
  (do* ((rest-vars vars (rest rest-vars))
	(front-string nil)
	(end-string ""))      
      ((null rest-vars)
       (format nil "~A,nil~A" front-string end-string))
    (let* ((head-var (first rest-vars)))
      (if (null front-string)
	  (setf front-string (format nil "                ZX = _(~A" head-var))
	(setf front-string (format nil "~A,_(~A" front-string head-var)))
      (setf end-string (format nil "~A)" end-string)))))
      
(defun strathelp=recursive-number (number letter inner)
  (if (= number 0)
      (if (stringp inner)
	  inner
	(format nil "~A" inner))
    (do* ((current-string "")
	  (rest-number number (decf rest-number)))
	((= rest-number 0)
	 current-string)
      (if (string= current-string "")
	  (setf current-string (format nil "~A(~A)" letter inner))
	(setf current-string (format nil "~A(~A)" letter current-string))))))
       
		       

(defun strathelp=convert-operation (op)
  (let* ((env (th~env 'zmz))
	 (plusrcl (env~lookup-object 'plus-resclass env))
	 (minusrcl (env~lookup-object 'minus-resclass env))
	 (timesrcl (env~lookup-object 'times-resclass env)))
    (cond ((data~equal op plusrcl)
	   "+(x,y)")
	  ((data~equal op timesrcl)
	   "*(x,y)")
	  ((data~equal op minusrcl)
	   "+(x,-(y))")
	  ((data~abstr-p op)
	   (strathelp=convert-operation-recursive (data~abstr-range op)))
	  (t
	   (omega~error "~%Not implemented case in function strathelp=convert-operation.")))))

(defun strathelp=convert-operation-recursive (op)
  (let* ((env (th~env 'zmz))
	 (plusrcl (env~lookup-object 'plus-resclass env))
	 (minusrcl (env~lookup-object 'minus-resclass env))
	 (timesrcl (env~lookup-object 'times-resclass env))
	 (resclop (env~lookup-object 'resclass env)))
    (cond ((and (data~appl-p op)
		(or (data~equal (data~appl-function op) plusrcl)
		    (data~equal (data~appl-function op) timesrcl)
		    (data~equal (data~appl-function op) minusrcl)))		    
	   (let* ((func (data~appl-function op))
		  (args (data~appl-arguments op))
		  (converted-arg1 (strathelp=convert-operation-recursive (first args)))
		  (converted-arg2 (strathelp=convert-operation-recursive (second args))))
	     (cond ((data~equal func plusrcl)
		    (format nil "+(~A,~A)" converted-arg1 converted-arg2))
		   ((data~equal func timesrcl)
		    (format nil "*(~A,~A)" converted-arg1 converted-arg2))
		   ((data~equal func minusrcl)
		    (format nil "+(~A,-(~A))" converted-arg1 converted-arg2))
		   (t
		    (omega~error "~%Not implemented case in function strathelp=convert-operation-recursive.")))))
	  ((and (data~appl-p op)
		(data~equal (data~appl-function op) resclop))
	   (let* ((number (keim~name (second (data~appl-arguments op)))))
	     (strathelp=recursive-number number "s" "0")))
	  ((term~variable-p op)
	   (string-downcase (string (keim~name op))))
	  (t
	   (omega~error "~%Not implemented case in function strathelp=convert-operation-recursive II.")))))


(defun strathelp=read-file-as-strings (file-name)
  (declare (edited  "30-MAR-1998")
	   (authors Ameier)
	   (input   "A file.")
	   (effect  "None.")
	   (value   "A string, with the contents of the file."))
  (with-open-file (stream file-name
			  :direction :input)
		  (do* ((eof-flag nil)
			(protocoll 'initial)
			(list-of-strings nil))
		      (eof-flag
		       list-of-strings)
		    (let ((string-line (read-line stream nil '+-*/)))
		      ;;(format T "~%STRING-LINE iS: '~A',~%PROTOCOLL IS: ~A" string-line protocoll)
		      (cond ((equal string-line '+-*/) 
			     (setf eof-flag 't))
			    ((and (equal protocoll 'initial)
				  (string= string-line "*                         P R O O F   P R O T O C O L                          *"))
			     (setf protocoll 'on)
			     ;; skip two lines!!!
			     (read-line stream nil '+-*/)
			     (read-line stream nil '+-*/))
			    ((and (equal protocoll 'on)
				  (string= string-line ""))
			     (setf protocoll 'finish))
			    ((or (string-equal protocoll 'initial)
				 (string-equal protocoll 'finish))
			     (setf list-of-strings (append list-of-strings (list string-line)))))))))




;;(setf sod*current-strategies '(WMOnResidueClass))
;;
;;(setf sod*current-strategic-control-rules nil)
