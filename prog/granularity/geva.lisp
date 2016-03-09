(in-package :omega)

(mod~defmod GEVA 
            :uses (omega just keim parse pds pdsc proc rule socket tac)
            :documentation "granularity evaluation mechanism"
            :exports (
		      geva~evaluate
		      geva~evaluate-omega
		      geva~evaluate-and-reset-omega
		      geva~evaluate-omega-standard
		      
		      geva*count-steps-schema
		      geva*one-half-schema
		      geva*name-steps-schema
		      ))




(defclass geva+line () ;; proof line data structure
         ((content :initarg :content
	           :initform NIL
		   :accessor geva=line-content)  
	  (justs :initarg :justs
		    :initform NIL
		    :accessor geva=line-justs) 
	  (context :initarg :context
		    :initform NIL
		    :accessor geva=line-context)
	  (consequences :initarg :consequences
		  :initform NIL
		  :accessor geva=line-consequences)
	  (rating :initarg :rating
		  :initform NIL
		  :accessor geva=line-rating)
	  (flag   :initform NIL
		  :accessor geva=line-flag)))

(defclass geva+arg () ;;; proof argument data structure
         ((content :initarg :content
	           :initform NIL
		   :accessor geva=arg-content)  
	  (premises :initarg :premises
		    :initform NIL
		    :accessor geva=arg-premises) 
	  (conclusions :initarg :conclusions
		    :initform NIL
		    :accessor geva=arg-conclusions)
	  (method :initarg :method
		  :initform NIL
		  :accessor geva=arg-method)
	 (up :initarg :up
		  :initform NIL
		  :accessor geva=arg-up)
	  (down :initarg :down
		  :initform NIL
		  :accessor geva=arg-down)
	  (rating :initarg :rating
		  :initform NIL
		  :accessor geva=arg-rating)
	  (flag   :initform NIL
		  :accessor geva=arg-flag)))


(defclass geva+proof () ;;; proof data structure
	 ((premises :initarg :premises
	           :initform NIL
		   :accessor geva=proof-premises)  
	  (conclusions :initarg :conclusions
		    :initform NIL
		    :accessor geva=proof-conclusions)
	  (prooflines :initarg :prooflines
		      :initform NIL
		      :accessor geva=prooflines)
	  (proofarguments :initarg :proofarguments
		      :initform NIL
		      :accessor geva=proofarguments)
	  ))

(defclass geva+argument () ;;; ??? argument evaluation rather
	  ((function :initarg :fct
		     :initform (lambda (node) 0)
		     :accessor geva=argument-function)))

(defclass geva+argmethod () ;;; identifies an abstract method
          ((name :initarg :name
		 :initform 'unnamed
		 :accessor geva=argmethod-name)))

(defclass geva+argmethod-omega (geva+argmethod) ;;; identifies an abstract omega method
          ((outline :initarg :outline
		    :initform 'DONTCARE
		    :accessor geva=argmethod-outline)
            (subst :initarg :subst
		    :initform 'DONTCARE
		    :accessor geva=argmethod-subst)
	    (params :initarg :params
		    :initform 'DONTCARE
		    :accessor geva=argmethod-params)))


;;; a schema: assignment: rules -> parameters, default parameter, stateful parameters

(defclass geva+schema ()
  ((method-evaluation-table :initform (make-hash-table :test 'geva=method-equal :hash-function 'geva=evalhash)
			    :accessor geva=schema-table)
   (default-arg-rating      :initarg :default-arg-rating
                            :initform (lambda (x) 0)
			    :accessor geva=default-arg-rating)
   (paramcells              :initform NIL
			    :accessor geva=schema-paramcells)
   (proofline-rating        :initform (lambda (x) 0)
			    :accessor geva=schema-proofline-rating)))

(defun geva~make-schema() (make-instance 'geva+schema))

;;;; CONVERSION PDS -> GEVA Structure

;;; this structure is not generic, though
(defun geva=convert-pds2evalproof (pds)
(let* ((conv-nodes (make-hash-table))
       (conv-justs (make-hash-table :test #'equal)))
  (setq prooflines NIL)
  (setq proofarguments NIL)
  ;;; create sentencenodes
  (mapcar
   (lambda (x)
     (let* ((snode (make-instance 'geva+line :content x
				  :justs NIL
				  :context NIL
				  :consequences NIL
				  :rating 0)))
       (setf (gethash x conv-nodes) snode)))
   (prob~proof-steps pds))
  (maphash (lambda (x y) (setf prooflines (cons y prooflines))) conv-nodes)
  ;;; create argnodes
  (mapcar
   (lambda (n)
     (let* ((justifications (pdsn~all-justs n)))
       (mapcar
	(lambda (x) ;; one particular justification
	  (let* ((nconv (gethash n conv-nodes))
		 (method (make-instance 'geva+argmethod-omega 
					                      :name (keim~name (just~method x))
					                      :outline (pdsj~outline-pattern x)
							      :subst (pdsj~subst x)
							      :params (pdsj~parameters x)))
		 (premises1 (pdsj~all-premises x))
		 (premises (mapcar (lambda (x) (gethash x conv-nodes)) premises1))
		 (conclusions (list (gethash n conv-nodes)))
		 (argnode (make-instance 'geva+arg
					 :content x
					 :method method
					 :premises premises
					 :conclusions conclusions)))
	    (progn
	      (setf (gethash x conv-justs) argnode))
	    (setf (geva=line-justs nconv)
		  (cons argnode (geva=line-justs nconv)))))
	justifications)))
   (prob~proof-steps pds))
  (maphash (lambda (x y) (setf proofarguments (cons y proofarguments))) conv-justs)
  ;;; create proof object
  (let ((premises (mapcar
		   (lambda (x)
		     (gethash x conv-nodes))
		   (pds~support-nodes pds)))
	(conclusions (list (gethash (prob~proof-root pds) conv-nodes))))
  (make-instance 'geva+proof :premises premises :conclusions conclusions :prooflines prooflines :proofarguments proofarguments))))

;;; GENERIC FUNCTIONS AND PREDICATES

(defmethod geva=class-slot-names1 ((instance standard-object))
  "Given an INSTANCE, returns a list of the slots in the instance's class."
  (mapcar #'clos:slot-definition-name
          (clos:class-slots (class-of instance))))


(defun geva=flatten (obj acc)
  (cond ((consp obj) (let ((result (geva=flatten (car obj) nil)))
		       (geva=flatten (cdr obj) (append result acc))))
	((equal obj NIL) acc)
	(T (list obj))))


(defmethod geva=method-equal ((x geva+argmethod) (y geva+argmethod))
  (let* ((slots1 (geva=class-slot-names1 x))
	 (slots2 (geva=class-slot-names1 y)))
    (progn (format t "slot names 1 ~S~%" slots1)
	   (format t "slot names 2 ~S~%" slots2)
	(and (equal (class-of x)
		    (class-of y))
	     (equal (geva=class-slot-names1 x)
		    (geva=class-slot-names1 y))
	     (reduce (lambda (x y) (and x y))
		     (mapcar (lambda (z)
			       (let* ((slv1 (slot-value x z))
				      (slv2 (slot-value y z)))
				 (progn (format t "slot value 1 ~S~%" slv1)
					(format t "slot value 2 ~S~%" slv2)
				 (or (equal slv1 slv2)
				     (equal 'DONTCARE slv1)
				     (equal 'DONTCARE slv2)))))
			     (geva=class-slot-names1 x)))))))


;;; MAPPING FUNCTIONS ON THE EVAL-PROOF DATASTRUCTURE

(defmethod geva=mapproof-line ((snode geva+line)  sfunction afunction)
  (let* ((justs (geva=line-justs snode)))
    (progn (funcall sfunction snode)
	   ; (format t "I am in mapproof-dash! ~S~%" justs)
  (mapcar (lambda (x) (geva=mapproof-arg x sfunction afunction)) justs))))

(defmethod geva=mapproof-arg ((anode geva+arg) sfunction afunction)
  (let* ((premises (geva=arg-premises anode)))
    (progn (funcall afunction anode)
	   ; (format t "I am in mapproof-dashdash! ~S~%" premises)
  (mapcar (lambda (x) (geva=mapproof-line x sfunction afunction)) premises))))

(defun geva=mapproof (proof sfunction afunction)
  (let ((conclusions (geva=proof-conclusions proof)))
	(progn (format t "Conclusions ~S~%" conclusions)
	(mapcar (lambda (x) (geva=mapproof-line x sfunction afunction)) conclusions))))


;;;

(defmethod geva=map-line ((snode geva+line)  sfunction afunction)
  (let* ((justs (geva=line-justs snode))
	 (value (funcall sfunction snode)))
    (list value (mapcar (lambda (x) (geva=map-arg x sfunction afunction)) justs))))

(defmethod geva=map-arg ((anode geva+arg) sfunction afunction)
  (let* ((premises (geva=arg-premises anode))
	 (value (funcall afunction anode)))
  (list value (mapcar (lambda (x) (geva=map-line x sfunction afunction)) premises))))

(defun geva=map (proof sfunction afunction)
  (let ((conclusions (geva=proof-conclusions proof)))
	(progn (format t "Conclusions ~S~%" conclusions)
	(mapcar (lambda (x) (geva=map-line x sfunction afunction)) conclusions))))

;;; MECHANICS FOR THE EVALUATION MECHANISM

(defun geva=setallflags (proof symbol)
  (geva=map proof (lambda (x) (setf (geva=line-flag x) symbol)) (lambda (x) (setf (geva=arg-flag x) symbol))))

(defmethod geva=display-line ((snode geva+line))
  (format t "SENTENCENODE~%")
  (format t "content ~S~%" (geva=line-content snode))
  (format t "justs ~S~%" (geva=line-justs snode))
  (format t "context ~S~%" (geva=line-context snode))
  (format t "consequences ~S~%" (geva=line-consequences snode))
  (format t "rating ~S~%" (geva=line-rating snode))
  (format t "flag ~S~%~%" (geva=line-flag snode)))

(defmethod geva=display-arg ((anode geva+arg))
  (format t "ARGNODE~%")
  (format t "content ~S~%" (geva=arg-content anode))
  (format t "premises ~S~%" (geva=arg-premises anode))
  (format t "conclusions ~S~%" (geva=arg-conclusions anode))
  (format t "method ~S~%" (geva=argmethod-name (geva=arg-method anode)))
  (format t "rating ~S~%" (geva=arg-rating anode))
  (format t "flag ~S~%~%" (geva=arg-flag anode)))


;;; EVALUATION MECHANISM

(defun geva=eval-argnode (node schema)
  (if (equal (geva=arg-rating node) NIL)
      (let* ((method (gethash (geva=arg-method node) (geva=schema-table schema)))
	     (function (if (equal method NIL)
			   (progn (format t "unknown method!~S ~S  ~%"
					  (geva=arg-method node)
					  (geva=argmethod-name
					   (geva=arg-method node)))
				  (geva=default-arg-rating schema))
			 (geva=argument-function method)))
	     (value (funcall function node))
	     (setf (geva=arg-rating node) value))
	(setf (geva=arg-rating node) value))))
  ;; look up justification evaluation function in table
  ;; pass on arguments of node to eval function
  ;; filling in value)

(defmethod geva=read-rating-once ((snode geva+line))
  (if (equal (geva=line-flag snode) NIL)
      (let ((value (geva~line-rating snode)))
	(progn (setf (geva~line-flag snode) 'T)
	       value))))

(defmethod geva=read-rating-once ((anode geva+arg))
  (if (equal (geva=arg-flag anode) NIL)
      (let ((value (geva=arg-rating anode)))
	(progn (setf (geva=arg-flag anode) 'T)
	       value))))

(defun geva=harvest-arg-ratings (proof)
  (progn (geva=setallflags proof nil)
	 (let ((results (geva=flatten (geva=map proof (lambda (x) NIL) (lambda (x) (geva=read-rating-once x)))  nil)))
	   (progn
	     (geva=setallflags proof nil)
	     results))))

(defun geva=rate-all-arguments (proof schema)
  (geva=map proof (lambda (x) NIL) (lambda (x) (geva=eval-argnode x schema))))

(defun geva=evaluate-complete-arguments (proof schema)
  (let* ((arguments (geva=proofarguments proof)))
    (mapcan (lambda (x) (geva=eval-argnode x schema)) arguments)
    (mapcar #'geva=read-rating-once arguments)))
    

;;; EVALUATION HASHTABLE

;; (defun evalhash (x)
;;   (if (equal (class-of x) (find-class 'eval~argmethod-omega))
;;       (progn (format t "myhash in operation~%")
;;       (sxhash (list (eval~argmethod-name x) (eval~argmethod-outline x) (eval~argmethod-subst x) (eval~argmethod-params x))))
;;     (sxhash x)))

;; this hash function prevents usage of DONTCARES.

(defun geva=evalhash (x)
  (if (subtypep (class-of x) (find-class 'geva+argmethod))
      (progn (format t "method hashed!~%")
      (sxhash (geva=argmethod-name x) ))
    (sxhash x)))

;;; ;;; ;;;
(setq geva*eval-functions (make-hash-table :test 'geva=method-equal :hash-function 'geva=evalhash))
;;; ;;; ;;;

;;; PARAMETER-CELLS

;; (setq eval*learningcells nil)

(defclass geva+paramcell ()
	 ((init-value :initarg :initvalue
	              :initform NIL)  
	  (value :initarg :initvalue
		 :initform NIL)
	  (delta :initarg :delta
		 :initform (lambda (x) x)
		 :accessor geva~paramcell-delta)))

(defun geva=make-paramcell(init-value &optional delta)
  (if (null delta)
      (make-instance 'geva+paramcell :initvalue init-value)
  (make-instance 'geva+paramcell :initvalue init-value :delta delta)))
  
(defmethod geva=query-cell ((cell geva+paramcell))
  (let ((value (slot-value cell 'value)))
    (progn 
      (setf (slot-value cell 'value) (funcall (geva~paramcell-delta cell) value))
      value)))

(defmethod geva=reset-cell ((cell geva+paramcell))
    (setf (slot-value cell 'value) (slot-value cell 'init-value)))


;;; obsolete

(defun geva=makelearningcells (list-of-parameters)
  (if (consp list-of-parameters)
      (let* ((initval (car (car list-of-parameters)))
	     (rate (cdr (car list-of-parameters)))
	     (newcell (make-instance 'eval~learningcell :initvalue initval :rate rate)))
	(cons newcell (makelearningcells (cdr list-of-parameters))))
    (list)
    ))

(defun geva=reset-learningcells()
  (mapcar (lambda (x) (reset-cell x)) eval*learningcells))

;;;end (obsolete)

;;; USER FRIENDLYNESS

(defun geva=make-const-eval (const)
  (make-instance 'geva+argument :fct (lambda (node) const)))

;;; obsolete
(defun geva=make-learning-eval (ordinary-eval init rate)
  (let* ((cell (make-instance 'eval~learningcell :initvalue init :rate rate))
	 (fct (lambda (node) (let ((value (funcall (geva=argument-function ordinary-eval) node))
				   (cellvalue (query-cell cell)))
			       (* cellvalue value)))))
    (progn (setf eval*learningcells (cons cell eval*learningcells))
	   (make-instance 'geva+argument :fct fct))))
;;;end (obsolete)

(defun geva=register-paramcell (paramcell schema)
  (setf geva=schema-paramcells
	(cons paramcell
	      (geva=schema-paramcells schema)))
  )

(defun geva~reset-paramcells (schema)
  (mapcar (lambda (x) (geva=reset-cell x)) (geva=schema-paramcells schema))) 

(defun geva=make-paramcell-eval (paramcell-eval-fn init delta schema)  ;;; paramcell-eval-fn: node x eval -> eval
  (make-instance 'geva+argument :fct
		 (let ((paramcell (geva=make-paramcell init delta)))
		   (progn
		     (geva=register-paramcell paramcell schema)
		     (lambda (x) (funcall paramcell-eval-fn x (geva=query-cell paramcell)))))))
  

(defun geva~make-paramcell-eval-exponential (init rate schema)
  (geva=make-paramcell-eval
   (lambda (x y) y)
   init
   (lambda (x) (* rate x))
   schema))

;; (geva~make-paramcell-eval-exponential 5 0.2  geva*one-half-schema)

(defun geva=register-eval (argmethod argeval schema)
  (setf (gethash argmethod (geva=schema-table schema)) argeval))

(defun geva=mk-argmth-omega (name &optional (outline 'DONTCARE)
			     &optional (subst 'DONTCARE)
			     &optional (params 'DONTCARE))
  (make-instance 'geva+argmethod-omega :name name :outline outline :subst subst :params params))

(defun geva=eval-proof-arguments (cpp schema conversion)
  (let* ((evalproof (funcall conversion cpp)))
    (progn (geva=rate-all-arguments evalproof schema)
	   (geva=harvest-arg-ratings evalproof))))

(defun geva=eval-complete-proof-arguments (cpp schema conversion)
  (let* ((evalproof (funcall conversion cpp)))
    (geva=evaluate-complete-arguments evalproof schema)))


(defun geva=register+ (method-list eval-fn schema)
  (if (null method-list)
      NIL
    (progn
      (geva=register-eval (car method-list) eval-fn schema)
      (format t "Registering: ~S ~S ~S ~%" (car method-list) eval-fn schema)
      (geva=register+ (cdr method-list) eval-fn schema))))

(defun geva=register++ (method-list create-eval-fn schema)
  (if (null method-list)
      NIL
    (progn
      (geva=register-eval (car method-list) (funcall create-eval-fn) schema)
      (format t "Registering: ~S ~S ~S ~%" (car method-list)  (funcall create-eval-fn)schema)
      (geva=register++ (cdr method-list) create-eval-fn schema))))


;;;;; Top Level Procedures

(defun geva~evaluate (cpp schema conversion)
  (geva=eval-proof-arguments cpp schema conversion))

(defun geva~evaluate-omega (cpp schema op)
  (reduce op (geva=eval-proof-arguments cpp schema #'geva=convert-pds2evalproof)))

(defun geva~evaluate-omega-complete (cpp schema op)
  (reduce op (geva=eval-complete-proof-arguments cpp schema #'geva=convert-pds2evalproof)))

(defun geva~evaluate-and-reset-omega (cpp schema op)
  (let ((result (reduce op (geva=eval-proof-arguments cpp schema #'geva=convert-pds2evalproof))))
    (geva~reset-paramcells schema)
    result))

(defun geva~evaluate-omega-standard (cpp)
  (let* ((countsteps (geva~evaluate-omega cpp geva*count-steps-schema #'+))
	 (namesteps  (geva~evaluate-omega cpp geva*name-steps-schema (lambda (x y) (union (if (consp x) x (list x)) (list y)))))
         (ripsprobs  (geva~evaluate-omega cpp geva*rips-probability-schema #'*))
	 (countdefns  (geva~evaluate-omega cpp geva*count-defns-schema #'+))
	 (namedefns (geva~evaluate-omega cpp geva*name-defns-schema (lambda (x y) (union (if (consp x) x (list x)) (list y)))))
	 (countsteps-c (geva~evaluate-omega-complete cpp geva*count-steps-schema #'+))
	 (namesteps-c  (geva~evaluate-omega-complete cpp geva*name-steps-schema (lambda (x y) (union (if (consp x) x (list x)) (list y)))))
         (ripsprobs-c  (geva~evaluate-omega-complete cpp geva*rips-probability-schema #'*))
	 (countdefns-c (geva~evaluate-omega-complete cpp geva*count-defns-schema #'+))
	  (namedefns-c (geva~evaluate-omega-complete cpp geva*name-defns-schema (lambda (x y) (union (if (consp x) x (list x)) (list y)))))
	 )
    (format t "### EVALUTATION ###~%")
    (format t "### only relevant steps ###~%")
    (format t "Number of steps      : ~S ~%" countsteps)
    (format t "Method names         : ~S ~%" namesteps)
    (format t "Rips probabilities   : ~S ~%" ripsprobs)
    (format t "Definitions          : ~S ~%" countdefns)
    (format t "Definition names     : ~S ~%" namedefns)
    (format t "### complete proof ###~%")
    (format t "Number of steps      : ~S ~%" countsteps-c)
    (format t "Method names         : ~S ~%" namesteps-c)
    (format t "Rips probabilities   : ~S ~%" ripsprobs-c)
    (format t "Definitions          : ~S ~%" countdefns-c)
    (format t "Definition names     : ~S ~%" namedefns-c)
    (format t "### EVALUTATION ###~%")
    (list countsteps namesteps)))
  



;;;;; Schemata

;;; Schema 1: Counting Steps, only those that are registered

(setq geva*count-steps-schema (geva~make-schema))

(geva=register+
 (mapcar (lambda (x) (geva=mk-argmth-omega x))
	 '(RIPS-BCKW-IF-INTRO
	   RIPS-BCKW-IF-ELIM
	   RIPS-BCKW-ANDI
	   RIPS-MODUS-PONENS
	   RIPS-CONJ-MODUS-PONENS
	   RIPS-DILEMMA
	   RIPS-DISJ-MODUS-PONENS
	   RIPS-DISJ-SYLL
	   RIPS-DEMORGAN-1
	   RIPS-DEMORGAN-2
	   RIPS-NOTNOTE
	   RIPS-IMPI
	   RIPS-ANDI
	   RIPS-NOTE
	   RIPS-NOTI
	   RIPS-ORE
	   ORIL
	   ORIR
	   RIPS-NEG-COND-TRAFO
	   RIPS-COND-TRAFO
	   RIPS-CONJ-SYLLOGISM
	   RIPS-ANDE)) (geva=make-const-eval 1) geva*count-steps-schema)

(geva=register+
 (mapcar
  (lambda (x)
    (geva=mk-argmth-omega x))
  '(ANDI
    ANDE
    EXISTSI
    EXISTSE
    FALSEE
    FORALLE
    FORALLI
    IMPE
    IMPI
    NOTI
    NOTE
    ORE
    ORIL
    ORIR
    DEFN-CONTRACT
    DEFN-EXPAND
    ANDEL
    ANDER
					; KEIM::HYP
    OTTER
    defnE
    defnI
    DEFNI
    defnI
    defnE
    DefnI
    DefnE
    DefsI
    DefsE
    ASSERTION
    IMP2OR
    ))
 (geva=make-const-eval 1)
 geva*count-steps-schema)

;;; Schema 2: Just display the names of the steps involved

(setq geva*name-steps-schema
      (make-instance
       'geva+schema :default-arg-rating
       (lambda (x)
	 (geva=argmethod-name
	  (geva=arg-method x)))))



;;; Schema 3: All values start with 1 and drop by 1/2

(setq geva*one-half-schema (geva~make-schema))

(geva=register++
(mapcar (lambda (x) (geva=mk-argmth-omega x))
	 '(RIPS-BCKW-IF-INTRO
	   RIPS-BCKW-IF-ELIM
	   RIPS-BCKW-ANDI
	   RIPS-MODUS-PONENS
	   RIPS-CONJ-MODUS-PONENS
	   RIPS-DILEMMA
	   RIPS-DISJ-MODUS-PONENS
	   RIPS-DISJ-SYLL
	   RIPS-DEMORGAN-1
	   RIPS-DEMORGAN-2
	   RIPS-NOTNOTE
	   RIPS-IMPI
	   RIPS-ANDI
	   RIPS-NOTE
	   RIPS-NOTI
	   RIPS-ORE
	   ORIL
	   ORIR
	   RIPS-NEG-COND-TRAFO
	   RIPS-COND-TRAFO
	   RIPS-CONJ-SYLLOGISM
	   RIPS-ANDE
	   ANDI
    ANDE
    EXISTSI
    EXISTSE
    FALSEE
    FORALLE
    FORALLI
    IMPE
    IMPI
    NOTI
    NOTE
    ORE
    ORIL
    ORIR
    DEFN-CONTRACT
    DEFN-EXPAND
    ANDEL
    ANDER
					; KEIM::HYP
    OTTER
    defnE
    defnI
    DEFNI
    defnI
    defnE
    DefnI
    DefnE
    DefsI
    DefsE
    ASSERTION
    IMP2OR
	   )) (lambda() (geva~make-paramcell-eval-exponential 1 0.5 geva*one-half-schema)) geva*one-half-schema) 


;;; Schema 4 : Rips's parameters


(setq geva*rips-probability-schema (geva~make-schema)) 

;;; table 7.5 p. 259 "Complex multi-variable arguments"

(geva=register-eval (geva=mk-argmth-omega 'RIPS-ANDE) (geva=make-const-eval 0.96) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-BCKW-ANDI) (geva=make-const-eval 0.83) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-BCKW-IF-ELIM) (geva=make-const-eval 0.90) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-BCKW-IF-INTRO) (geva=make-const-eval 0.66) geva*rips-probability-schema)
;; backward not intro: not implemented
(geva=register-eval (geva=mk-argmth-omega 'RIPS-MATCHING) (geva=make-const-eval 0.62) geva*rips-probability-schema)
;; actually: matching to variables 0.62, matching to names: 1.00
;;; then: probability of guessing: 0.70

(geva=register-eval (geva=mk-argmth-omega 'RIPS-MATCHING) (geva=make-const-eval 0.62) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-BCKW-IF-INTRO) (geva=make-const-eval 0.66) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-BCKW-IF-ELIM) (geva=make-const-eval 0.90) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-MODUS-PONENS) (geva=make-const-eval 0.723) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-ANDE) (geva=make-const-eval 0.96) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-ANDI) (geva=make-const-eval 0.83) geva*rips-probability-schema)

;;; table 7.2 p. 243 "Syllogism performance"

;;; left out

;;; table 5.2 p. 156 "Evidence"

(geva=register-eval (geva=mk-argmth-omega 'RIPS-DISJ-MODUS-PONENS) (geva=make-const-eval 1) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-ANDI) (geva=make-const-eval 1) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-ANDE) (geva=make-const-eval 0.963) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-IMPI) (geva=make-const-eval 0.861) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-ORE) (geva=make-const-eval 0.858) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-MODUS-PONENS) (geva=make-const-eval 0.723) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-DE-MORGAN-1) (geva=make-const-eval 0.715) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-DISJ-SYLLOGISM) (geva=make-const-eval 0.713) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-NOTI) (geva=make-const-eval 0.238) geva*rips-probability-schema)
(geva=register-eval (geva=mk-argmth-omega 'RIPS-ORI) (geva=make-const-eval 0.197) geva*rips-probability-schema)

(geva=register-eval (geva=mk-argmth-omega 'KEIM::HYP) (geva=make-const-eval 1) geva*rips-probability-schema)

 ;   defnE
 ;   defnI
 ;   DEFNI
 ;   defnI
 ;   defnE
 ;   DefnI
 ;   DefnE
 ;   DefsI
 ;   DefsE

(setf (geva=default-arg-rating geva*rips-probability-schema) (lambda (x) 1))

;;; Schema 5: Count Definition Expansions and Contractions

(setq geva*count-defns-schema (geva~make-schema)) 

(setq omega-rule-defnE1 (geva=mk-argmth-omega 'defnE))
(setq omega-rule-defnE2 (geva=mk-argmth-omega "defnE"))
(setq omega-rule-defnE3 (geva=mk-argmth-omega "DefnE"))
(setq omega-rule-defnI1 (geva=mk-argmth-omega "DefnI"))
(setq omega-rule-defnI2 (geva=mk-argmth-omega "defnI"))
(setq omega-rule-defnI3 (geva=mk-argmth-omega 'defnI))
(setq omega-rule-defnI4 (geva=mk-argmth-omega 'DefnI))
(setq omega-rule-defsI (geva=mk-argmth-omega "DefsI"))
(setq omega-rule-defsE (geva=mk-argmth-omega "DefsE"))

(geva=register+ (list omega-rule-defnE1 omega-rule-defnE2 omega-rule-defnE3 omega-rule-defnI1 omega-rule-defnI2 omega-rule-defnI3 omega-rule-defnI4 omega-rule-defsI omega-rule-defsE)  (geva=make-const-eval 1)  geva*count-defns-schema)



(setq geva*name-defns-schema
      (make-instance
       'geva+schema :default-arg-rating
       (lambda (x)
	 NIL)))

(geva=register+ (list omega-rule-defnE1 omega-rule-defnE2 omega-rule-defnE3 omega-rule-defnI1 omega-rule-defnI2 omega-rule-defnI3 omega-rule-defnI4 omega-rule-defsI omega-rule-defsE) (make-instance 'geva+argument :fct (lambda (node) (first (pdsj~parameters (geva=arg-content node)))))  geva*name-defns-schema)

(make-instance 'geva+argument :fct (lambda (node)  node))

(setq geva*name-defns-schema (geva~make-schema)) 

(geva~evaluate-omega omega*current-proof-plan geva*name-defns-schema (lambda (x y) (union (if (consp x) x (list x)) (list y))))


(car (pdsj~parameters defni))
