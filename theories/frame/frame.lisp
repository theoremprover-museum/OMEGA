;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
(in-package :omega)

(defvar frame*th (th~find-theory 'frame))
(defvar frame*db
      (if  (and (sys~getenv "MAKEPORT")
		(sys~getenv "MAKEHOST"))
	  (list (sys~getenv "MAKEPORT")
		(sys~getenv "MAKEHOST"))
	(list "localhost" 31415)))

(setf frame*th (th~find-theory 'frame))

;---Some classes--------------------------------------------------------

(eval-when (load compile eval)

  (defclass frame+conditional (keim+name)
    ((info :initarg :info
	   :initform nil
	   :accessor frame~info
	   :documentation "follows")
     (conditions :initarg :conditions
		 :initform nil
		 :accessor frame~conditions
		 :documentation "follows")
     (evc :initarg :evc
	  :initform nil
	  :accessor frame~evc
	  :documentation "follows")))

  (defclass frame+cproperty (frame+conditional)
    ((property  :initarg :property
		:initform nil
		:accessor frame~property
		:documentation "follows")))
  
  (defclass frame+cdefinition (frame+conditional)
    ((mapsfrom :initarg :mapsfrom
	       :initform nil
	       :accessor frame~mapsfrom
	       :documentation "follows")
     (mapsto :initarg :mapsto
	     :initform nil
	     :accessor frame~mapsto
	     :documentation "follows")))

  )
(defmethod print-object ((frame frame+cdefinition) stream)
  (format stream "~%--------------cdefi---------------------~%~A~%~{~A ~}~%|-~%~A --> ~A"
	  (frame~evc frame)
	  (frame~conditions frame)
	  (frame~mapsfrom frame)
	  (frame~mapsto frame)))

(defmethod print-object ((frame frame+cproperty) stream)
  (format stream "~%--------------cprop---------------------~%~A~%~{~A ~}~%|-~%~A"
	  (frame~evc frame)
	  (frame~conditions frame)
	  (frame~property frame)))

;---Loading the theory--------------------------------------------------------

(defmethod post~read-object :around ((term symbol) (env env+environment) (indicator (eql :existing-term)))
  (frame=get-declaration term env)
  (call-next-method))

(defmethod post~read-object :around ((term symbol) (env env+environment) (indicator (eql :existing-type)))
  (frame=get-declaration term env)
  (call-next-method))

(defun frame=get-declaration (term env)
   (when (and (not (env~lookup-object term env))
	      (or (eq env (th~env frame*th))
		  (member (th~env frame*th) (keim::env=trans-parents env) :test #'eq)))
    (let ((make (frame=get-from-framedb 'fx~get-declaration term)))
      (when make (frame=read-declaration make (th~env frame*th))))))

;---Communication--------------------------------------------------------

(defun frame=get-from-framedb (command &rest args)
  (unless (socket~find-socket :frame)
    (socket~define :frame))
  (sys~handler-case
   (progn 
     (socket~connect (first frame*db)(second frame*db) :frame)
     (http~send-request :frame
			(rpc~compose-methodcall command (frame=convert-args args))
			:uri "/rpc2")
     (let ((response (http~read-page :frame)))
       (socket~close :frame)
       (if  (= (second (car response)) 200)
	   (multiple-value-bind (parsed ok?)
	       (rpc~parse-methodresponse (third response))
	     (if ok? (frame=string2symbol parsed) (omega~error "RPC-ERROR ~A" parsed)))
	 (omega~error "HTTP-ERROR ~A" response))))
   (T (c)
      (socket~close :frame)
      (omega~error "~A" c))))

(defgeneric frame=convert-args (args)
  ;; format of terms: 2-elemnent list (term sig-declarations)
  (:method ((args cons))
	   (cons (frame=convert-args (car args))
		 (frame=convert-args (cdr args))))
  (:method ((args term+term))
	   (if omega*current-proof-plan
	       (let* ((stream (make-string-output-stream))
		      (proof-env (pds~environment omega*current-proof-plan))
		      (prob-env (prob~proof-environment omega*current-proof-plan))
		      (locals  (mapcan #'(lambda (env)
					   (mapcar #'(lambda (entry)
						       (env~lookup-object entry env))
						   (env~classes-keys env
								     (list 'term+variable 'term+constant 'type+constant 'type+variable)
								     nil)))
				       (list proof-env prob-env)))
		      (terms (remove-if-not #'(lambda (sub) (and (data~primitive-p sub)
;                                                                 (progn (omega~trace "~A member ~A env ~A"
;                                                                                     sub
;                                                                                     (member sub locals :test 'eq)
;                                                                                     (env~lookup-object (keim~name sub) proof-env))
;                                                                        t)
								 (or (member sub locals :test 'eq)
								     (not (env~lookup-object (keim~name sub) proof-env)))))
					    (data~all-substructs args))))
		 (cons
		  (progn 
		    (post~print args stream)
		    (get-output-stream-string stream))
		    (mapcar #'(lambda (term)
				(format stream "(~A ." (frame=class2key term))
				(post~print-declaration term stream)
				(format stream ")")
				(get-output-stream-string stream))
			  terms)))
	     (post~print args nil)))
  (:method ((stuff t))
	   stuff))

(defun frame=class2key (class)
  (let ((str (string (class-name (class-of class)))))
    (subseq str (1+ (search "+" str)))))
  
(defgeneric frame=string2symbol (stuff)
  (:method ((stuff cons))
	   (mapcar #'frame=string2symbol stuff))
  (:method ((stuff string))
	   (read-from-string stuff))
  (:method ((stuff t))
	   stuff))

;---Parsing MaKE stuff--------------------------------------------------------

(defun frame=read-declaration (obj env)
  (when (and (not (env~lookup-object (if (symbolp (cadr obj)) obj (caadr obj)) env))
	     (member (th~env frame*th) (cons env (keim::env=trans-parents env)) :test 'eq))
    (post~read-object  (cadr obj) env
		      (read-from-string (concatenate 'string ":" (string (car obj)))))))

(defun frame=read-cprops (cprops)
  (mapcar #'frame=read-cprop cprops))

(defun frame=read-cprop (cprop)
  (frame=make-cproperty
   (frame=read-term (cdr (assoc 'property cprop)))
   (frame=read-terms (cdr (assoc 'conditions cprop)))
   (mapcar #'frame=read-terms (cdr (assoc 'evc cprop)))))

(defun frame=read-cdefis (cprops)
  (mapcar #'frame=read-cdefi cprops))

(defun frame=read-cdefi (cprop)
  (frame=make-cdefinition
   (frame=read-term (cdr (assoc 'mapsfrom cprop)))
   (frame=read-term (cdr (assoc 'mapsto cprop)))
   (frame=read-terms (cdr (assoc 'conditions cprop)))
   (mapcar #'frame=read-terms (cdr (assoc 'evc cprop)))))

(defun frame=read-terms (terms)
  (mapcar #'frame=read-term terms))

(defun frame=read-term (term)
  (mapc #'(lambda (dec) (frame=read-declaration dec (pds~environment omega*current-proof-plan))) (rest term))
  (post~read-object (car term) (pds~environment omega*current-proof-plan) :existing-term))

;---Queries--------------------------------------------------------

(defun frame~get-equations (prems conc)
  (frame=read-cprops
   (apply #'frame=get-from-framedb (cons 'fx~get-equations
					 (cons conc prems)))))

;(let (frame*prpln frame*eqobj frame*eqass frame*cons frame*names)

  (defun frame=init ()
    (unless (eq frame*prpln omega*current-proof-plan)
      (setf frame*prpln omega*current-proof-plan
	    frame*eqobj (make-hash-table :test #'eq)
	    frame*eqass (make-hash-table :test #'eq)
	    frame*conse (make-hash-table :test #'eq)
	    frame*names (make-hash-table :test #'eq))))


(defun frame~find-object (name)
  (gethash name frame*names))

(defun frame=make-cproperty (property conditions evc &optional info)
  (let ((name (gentemp 'cprop)))
    (setf (gethash name frame*names)
	  (make-instance 'frame+cproperty
			 :info info
			 :evc evc
			 :property property
			 :name name
			 :conditions conditions))))

(defun frame=make-cdefinition (from to  conditions evc &optional info)
  (let ((name (gentemp 'cdefi)))
    (setf (gethash name frame*names)
	  (make-instance 'frame+cdefinition
			 :info info
			 :evc evc
			 :mapsfrom from
			 :mapsto to
			 :name name
			 :conditions conditions))))

  
(defgeneric frame~get-equations-for-objects (term)
  (:method :before (term)
	   (declare (ignore term))
	   (frame=init))
  (:method ((term term+term))
	   (frame=read-cdefis
	    (apply #'frame=get-from-framedb (list 'fx~get-equations-for-objects term))))
  (:method ((node node+node))
	   (or (gethash node frame*eqobj)
	       (setf (gethash node frame*eqobj)
		     (mapc #'(lambda (equ) (setf (frame~info equ) node)) (frame~get-equations-for-objects (node~formula node)))))))
			 
(defgeneric frame~get-equations-from-assumption (assump)
  (:method :before (term)
	   (declare (ignore term))
	   (frame=init))
  (:method ((term term+term))
	   (frame=read-cdefis
	    (apply #'frame=get-from-framedb (list 'fx~get-equations-from-assumption term))))
  (:method ((node node+node))
	   (or (gethash node frame*eqass)
	       (setf (gethash node frame*eqass)
		     (mapc #'(lambda (equ) (setf (frame~info equ) node)) (frame~get-equations-from-assumption (node~formula node)))))))

(defgeneric frame~get-consequences (term)
  (:method :before (term)
	   (declare (ignore term))
	   (frame=init))
  (:method ((term term+term))
	   (frame=read-terms
	    (frame=get-from-framedb 'fx~get-consequences  term)))
  (:method ((node node+node))
	   (or (gethash node frame*conse)
	       (setf (gethash node frame*conse)
		     (frame~get-consequences (node~formula node))))))


(defun frame~get-structural-defis (const term)
  (frame=init)
  (mapcar #'(lambda (result)
	      (cons (frame=read-cdefi (car result))
		    (frame=read-terms (rest result))))
	  (frame=get-from-framedb  'fx~get-structural-defis const term)))


(defgeneric frame~get-definitions (term)
  (:method :before (term)
	   (declare (ignore term))
	   (frame=init))
  (:method ((term term+term))
	   (mapcar #'frame=read-terms
	    (frame=get-from-framedb 'fx~get-definitions  term)))
  (:method ((node node+node))
	   (frame~get-definitions (node~formula node))))


(defgeneric frame~equality? (term)
  ;to be replace by frame-kinds
  (:method ((term term+term))
	   (frame=get-from-framedb 'fx~equality? term))
  (:method ((node node+node))
	  (frame~equality?  (node~formula node))))


(defgeneric frame~logic? (term)
  (:method ((term term+term))
	   (frame=get-from-framedb 'fx~logic? term))
  (:method ((node node+node))
	  (frame~logic?  (node~formula node))))

(defun frame~get-connectives ()
  (mapcar #'(lambda (con)
	      (cons (frame=read-term (car con))
		    (cdr con)))
	      (frame=get-from-framedb 'fx~connectives)))


(defmethod th~connectives :around (thing)
  (let ((connectives (call-next-method thing)))
    (if connectives connectives
      (let ((connectives (frame~get-connectives)))
	(setf (th~connectives frame*th)
	      (mapcar #'(lambda (con)
			  (list (car con)
				(case (cadr con)
				  ('universal    "UNIVERSAL-QUANTOR")
				  ('universal1   "UNIVERSAL-QUANTOR")
				  ('existential  "EXISTENTIAL-QUANTOR")
				  ('existential1 "EXISTENTIAL-QUANTOR")
				  ('falsehood "FALSITY")
				  (otherwise (string (cadr con))))))
		      connectives))))))
  
  
;)


;;(frame~get-equations (mapcar #'(lambda (nod) (node~formula (pds~label2node nod))) '(a1 a2 a3)) (node~formula (pds~label2node 'image-closed)))
;;(frame~get-equations (mapcar #'(lambda (nod) (node~formula (pds~label2node nod))) '(a1 a2)) (node~formula (pds~label2node 'ac-simple)))
;;(frame=get-from-framedb 'fx=test 100000)

