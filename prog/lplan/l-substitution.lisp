;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-

(in-package "KEIM")

(defun subst~create-ntc (domain codomain &optional copy)
  (declare (edited  "04-NOV-1991 11:00")
           (authors GKLEIN RICHTS)
           (input   "A list of variables and a list of terms. Optional a flag.")
           (effect  "None.")
           (value   "The new substitution with the domain DOMAIN and the codomain CODOMAIN."
                    "If COPY = T the input-lists are copied.")
           (example "(x y) (a (f a)) --> {(X --> A) (Y --> (F A))}"))
  (let ((mapping (mapp~create domain codomain copy)))
    (change-class mapping 'subst+substitution))
  )

(defun subst~create-nso (domain codomain &optional copy)
  (declare (edited  "04-NOV-1991 11:00")
           (authors GKLEIN RICHTS)
           (input   "A list of variables and a list of terms. Optional a flag.")
           (effect  "None.")
           (value   "The new substitution with the domain DOMAIN and the codomain CODOMAIN."
                    "If COPY = T the input-lists are copied.")
           (example "(x y) (a (f a)) --> {(X --> A) (Y --> (F A))}"))
  (let ((mapping (mapp~create domain codomain copy)))
    (change-class mapping 'subst+substitution))
  )

(defun subst~apply-ntc (substitution object &key (destructive nil) (replacers-downto '(data+struct))
				     (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:14")
           (authors RICHTS AMEIER)
           (input   "A substitution and a struct.")
           (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "If DESTRUCTIVE is true the input struct is changed destructively."
		    "If DESTRUCTIVE is nil, but DOWNTO is a list of Clos-Classes,"
		    "all objects of this classes are shared. So if some substructs are"
		    "of one of this shared objects, the replacement is also done destructively"
		    "on them.")
           (value   "The new object with its terms instantiated by SUBSTITUTION."
		    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
		    "        A copy of all structs of the codomain of the substitution is made one time, using"
		    "        data~copy with REPLACERS-DOWNTO as downto argument and :all-classes as preserve"
		    "        argument. This (possibly new created) structs are then used for the replacement.")
	   (example "{(X --> A) (Y --> B)} (Q X (G X Y)) --> (Q A (G A B))"))
  (data~replace-free-variables-ntc object (subst~domain substitution) (subst~codomain substitution)
				   :destructive destructive
				   :downto downto
				   :replacers-downto replacers-downto
				   :test test))

(defun subst~compose-substitution-nso (outer-substitution inner-substitution
							  &key (destructive nil) (replacers-downto '(data+struct))
							  (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:19")
           (authors RICHTS AMEIER)
           (input   "Two substitutions.")
	   (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "If DESTRUCTIVE is true the input codomain structs of the INNER-SUBSTITUTION are"
		    "changed destructively. If DESTRUCTIVE is nil, but downto contains some Clos-Classes,"
		    "all objects of this classes are shared. So if some substructs of the INNER-SUBSTITUTION"
		    "are of one of this shared objects, the replacement is also done destructively on them.")
           (value   "OUTER-SUBSTITUTION is applied to the codomain of INNER-SUBSTITUTION and all domain-variables of"
                    "OUTER-SUBSTITUTION not in the domain of INNER-SUBSTITUTION are added with their codomain-terms:"
                    "The new substitution."
		    "REMARK: In order to obtain an idempotent substitution the codomain of OUTER-SUBSTITUTION"
                    "        and the domain of INNER-SUBSTITUTION must be variable disjoint."
		    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
		    "        A copy of all structs of the codomain of the OUTER-SUBSTITUTION is made one time, using"
		    "        data~copy with REPLACERS-DOWNTO as downto argument and :all-classes as preserve"
		    "        argument. This (possibly new created) structs are then used for the replacement and"
		    "        are added (together with there according domain variables) as new pairs.")
	   (example "{(Z --> A)} {(X --> A) (Y --> (F Z))} --> {(Z --> A) (Y --> (F A)) (X --> A)}"
		    "{(X --> A) (Y --> (F Z))} {(Z --> A)} --> {(Y --> (F Z)) (X --> A) (Z --> A)}"))
  (let* ((shared-outer-substitution (subst~create-ntc (subst~domain outer-substitution)
						      (mapcar #'(lambda (codo-term)
								  (data~copy codo-term
									     :downto
									     replacers-downto
									     :explode nil
									     :preserve :all-classes))
							      (subst~codomain outer-substitution))))
	 (new-substitution (subst~apply-ntc shared-outer-substitution
					    inner-substitution
					    :destructive destructive
					    :replacers-downto '(data+struct)
					    :downto downto
					    :test test)))
    (mapc #'(lambda (var term)
              (unless (member var (subst~domain new-substitution) :test #'data~equal)
		(subst~insert-component! var term new-substitution)))
          (subst~domain shared-outer-substitution)
	  (subst~codomain shared-outer-substitution))
    new-substitution))

(defun subst~disjoint-compose-substitution-nso (outer-substitution inner-substitution
								   &key (destructive nil) (replacers-downto '(data+struct))
								   (downto '(data+primitive)) (test #'eq))
  (declare (edited  "04-NOV-1991 11:22")
           (authors RICHTS gk AMEIER)
           (input   "Two substitutions the domains of which are disjoint.")
	   (effect  "Depends on the keyword arguments DESTRUCTIVE and DOWNTO."
		    "If DESTRUCTIVE is true the input codomain structs of the INNER-SUBSTITUTION are"
		    "changed destructively. If DESTRUCTIVE is nil, but downto contains some Clos-Classes,"
		    "all objects of this classes are shared. So if some substructs of the INNER-SUBSTITUTION"
		    "are in one of this shared objects, the replacement is also done destructively.")
           (value   "OUTER-SUBSTITUTION is applied to the codomain of INNER-SUBSTITUTION and the domain and codomain of"
                    "OUTER-SUBSTITUTION is added to the resulting substitution:"
                    "A new substitution."
		    "REMARK: In order to obtain an idempotent substitution the codomain of OUTER-SUBSTITUTION"
                    "        and the domain of INNER-SUBSTITUTION must be variable disjoint."
		    "Remark: Which structs are shared depends also on the keyword argument REPLACERS-DOWNTO."
		    "        A copy of all structs of the codomain of the OUTER-SUBSTITUTION is made one time, using"
		    "        data~copy with REPLACERS-DOWNTO as downto argument and :all-classes as preserve"
		    "        argument. This (possibly new created) structs are then used for the replacement and"
		    "        are added (together with there according domain variables) as new pairs.")
	   (example "{(X --> F) (Y --> (F A))} {(Z --> C)} -->{(Z --> C) (X --> F) (Y --> (F A))}"))  
  (when (intersection (subst~domain outer-substitution) (subst~domain inner-substitution) :test test)
    (error "Substitution ~A and Substitution ~A should be disjoint." outer-substitution inner-substitution))
  (let* ((shared-outer-substitution (subst~create-ntc (subst~domain outer-substitution)
						      (mapcar #'(lambda (codo-term)
								  (data~copy codo-term
									     :downto replacers-downto
									     :preserve :all-classes
									     :explode nil))
							      (subst~codomain outer-substitution))))
	 (new-substitution (subst~apply-ntc shared-outer-substitution
					    inner-substitution
					    :destructive destructive
					    :replacers-downto '(data+struct)
					    :downto downto
					    :test test)))
    (setf (subst~domain new-substitution) (append (subst~domain new-substitution) (subst~domain shared-outer-substitution)))
    (setf (subst~codomain new-substitution) (append (subst~codomain new-substitution) (subst~codomain shared-outer-substitution)))
    new-substitution))

(defmethod data=replace-fv-nso ((datum list) bound domain codomain destructive downto test)
    (declare (edited  "13-JAN-1998" "27-JAN-1998")
             (authors Gebhard Ameier)
             (input   "look at data~replace-free-variables")
             (effect  "look at data~replace-free-variables")
             (value   "look at data~replace-free-variables"))
    (if datum
	(mapcar #'(lambda (dat)
		    (data=replace-fv-nso dat
					 bound
					 domain
					 codomain
					 destructive
					 downto
					 test))
		datum)
      nil))

(defmethod data=replace-fv-nso ((datum type+constant)
				bound domain codomain destructive downto test)
  (declare (ignore bound destructive test downto codomain domain))
  datum)

(defmethod data=replace-fv-nso ((datum type+variable)
			    bound domain codomain destructive downto test)  
  (let ((pos (position datum domain :test test)))
    (cond ((or (find datum bound :test test) (null pos))
	   (if (or destructive
		   (eq downto data*all-classes-keyword)
		   (some #'(lambda (x) (typep datum x)) downto))
	       datum
	     (data~copy datum)))
	  (pos
	   (nth pos codomain))   
	  (t
	   (error "data~~replace-fv: This message shopuldn't appear!")))))

(defmethod data=replace-fv-nso ((datum type+appl)
			    bound domain codomain destructive downto test)
  (if (or destructive
	  (eq downto data*all-classes-keyword)
	  (some #'(lambda (x) (typep datum x)) downto))
      (progn
	(setf (data~appl-function datum)
	      (data=replace-fv-nso (data~appl-function datum)
			       bound domain codomain 't downto test))
	(setf (data~appl-arguments datum)
	      (data=replace-fv-nso (data~appl-arguments datum)
			       bound domain codomain 't downto test))
	datum)
    (let* ((new-fun (data=replace-fv-nso
		     (data~appl-function datum)
		     bound domain codomain destructive downto test))
	   (new-args (data=replace-fv-nso
		      (data~appl-arguments datum)
		      bound domain codomain  destructive downto test)))
      (make-instance (class-of datum)
		     :function new-fun
		     :arguments new-args 
		     :status (data=status datum)
		     :annotation nil))))

(defmethod data=replace-fv-nso ((datum type+func)
			    bound domain codomain destructive downto test)
  (if (or destructive
	  (eq downto data*all-classes-keyword)
	  (some #'(lambda (x) (typep datum x)) downto))
      (progn
	(setf (data~abstr-range datum)
	      (data=replace-fv-nso (data~abstr-range datum)
			       bound domain codomain 't downto test))
	(setf (data~abstr-domain datum)
	      (data=replace-fv-nso (data~abstr-domain datum)
			       bound domain codomain 't downto test))
	datum)
    (let* ((new-dom (data=replace-fv-nso
		     (data~abstr-domain datum)
		     bound domain codomain destructive downto test))
	   (new-ran (data=replace-fv-nso
		     (data~abstr-range datum)
		     bound domain codomain destructive downto test)))
      (make-instance (class-of datum)
		     :range new-ran 
		     :domain new-dom
		     :status (data=status datum)
		     :annotation nil))))


(defmethod data=replace-fv-nso ((datum type+schema) bound domain codomain destructive downto test)

  (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep datum x)) downto))
      (progn
	(setf (data~schema-range datum)
	      (data=replace-fv-nso (data~schema-range datum) (remove-duplicates (append bound (data~schema-domain datum)))
				   domain codomain 't downto test))
	datum)
    (let* ((new-ran (data=replace-fv-nso (data~schema-range datum) (remove-duplicates (append bound (data~schema-domain datum)))
					 domain codomain destructive downto test)))
      
      (make-instance (class-of datum)
		     :datum new-ran
		     :domain (data~schema-domain datum)
		     :status (data=status datum)
		     :annotation (data~copy (data~annotation datum) :downto (list 'data+variable))))))

(defmethod data=replace-fv-nso ((datum term+constant)
				bound domain codomain destructive downto test)
  (let* ((old-type (data~copy (data~annotation datum)
			      :downto (list 'data+primitive)))
	 (new-type (data=replace-fv-nso
		    (term~type datum)
		    bound domain codomain destructive downto test)))
    (if (keim~equal old-type new-type)
	datum
      (let* ((new-constant (term~constant-create (keim~name datum) new-type)))
	(setf (data~constant-origin new-constant)
	      (data~constant-origin datum))
	new-constant))))

(defmethod data=replace-fv-nso ((datum term+variable)
				bound domain codomain destructive downto test)  
  (setf (data~annotation datum)
	(data=replace-fv-nso (term~type datum)
			     bound domain codomain 't downto test))
  (let* ((pos (position datum domain :test test)))
    (cond ((or (find datum bound :test test)
	       (null pos))
	   ;; -> nicht ersetzen !
	   (if (or destructive
		   (eq downto data*all-classes-keyword)
		   (some #'(lambda (x) (typep datum x)) downto))
	       datum
	     (data~copy datum)))
	  (pos
	   ;; -> ersetzen 
	   (nth pos codomain))
	  (t (error "data~~replace-fv: This message shopuldn't appear!")))))


(defmethod data=replace-fv-nso ((datum term+appl)
			    bound domain codomain destructive downto test)
  (if (or destructive
	  (eq downto data*all-classes-keyword)
	  (some #'(lambda (x) (typep datum x)) downto))
      (progn
	(setf (data~appl-function datum)
	      (data=replace-fv-nso (data~appl-function datum)
				   bound domain codomain 't downto test))
	(setf (data~appl-arguments datum)
	      (data=replace-fv-nso (data~appl-arguments datum)
				   bound domain codomain 't downto test))
	(setf (data~annotation datum)
	      (data=replace-fv-nso (data~annotation datum)
				   bound domain codomain destructive downto
				   test))
	datum)
    (let* ((new-fun (data=replace-fv-nso
		     (data~appl-function datum)
		     bound domain codomain destructive downto test))
	   (new-args (data=replace-fv-nso
		      (data~appl-arguments datum)
		      bound domain codomain  destructive downto test))
	   (new-type (data=replace-fv-nso
		      (data~annotation datum)
		      bound domain codomain destructive downto test)))
      (make-instance (class-of datum)
		     :function new-fun
		     :arguments new-args 
		     :status (data=status datum)
		     :annotation new-type
		     ))))


(defmethod data=replace-fv-nso ((datum term+abstr)
			    bound domain codomain destructive downto test)
  
  (let* ((new-bound (append bound (mapcan #'(lambda (x)
					      (when (data~variable-p x) (list x)))
					  (data~abstr-domain datum)))))
    (if (or destructive
	    (eq downto data*all-classes-keyword)
	    (some #'(lambda (x) (typep datum x)) downto))
	(progn
	  (setf (data~abstr-range datum)
		(data=replace-fv-nso (data~abstr-range datum)
				     new-bound domain codomain 't downto test))
	  (setf (data~abstr-domain datum)
		(data=replace-fv-nso (data~abstr-domain datum)
				     new-bound domain codomain 't downto test))
	  (setf (data~annotation datum)
		(data=replace-fv-nso (data~annotation datum)
				     new-bound domain codomain destructive downto
				     test))
	  datum)
      (let* ((new-ran (data=replace-fv-nso (data~abstr-range datum)
					   new-bound domain codomain destructive downto test))
	     (new-dom (data=replace-fv-nso (data~abstr-domain datum)
					   new-bound domain codomain destructive downto test))
	     (new-type (data=replace-fv-nso (data~annotation datum)
					    new-bound domain codomain destructive downto test)))
	
	(make-instance (class-of datum)
		       :range new-ran 
		       :domain new-dom
		       :status (data=status datum)
		       :annotation new-type)))))



(defmethod data=replace-fv-nso ((datum term+schema)
			    bound domain codomain destructive downto test)
  
  (let* ((new-bound (append bound (mapcan #'(lambda (x)
					      (when (data~variable-p x) (list x)))
					  (data~schema-domain datum)))))
    (if (or destructive
	    (eq downto data*all-classes-keyword)
	    (some #'(lambda (x) (typep datum x)) downto))
	(progn
	  (setf (data~schema-range datum)
		(data=replace-fv-nso (data~schema-range datum)
				     new-bound domain codomain 't downto test))
	  (setf (data~schema-domain datum)
		(data=replace-fv-nso (data~schema-domain datum)
				     new-bound domain codomain 't downto test))
	  (setf (data~annotation datum)
		(data=replace-fv-nso (data~annotation datum)
				     new-bound domain codomain destructive downto
				     test))
	  datum)
      (let* ((new-ran (data=replace-fv-nso (data~schema-range datum)
					   new-bound domain codomain destructive downto test))
	     (new-dom (data=replace-fv-nso (data~schema-domain datum)
					   new-bound domain codomain destructive downto test))
	     (new-type (data=replace-fv-nso (data~annotation datum)
					    new-bound domain codomain destructive downto test)))
	
	(make-instance (class-of datum)
		       :datum new-ran 
		       :domain new-dom
		       :status (data=status datum)
		       :annotation new-type)))))
