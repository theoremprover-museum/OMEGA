(meth~define buildterm (list env)
	     (let ((term (term~env-lookup list env)))
	       (if term
		   term
		 (error "Something went wrong while building term ~A." list))))

(meth~define buildspecterm (env)
	     (let ((term (term~env-lookup '(p-plus-r1 (size-function F u v) (size-function G u v)) env)))
	       (print term)
	       (if term
		   term
		 (error "Something went wrong while building term."))))

(meth~define buildspecterm2 (env)
	     (let ((term (term~env-lookup '(size-function (p-plus-r1 F G) u v) env)))
	       (print term)
	       (if term
		   term
		 (error "Something went wrong while building term."))))
