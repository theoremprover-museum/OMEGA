;;; -*- syntax: common-lisp; package: omega; base: 10; mode: lisp -*-

;;; Tex output commands.

(in-package :omega)

(mod~defmod p2t-com :uses (mod sys com comint omega tex post2tex inter asi arg
			  )
	    :documentation "Definition of the TEX output commands."
  	    :exports (
		      ))

;; REMARK: The COMMADS THEMSELVES ARE NOW IN: .../omega-3/commands/presentation.com


;;;outputs a TeX (not LaTeX) form of the current nd proof into file

(defun p2tcom=tex-proof-file-default ()
  (let ((proof omega*current-proof-plan))
    (if (pds~proof-plan-p proof)
	 (make-pathname :directory "~" 
			:name (string-downcase (keim~name proof))
			:type "tex")
	(com~unspecified))))

(defun p2tcom=tex-proof (file style)
  (let ((proof omega*current-proof-plan))
    (if (pds~proof-plan-p proof)
	(tex~print-proof proof style file)
      (omega~error "No proof plan active, no TeX-file written"))))



;;;outputs a LaTeX form of the current ND proof into a file

(defun p2tcom=post2tex-proof (file)
  (let ((proof omega*current-proof-plan))
    (if (pds~proof-plan-p proof)
	(let* ((yes-no (omega~query
			"Including TeX macros to define occuring symbols? (y/n)"
			(arg~find-argtype 'boolean) T)))
	  (when yes-no
	    (post2tex~set-predefined-symbols (omega~query
					      "Name of the file containing the macro definitions:"
					      (arg~find-argtype 'pathname)
					      (make-pathname :directory (*user-top-dir*)
							     :name "omega-3/prog/post2tex/signature.tex"))))
	  (if (omega~query
	       "Sorting the ND lines? (y/n)"
	       (arg~find-argtype 'boolean) nil)
	      (post2tex~write-ho-proof-to-file (post2tex~sorted-structure proof)
					       file)
	    (post2tex~write-ho-proof-to-file proof file))
	  (post2tex~reset-predefined-symbols)
	  (values))
      (omega~error "No proof plan active, no TeX-file written"))))


;;;outputs a LaTeX form of the current plan state into a file
;;; is this function still used???????

; obsolete MP
;
;(defun p2tcom=post2tex-plan-state (file)
;  (let ((plan-state (first plan*planning-state)))
;    (if plan-state
;        (let* ((yes-no (omega~query
;                        "Including TeX macros to define occuring symbols? (y/n)"
;                        (arg~find-argtype 'boolean) t)))
;          (when yes-no
;            (post2tex~set-predefined-symbols (omega~query
;                                              "Name of the file containing the macro definitions:"
;                                              (arg~find-argtype 'pathname)
;                                              (*user-top-dir*))))
;          (post2tex~write-ho-proof-to-file (post2tex~sorted-structure plan-state)
;                                           file)
;          (post2tex~reset-predefined-symbols)
;          (values))
;      (omega~error "The current plan state is empty."))))
      

;;; Write a proof representation of the current proof in LaTeX format to a file

(defun p2tcom=post2tex-proof-tree (file)
  (declare (edited  "14-AUG-2000")
	   (authors Sorge)
	   (input   "A filename")
	   (effect  "Writes a proof tree in LaTeX to a file.")
	   (value   "Undefined."))
  (let ((proof omega*current-proof-plan))
    (if (pds~proof-plan-p proof)
	(let* ((yes-no (omega~query
			"Including TeX macros to define occuring symbols? (y/n)"
			(arg~find-argtype 'boolean) nil)))
	  (when yes-no
	    (post2tex~set-predefined-symbols (omega~query
					      "Name of the file containing the macro definitions:"
					      (arg~find-argtype 'pathname)
					      (pathname (*user-top-dir*)))))
	  (p2ttree~write-nd-tree-to-tex-file file proof)
	  (post2tex~reset-predefined-symbols)
	  (values))
      (omega~error "No proof plan active, no TeX-file written"))))


