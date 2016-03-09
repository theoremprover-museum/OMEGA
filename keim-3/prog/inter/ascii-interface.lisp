;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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

(in-package :keim)

(mod~defmod asi :uses (mod sys inter arg) 
	    :documentation "Defines a simple ASCII interface."
	    :exports (
		      asi+inter
		      asi~create
		      asi~input
		      asi~output
		      asi~error
                      asi~warn
                      asi~message
                      asi~trace
                      asi~query
		      asi+no-input-error
		      asi~linereadp
                      inter~input-query

		      asi*abort-symbol
		      ))


#{
\section{A simple ASCII interface}
 Here we define a simple interface using streams.  The {\vb asi+inter}
 interface class has three slots.  The first is used for input and
 must be an input stream.  The second two, output and error, must
 be output streams.  Output is for normal output and error is for
 error output.  This kind of interface is conceptually similar to
 the {\vb *standard-input*}, {\vb *standard-output*} and {\vb *error-output*} 
of \commonlisp.

 In 1998, we use some additional streams for warnings, messages, tracing and
 query.
#}

(eval-when (load compile eval) 
(defclass asi+inter (inter+face)
  ((input :initarg :input :accessor asi~input
          :documentation "An input stream.")
   (output :initarg :output :accessor asi~output
          :documentation "An output stream.")
   (error :initarg :error :accessor asi~error
          :documentation "An output stream for error messages.")
   (warn :initarg :warn :accessor asi~warn
         :documentation "An output stream for warning messages.")
   (message :initarg :message :accessor asi~message
            :documentation "An output stream for status messages.")
   (trace :initarg :trace :accessor asi~trace
          :documentation "An output stream for monitoring messages.")
   (query :initarg :query :accessor asi~query
          :documentation "An input/output stream for queries."))
 
  (:documentation "An interface with a single input stream, a output stream 
for normal output and a output stream for error output. Extensions: warn for
warning output, message for (status) output, trace for code monitoring and
 query for additional input of functions (do you really mean it)")))


(defun asi~create (&key (name (gensym "ASI-INTERFACE"))
			(input *standard-input*) (output *standard-output*)
			(error *error-output*)
                        (warn *error-output*)
                        (trace *trace-output*)
                        (message *trace-output*)
                        (query *query-io*)
			(help ""))
  (declare 
   (authors nesmith konrad)
   (input "Four keyword parameters. NAME names the interface.  INPUT
is the interface's input stream (defaults to *standard-input*); OUTPUT
is the interface's output stream and defaults to *standard-output*,
while ERROR is the interface's ERROR stream and defaults to *error-output*.
We also use trace and query-io.")
   (value "A new interface initialized with given slots."))
  (assert (symbolp name))
  (assert (input-stream-p input))
  (assert (output-stream-p output))
  (assert (output-stream-p error))
  (assert (output-stream-p trace))
  (assert (output-stream-p message))
  (assert (output-stream-p warn))
  (assert (output-stream-p query))
  (assert (input-stream-p query))
  (assert (stringp help))
  (make-instance 'asi+inter :name name :input input 
		 :output output :error error :warn warn
		 :trace trace :message message :query query :help help))

(defvar asi*abort-symbol :abort)

(defmacro asi=prompt-with-abort (body)
  (declare (edited  "08-DEC-1999")
	   (authors Sorge)
	   (effect  "Prompts with the possibility to abort command execution."))
  `(if (eq obj asi*abort-symbol)
       (sys~signal
	(sys~make-condition
	 'asi+abort
	 ;:interface interface 
	 ))
     ,body))
       

#{ {\vb inter~terpri} merely performs a {\vb terpri} on the output stream.#}
(defmethod inter~terpri ((interface asi+inter))
  (terpri (asi~output interface)))

#{{\vb inter~fresh-line} performs a {\vb fresh-line} on the output stream.
#}
(defmethod inter~fresh-line ((interface asi+inter))
  (fresh-line (asi~output interface)))

#{
{\vb inter~print-error} prints the error object to the error stream,
 with a short preamble.
#}
(defmethod inter~print-error ((interface asi+inter) error)
  (format (asi~error interface) "~%;;;ERROR: ~A~&" error))

#{ {\vb inter~print-warning} prints the warning object to the warn stream,
 with a short preamble.
#}
(defmethod inter~print-warning ((interface asi+inter) warning)
  (format (asi~warn interface) "~%;;;WARNING: ~A~&" warning))

#{ {\vb inter~print-message} prints the message object to the message stream.
#}
(defmethod inter~print-message ((interface asi+inter) message)
  (format (asi~message interface) "~A~%" message))

#{ {\vb inter~print-trace} prints the trace object to the trace stream.
#}
(defmethod inter~print-trace ((interface asi+inter) trace)
  (format (asi~trace interface) "; ~A~&" trace))

#{ {\vb inter~print-query} prints the query object to the query stream.
#}
(defmethod inter~print-query ((interface asi+inter) query)
  (format (asi~query interface) "~A" query))


#{ {\vb inter~output-object} formats the object to the output stream, using
 the ~A directive.
#}
(defmethod inter~output-object ((interface asi+inter) object)
  (format (asi~output interface) "~A" object))

(defmethod inter~output-object ((interface asi+inter) (alist list))
  (inter~output-object interface "(")
  (if alist
      (asi=output-nonempty-list interface alist)
    (inter~output-object interface ")")))

(defun asi=output-nonempty-list (interface alist)
  (inter~output-object interface (first alist))
  (if (rest alist)
      (progn (inter~output-object interface " ")
             (asi=output-nonempty-list interface (rest alist)))
    (inter~output-object interface ")")))

#{ {\vb inter~input-object} reads an object from the input stream.  If eof
 is seen, an {\vb inter+error} condition is signaled.  Otherwise the 
 read-function of the given argtype is called on the object that is read.
#}
(defmethod inter~input-object ((interface asi+inter) (type arg+type))
  (let* ((read-function (arg~read-function type))
	 (input (asi~input interface))
	 (eof-sym (gensym))
	 (obj (read input nil eof-sym)))
    (if (eq obj eof-sym)
	(sys~signal
	 (sys~make-condition 'inter+error
			     :format-string "Hit eof on input"
			     :args nil))
      (asi=prompt-with-abort (funcall read-function obj)))))


#{ {\vb inter~input-object-info} reads an object from the input stream.  If eof
 is seen, an {\vb inter+error} condition is signaled.  Otherwise the 
 read-function of the given argtype is called on the object that is read.
 It returns in addition to the object, (object-name) specifying that the object is
 not a default object, and giving the object name. 
#}
(defmethod inter~input-object-info ((interface asi+inter) (type arg+type))
  (let* ((read-function (arg~read-function type))
	 (input (asi~input interface))
	 (eof-sym (gensym))
	 (obj (read input nil eof-sym)))
    (if (eq obj eof-sym)
	(sys~signal
	 (sys~make-condition 'inter+error
			     :format-string "Hit eof on input"
			     :args nil))
      (asi=prompt-with-abort
       (cons (list obj)
	     (funcall read-function obj))))))



(sys~define-condition asi+no-input-error (sys+error)
  ()
  (lambda (cond stream) (declare (ignore cond))
    (format stream "Received no input."))
  (:documentation "Indicates that the user simply entered a return and no input."))

(sys~define-condition asi+abort (com+abort-command)
  ()
  (lambda (cond stream) (declare (ignore cond))
    (format stream "Command execution aborted"))
  (:documentation "Indicates that the user aborted a command."))

(defmethod inter~prompt-for-return ((interface asi+inter) prompt)
  (declare (edited  "17-NOV-1995")
	   (authors Lassaad)
	   (input   "see interface.lisp")
	   (effect  "see interface.lisp")
	   (value   "see interface.lisp"))
  (sys~handler-case
   (progn
     (inter~output-object interface prompt)
     (asi~linereadp (asi~input interface)))
   (asi+no-input-error () (return-from inter~prompt-for-return T))			               
   (inter+error (condition)
		(inter~print-error interface condition))
   (inter+warning (condition)
		  (inter~print-warning interface condition))))

(defmethod inter~prompt-for-command ((interface asi+inter) prompt)
  (declare (edited  "30-JUL-1998")
	   (authors Hess)
	   (input   "see interface.lisp")
	   (effect  "see interface.lisp")
	   (value   "see interface.lisp"))
  (sys~handler-case
   (progn
     (inter~terpri interface)
     (inter~output-object interface prompt)
     (asi~linereadp (asi~input interface)))
   (asi+no-input-error () nil)
   (inter+error (condition)
		(inter~print-error interface condition))
   (inter+warning (condition)
		  (inter~print-warning interface condition))))

(defmethod inter~prompt-for-input-with-default-info ((interface asi+inter) prompt (argtype arg+type) default)
  (declare (edited  "04-MAR-1999" "07-NOV-1995")
	   (authors Pollet Lassaad)
	   (input   "see interface.lisp")
	   (effect  "see interface.lisp")
	   (value   "see interface.lisp"))
  (sys~handler-case
   (progn
     (inter~output-object interface prompt)
     (inter~output-object interface "[")
     (inter~output-object interface default)
     (inter~output-object interface "]")
     (let* ((read-function (arg~read-function argtype))
	    (input (asi~input interface))
	    (obj (car (asi~linereadp input))))
       (asi=prompt-with-abort
	(cons (list obj)
	      (funcall read-function obj)))))
   (asi+no-input-error () (return-from inter~prompt-for-input-with-default-info
			    (let* ((read-function (arg~read-function argtype))
				   (obj (inter~get-symbol default)))
			      (cons "default"
				    (funcall read-function obj)))))
                            ;;; (cons "default" default))) MP: see interface.lisp
                              
   (inter+error (condition)
		(inter~print-error interface condition))
   (inter+warning (condition)
		  (inter~print-warning interface condition))))


(defmethod inter~prompt-for-input-with-default ((interface asi+inter) prompt (argtype arg+type) default)
  (sys~handler-case
   (progn
     (inter~output-object interface prompt)
     (inter~output-object interface "[")
     (inter~output-object interface default)
     (inter~output-object interface "]")
     (let* ((read-function (arg~read-function argtype))
	    (input (asi~input interface))
	    (obj (car (asi~linereadp input))))
       (asi=prompt-with-abort
	(funcall read-function obj))))
   (asi+no-input-error () (return-from inter~prompt-for-input-with-default default))
   (inter+error (condition)
		(inter~print-error interface condition))
   (inter+warning (condition)
		  (inter~print-warning interface condition))))
  
#{Here we define a function to get forms from the input stream.
{\vb asi~linereadp} is based on the token-reading rules given in CLTL.
#}
(defun asi~linereadp (&optional (input-stream *standard-input*))
  (declare
   (authors nesmith Lassaad)
   (input "an optional stream (defaults to *standard-input*)")
   (value "a list of input objects contained on the line(s), when the end of "
	  "file is not reached; Otherwise nil" )
   (effect "an input line is read from the stream. If an object is not finished,"
           "another line is read (ad infinitum)(until end of file is reached)"))
  (labels ((lisp-ize (tokens)
	     (mapcar #'(lambda (x) (with-input-from-string (in x) (read in)))
		     tokens))
	   (tokenize-line (str)
	     (do ((next-start 0)
		  (last-start 0)
		  (len (length str))
		  (new-token t)
		  (tokens nil (if (and new-token (not (zerop (length new-token))))
				  (cons new-token tokens) 
				tokens)))
		 ((or (null new-token) 
		      (= len next-start))
		  (values (nreverse tokens) 
			  (if (null new-token) (subseq str last-start))
			  (if new-token t nil)))
	       (declare (special len))
	       (setq last-start next-start)
	       (multiple-value-setq (new-token next-start)
		 (get-next-token str next-start))))
	   (get-next-token (str begin)
	     ;;"Input string STR and index BEGIN. 
	     ;;Output: If we can get a full token from STR beginning at BEGIN, returns the 
	     ;;token (a string) and the index of STR at which the token ends.  Otherwise
	     ;;returns NIL and BEGIN."
	     (declare (special len))
	     ;; assume str is not empty string and first elt is not whitespace
	     (let ((char (char str begin)))
	       (cond ((eql char #\")	
		      (read-a-string-token str begin))
		     ((or (eql char #\')	
			  (eql char #\`)
			  (eql char #\,))
		      (multiple-value-bind (token end-index)
			  (get-next-token str (1+ begin))
			(if token (values (concatenate 'string (string char)
						       token)
					  end-index))))
		     ((eql char #\()	
		      (read-a-list-token str begin))
		     ((eql char #\))	
		      (values "" (next-non-whitespace-char str (1+ begin))))
		     ((eql char #\#)
		      (read-a-dispatch-macro-token str begin))
		     ((eql char #\\)	
		      (read-a-single-escape-token str begin))
		     ((eql char #\|)	
		      (accumulate-token-even-multiple str begin ""))
		     ((eql char #\;) 
		      (values "" (length str)))
		     (t
		      ;; hope it's a constituent character! 
		      (accumulate-token-even-multiple str begin "")))))
	   (read-a-dispatch-macro-token (str begin)
	     "know that begin'th char of str is a #"
	     (declare (special len))
	     (let ((ch (if (> len (1+ begin)) (char str (1+ begin)))))
	       (cond ((null ch) nil)
		     ((whitespace-char-p ch)
		      (values (subseq str begin (+ 2 begin)) 
			      (next-non-whitespace-char str (+ 2 begin))))
		     ((eql ch #\\)
		      ;; this should be a char object
		      (multiple-value-bind (token end-index)
			  (read-a-single-escape-token str (1+ begin))
			(if token (values (concatenate 'string "#"
						       token)
					  end-index))))
		     ((digit-char-p ch)
		      (let* ((end-of-num
			      (position-if-not  #'digit-char-p
						str :start (+ 1 begin)))
			     (next-ch (if (> len end-of-num) (char str end-of-num))))
			(cond ((or (null next-ch)
				   (whitespace-char-p next-ch))
			       ;; just send back the # and the number
			       (values (subseq str begin end-of-num)
				       (next-non-whitespace-char str
								 end-of-num)))
			      ((member next-ch '(#\( #\*))
			       ;; vectors
			       (multiple-value-bind (token end-index)
				   (get-next-token str end-of-num)
				 (if token (values (concatenate 'string 
						     (subseq str begin end-of-num)
						     token)
						   end-index))))
			      ((eql next-ch #\#)
			       (values (subseq str begin (1+ end-of-num))
				       (next-non-whitespace-char str
								 (1+ end-of-num))))
			      (t
			       ;; includes rational, array or ref
			       (multiple-value-bind (token end-index)
				   (get-next-token str (next-non-whitespace-char
							str
							(1+ end-of-num)))
				 (if token
				     (values (concatenate 'string
					       (subseq str begin (1+ end-of-num))
					       token)
					     end-index)))))))
		     ((eql  ch #\()
		      ;; vectors without numeric arg
		      (multiple-value-bind (token end-index)
			  (get-next-token str (1+ begin))
			(if token (values (concatenate 'string "#"
						       token)
					  end-index))))
		     ((eql ch #\|)
		      (let ((next-start
			     (read-macro-comment str (+ 2 begin))))
			(if next-start (values "" next-start))))
		     ((member ch '(#\* #\' #\, #\: #\. #\B #\b #\O #\o #\X #\x
				   #\S #\s #\+ #\- ))
		      ;; things which have a char then a form
		      ;; e.g., #*, #.,  #'
		      (let ((next-spot
			     (next-non-whitespace-char str (+ 2 begin)))
			    (token nil))
			(when (< next-spot len)
			  (multiple-value-setq (token next-spot)
			    (get-next-token str next-spot))
			  (when token (values (concatenate 'string 
						(subseq str begin (+ 2 begin))
						token)
					      next-spot)))))
		     (t
		      (multiple-value-bind (token end-index)
			  (get-next-token str (1+ begin))
			(if token 
			    (values (concatenate 'string "#" token)
				    end-index)
			  (values (subseq str begin (+ 2 begin))
				  (next-non-whitespace-char str (+ 2 begin)))))))))
	   (read-a-string-token (str begin)
	     (multiple-value-bind (token next-start)
		 (read-until-token str (1+ begin) #\")
	       (when token
		 (values (concatenate 'string "\"" token) next-start))))
	   (read-a-list-token (str begin)
	     "Read a list in as a single token."
	     (declare (special len))
	     (do ((token t)
		  (index (1+ begin)))
		 ((or (null token)
		      (>= index len)
		      (eql (char str index) #\)))
		  (if (or (null token)
			  (>= index len))
		      nil
		    (values (subseq str begin (1+ index))
			    (next-non-whitespace-char str (1+ index)))))
	       (multiple-value-setq (token index)
		 (get-next-token str index))))
	   (read-until-token (str begin closech)
	     "Return the substring of STR from BEGIN to the next unescaped 
              occurrence of CLOSECH.  Also return position of next non-whitespace char after
              that.  If no CLOSECH occurs, return NIL."
	     (declare (special len))
	     (do  ((index begin))
		 ((or (>= index len)
		      (eql (char str index) closech))
		  (if (= index len) 
		      nil
		    (values (subseq str begin (1+ index))
			    (next-non-whitespace-char str (1+ index)))))
	       (if (eql (char str index) #\\)
		   (incf index 2)
		 (incf index 1))))
	   (read-macro-comment (str begin) 
	     "This is to read comments which are delimited by \#\| and \|\#.
              Just skips over them, returning no tokens."
	     (declare (special len))
	     (do  ((index begin)
		   (num-found 1))
		 ((or (>= (1+ index) len)
		      (zerop num-found))
		  (if (zerop num-found)
		      (next-non-whitespace-char str index)
		    nil))
	       (cond ((eql (char str index) #\\)
		      (incf index 2))
		     ((and (eql (char str index) #\|)
			   (eql (char str (1+ index)) #\#))
		      (decf num-found)
		      (incf index 2))
		     ((and (eql (char str index) #\#)
			   (eql (char str (1+ index)) #\|))
		      (incf num-found)
		      (incf index 2))
		     (t (incf index 1)))))
	   (whitespace-char-p (ch)
	     (member ch '(#\tab #\space #\page #\return #\newline #\linefeed)))
	   (next-non-whitespace-char (str index)
	     (declare (special len))
	     (do ((index index (1+ index)))
		 ((or (= index len)
		      (not (whitespace-char-p (char str index))))
		  index)))
	   (read-a-single-escape-token (str begin)
	     "Rule 5 on page 335 of CLtL."
	     (when (not (= begin (1- (length str))))
	       (accumulate-token-even-multiple str (+ begin 2)
					       (subseq str begin (+ begin 2)))))
	   (accumulate-token-even-multiple (str begin token-so-far)
	     "Rule 8 on page 337 of CLtL."
	     (declare (special len))
	     (let ((ch (if (< begin len) (char str begin))))
	       (cond ((not ch)
		      (values token-so-far begin))
		     ((eql ch #\\)
		      (if (= len (1+ begin))
			  nil
			(accumulate-token-even-multiple 
			 str (+ begin 2)
			 (concatenate 'string 
			   token-so-far
			   (subseq str begin (+ begin 2))))))
		     ((eql ch #\|)
		      (accumulate-token-odd-multiple str (1+ begin)
						     (concatenate 'string
						       token-so-far (string
								     ch))))
		     ((member ch '(#\' #\( #\) #\" #\; #\`)
			      :test #'eql)
		      (values token-so-far begin))
		     ((whitespace-char-p ch)
		      (values token-so-far (next-non-whitespace-char str begin)))
		     (t
		      ;; should be constituent, or non-terminating macro character
		      (accumulate-token-even-multiple str (1+ begin)
						      (concatenate 'string
							token-so-far (string
								      ch)))))))
	   (accumulate-token-odd-multiple (str begin token-so-far)
	     "Rule 9 on page 337 of CLtL."
	     (declare (special len))
	     (let ((ch (if (< begin len) (char str begin))))
	       (cond ((not ch) nil)
		     ((eql ch #\|)
		      (accumulate-token-even-multiple str (1+ begin)
						      (concatenate 'string
							token-so-far (string
								      ch))))
		     ((eql ch #\\)
		      (if (= len (1+ begin))
			  nil
			(accumulate-token-odd-multiple 
			 str (+ begin 2)
			 (concatenate 'string 
			   token-so-far
			   (subseq str begin (+ begin 2))))))
		     (t 
		      (accumulate-token-odd-multiple str (1+ begin)
						     (concatenate 'string
						       token-so-far (string
								     ch)))))))

	   )
    (let ((eof-value (cons nil nil)))
      (do ((instring (read-line input-stream nil eof-value)
		     (unless finished-p
		       (concatenate 'string unfinished-part
				    (string #\newline)
				    (read-line input-stream nil 
					       eof-value))))
	   (new-tokens nil)
	   (tokens nil
		   (nconc tokens new-tokens))
	   (unfinished-part "")
	   (finished-p nil))
	  ((or (eql instring eof-value) finished-p)
	   (cond ((eql instring eof-value) nil)
		 (t (if tokens
			(lisp-ize tokens)
		      (sys~signal 'asi+no-input-error)))))
	(multiple-value-setq (new-tokens unfinished-part finished-p)
	  (tokenize-line instring))))))


(defmethod inter~input-query ((interface asi+inter) (prompt string) (argtype symbol))
  (declare (edited  "02-FEB-1998")
	   (authors Lassaad))
  (let ((type (arg~find-argtype argtype)))
    (if type
        (inter~input-query interface prompt type)
      (error "~A is not a valid ARGTYPE." argtype))))

(defmethod inter~input-query ((interface asi+inter) (prompt string) (argtype arg+type))
  (declare (edited  "02-FEB-1998")
	   (authors Lassaad))
  (format (asi~query interface) prompt)
  (let* ((read-function (arg~read-function argtype))
	 (input (asi~query interface))
	 (obj (read input)))
    (asi=prompt-with-abort
     (funcall read-function obj))))
  
(defmethod inter~choice-query ((interface asi+inter) (prompt string) (choices list))
  (declare (edited  "02-FEB-1998")
	   (authors Lassaad))
  (let* ((stream (asi~query interface))
	 (posint-type (arg~find-argtype 'posinteger))
	 (read-function (arg~read-function posint-type))
	 (obj nil) 
	 (n 1)
	 (newchoices nil))
    (dolist (choice choices (setq newchoices (nreverse newchoices)))
      (push (cons n choice) newchoices)
      (incf n))
    (loop
     (sys~handler-bind
      ((arg+input-error 
	#'(lambda (c) (inter~print-error interface c) nil)))
      (format stream prompt)
      (format stream "~%")
      (format stream "Choices are: ")
      (format stream "~%")
      (dolist (choice newchoices)
	(format stream "~A: ~A~%"
		(car choice)
		(etypecase (cdr choice)
		  (string (cdr choice))
		  (symbol (symbol-name (cdr choice)))
		  (number (cdr choice))
		  (keim+name (keim~name (cdr choice))))))
      (let* ((obj (asi=prompt-with-abort (funcall read-function (read stream))))
	     (found (find obj newchoices :key #'car)))
	(if found
	    (return-from inter~choice-query (cdr found))
	  (inter~print-error
	   interface
	   (sys~make-condition
	    'inter+error
	    :format-string
	    "You must enter a positive integer between 1 and ~D"
	    :interface interface 
	    :args (list (length choices))))))))
    ))
  
(defgeneric asi~yes-no-query (interface prompt)
  (declare (edited  "02-FEB-1998")
	   (authors Lassaad Sorge)
	   (input   "An interface and a string.")
	   (effect  "Writes the string to the query stream of INTERFACE"
		    "and waits for a decisive answer.")
	   (value   "T or NIL."))
  (:method ((interface asi+inter) (prompt string))
	   (format (asi~query interface) prompt)
	   (format (asi~query interface) "[Y/N]: ")
	   (loop
	    (sys~handler-bind
	     ((arg+input-error 
	       #'(lambda (c) (inter~print-error interface c) nil)))
	     (let ((answer (read (asi~query interface))))
	       (cond ((find answer '(Y YES T TRUE J JA JAWOHL "1") :test #'string-equal)
		      (return-from asi~yes-no-query T))
		     ((find answer '(N NO NIL FALSE NEIN NIX "0") :test #'string-equal)
		      (return-from asi~yes-no-query NIL))
		     (T (inter~print-error
			 interface
			 (sys~make-condition
			  'inter+error
			  :format-string "Please enter [Y]es or [N]o!"
			  :interface interface
			  :args ())))))))))












