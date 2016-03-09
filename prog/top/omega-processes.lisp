;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The new OMEGA architecture using LISP multiprocessing
;; =====================================================
;;
;; OMEGA consists of four independent processes:
;; - The OMEGA core process that executes commands
;; - The OMEGA command interpreter which accepts command input
;;   from the regular ascii interface
;; - The LOUI listener which handles the communication with LOUI
;; - And the service listener for communication with Mathweb
;;
;;  The OMEGA core process coordination between the other processes.
;;  It especially maintains a blackboard with requests for command
;;  executions and performs these sequentially.
;;
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; We could have several LOUI listeners working on the same OMEGA core.
;; For that we should make the LOUI listener handling (functions +
;; global variables) more generic.
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)


(mod~defmod OPR 
            :uses (arg asi atptop bb com comint inter keim mixin omega proc socket sys view)
            :documentation "Mechanism for multiprocessing OMEGA."
            :exports (opr+abort
                      opr+abort
                      opr+abort-return-object
                      opr+core-blackboard
                      
                      opr~active-p
                      opr~arrest-listener
                      opr~bb-command-queue
                      opr~create-core-blackboard
                      opr~enqueue-command
                      opr~execute-command
                      opr~initialize-listeners
                      opr~initialize-omega
                      opr~lisp-listener-p
                      opr~listener-stop
                      opr~loui-listener-p
		      opr~http-listener-p
                      opr~make-core-blackboard
                      opr~normalize-command
                      opr~pop-command
                      opr~process2socket
                      opr~quit
                      opr~release-listener
                      opr~reset
                      opr~reset-core-blackboard
                      opr~runtime-object
                      opr~service-listener-p
                      opr~signal-interrupt
                      opr~socket2process
                      opr~start
                      opr~stop-listener
                      opr~string2command
                      
                      opr*active
                      opr*calling-listener
                      opr*core-blackboard
                      opr*halt-loui-listener
                      opr*halt-service-listener
                      opr*interrupt-command
                      opr*lisp-listener
                      opr*http-listener
                      opr*listener-names
                      opr*lock
                      opr*loui-listener
                      opr*not-killed
                      opr*not-revoke-ascii-interface
                      opr*omega-core-process
                      opr*quit
                      opr*reset
                      opr*runtime-object
                      opr*service-listener))

;;; The following functions are internal in other modules and should not be used:
;;; (omega=output omega=str2msg omega=write2loui)

(eval-when (load compile eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   The class of core blackboards 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defclass opr+core-blackboard () 
 ((command-queue :accessor opr~bb-command-queue
		  :initarg :command-queue
		  :initform nil
		  :documentation "A priority queue of command execution requests."))
  (:documentation "The class of blackboards the OMEGA core process handles commands with."))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Global Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar opr*active nil "OMEGA multiprocessing active or not.")
(defvar opr*on-loui T "OMEGA is connected to the LOUI interface or not.")
(defvar opr*not-killed nil "OMEGA multiprocessing has been intentionally killed or not.")

(defvar opr*core-blackboard nil "The anchor for the core blackboard maintaining the commands.")

(defvar opr*lisp-listener nil "Accesses the lisp listener.")
(defvar opr*omega-core-process nil "Accesses the core process.")
(defvar opr*loui-listener nil "Accesses the loui socket listener.")
(defvar opr*service-listener nil "Accesses the mathweb socket listener.")
(defvar opr*http-listener nil "Accesses the http socket listener.")

(defvar opr*quit nil "Quitting multiprocessing.")
(defvar opr*reset nil "Reseting multiprocessing.")

(defvar opr*interrupt-command :interrupt
  "The command used for interrupting arbitary computations.")

(defvar opr*halt-lisp-listener '(:lisp :exit :exit-lisp :break)
  "A list of commands where the ascii-interface should wait for it to finish (must not include interrupt).")
(defvar opr*halt-loui-listener
  '(:socketread-problem :socketread-pds :socketread-resolution-proof :socketload-file :receive-atp-out)
  "A list of commands where the loui-listener should wait for it to finish.")

(defvar opr*halt-service-listener '(:plan :nsp
					  :s
					  :search
					  :show-constraints
					  :read-problem :execute-theory-log :execute-theory-log*
					  :solve-analogous-to-proof :solve-analogous-to-node)
  "A list of commands where the loui-listener should wait for it to finish.")
(defvar opr*halt-http-listener '(:plan :nsp :execute-theory-log :execute-theory-log*
					  :solve-analogous-to-proof :solve-analogous-to-node)
  "A list of commands where the http-listener should wait for it to finish.")

(defvar opr*runtime-object nil "The last runtime-object saved.")

(defvar opr*lock (proc~make-process-lock) "A lock for protecting operations of the core process")

;;(defvar opr*listener-names '(:inout :service :http) ; jzimmer 7.10.2000
;; "The names of the currently used listeners: :INOUT is the Loui listener, :SERVICE the Mathweb listener.")
(defvar opr*listener-names '(:inout :http)
  "The names of the currently used listeners: :INOUT is the Loui listener, :SERVICE the Mathweb listener.")

(defvar opr*calling-listener nil
  "The name of the listener that has submitted the command that is currently executed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  The socket listener processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;; Querying and such

(eval-when (load compile eval)

(defmacro opr~lisp-listener-p (process)
  `(eq ,process opr*lisp-listener))

(defmacro opr~loui-listener-p (process)
  `(and opr*loui-listener (eq ,process opr*loui-listener)))

(defmacro opr~service-listener-p (process)
  `(and opr*service-listener (eq ,process opr*service-listener)))

(defmacro opr~http-listener-p (process)
  `(and opr*http-listener (eq ,process opr*http-listener)))
)

(defun opr~socket2process (socketname)
  (cond ((string-equal socketname :inout) opr*loui-listener)
	((string-equal socketname :service) opr*service-listener)
	((string-equal socketname :http) opr*http-listener)
	(t (omega~error "Unknown socket: ~A" socketname))))

(defun opr~process2socket (process)
  (cond ((opr~loui-listener-p process) :inout)
	((opr~loui-listener-p process) :http)
	((opr~service-listener-p process) :service)))

;;; Handling and maintaining
(defun opr=reset-socket-listeners ()
  (declare (edited  "13-DEC-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Resets the socket listeners.")
	   (value   "Undefined."))
  (when opr*loui-listener
    (opr~listener-stop opr*loui-listener)
    (setf opr*loui-listener (opr=listener-create :inout)))
  (when opr*http-listener
    (opr~listener-stop opr*http-listener)
    (setf opr*http-listener (opr=listener-create :http 'opr=http-listener-function)))
  (when opr*service-listener
    (opr~listener-stop opr*service-listener)
    (setf opr*service-listener (opr=listener-create :service))))

(defun opr~listener-stop (listener)
  (declare (edited  "13-DEC-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Stops the socket listeners.")
	   (value   "Undefined."))
  (when (proc~p listener) (proc~kill listener)))

(defun opr=listener-create (socketname &optional (func 'opr=listener-function))
  (declare (edited  "13-DEC-1999")
	   (authors Sorge)
	   (input   "A socketname.")
	   (effect  "Creates a process listening on the socket specified by socketname.")
	   (value   "The listener process."))
  (proc~create :name (format nil "Listener (~A)" socketname)
	       :function func
	       :priority 200
	       :args (socketname)))

(defun opr=listener-function (socketname)
  (declare (edited  "13-DEC-1999")
	   (authors Sorge)
	   (input   "A socketname.")
	   (effect  "Loops for input available at the given socket and passes it on.")
	   (value   "Undefined."))
  (loop
   (with-simple-restart
    (abort "Return to OMEGA top level")
    (unless (socket~active? socketname)
      (return (omega~warn "Socket ~A is not longer active!" socketname)))
    (if (comint~debugger-on-break comint*current-comint)
	(progn
	  (proc~socket-wait-for-input-available socketname
						(format nil "Waiting for input on socket ~A" socketname))
	  (let ((string (socket~read socketname))
		(process (opr~socket2process socketname)))
	    (multiple-value-bind (command arguments)
		(opr~string2command string)
	      (when (and (not (opr~active-p)) opr*not-killed)
		(opr~initialize-omega))
	       (opr~enqueue-command
		(opr~normalize-command command :args arguments :process process))
	       (opr~stop-listener command process))))
      (sys~handler-case
       (progn
	 (proc~socket-wait-for-input-available socketname
					       (format nil "Waiting for input on socket ~A" socketname))
	 (let ((string (socket~read socketname))
	       (process (opr~socket2process socketname)))
	   (multiple-value-bind (command arguments)
	       (opr~string2command string)
	     (when (and (not (opr~active-p)) opr*not-killed)
	       (opr~initialize-omega))
	      (opr~enqueue-command
	       (opr~normalize-command command :args arguments :process process))
	      (opr~stop-listener command process))))
       (simple-error (c) (omega~error "~A" c))
       (error (c) (omega~error "~A " c)))))))

(defun opr=http-listener-function (socketname)
  (declare (edited  "13-DEC-1999")
	   (authors Sorge)
	   (input   "A socketname.")
	   (effect  "Loops for input available at the given socket and creates a process for the clientsocket.")
	   (value   "Undefined.")) 
  (loop
   (with-simple-restart
    (abort "Return to OMEGA top level")
    (unless (socket~active? socketname)
      (return (omega~warn "Socket ~A is not longer active!" socketname)))
     (let ((clientname (gentemp 'httpclient)))
       (socket~define clientname)
       (proc~socket-wait-for-input-available socketname
					     (format nil "Waiting for connect on socket ~A" :http))
       (socket~accept socketname clientname)
       (opr=listener-create clientname 'opr=httpclient-listener-function)))))
       
(defun opr=httpclient-listener-function (socketname)
  (declare (edited  "13-DEC-1999")
	   (authors Sorge)
	   (input   "A socketname.")
	   (effect  "Looks for input available passes it on.")
	   (value   "Undefined.")) 
  (sys~handler-case
   (progn
     (proc~socket-wait-for-input-available socketname
					   (format nil "Waiting for input on socket ~A" socketname))
     (let* ((string (http~read-page socketname))
	    (httpcommand (car (car string)))
	    (httpuri (car (rest (car string)))))
       (if (eq httpcommand 'post)
	   (if (eq httpuri '/rpc2)
	       (let* ((content (third string))
		      (com (rpc~parse-methodcall content))
		      (process socketname))
		 (multiple-value-bind (command arguments)
		     (opr=list2command com)
		   (when (and (not (opr~active-p)) opr*not-killed)
		     (opr~initialize-omega))
		   (opr~enqueue-command
		    (opr~normalize-command command :args arguments :process process))))
	     (progn (http~send-error socketname 404)
		   (socket~close socketname)
		   (socket~delete socketname)))
	 (progn (http~send-error  socketname 501)
		(socket~close socketname)
		(socket~delete socketname)))))
   (simple-error (c)
		 (progn (http~send-error socketname 500)
			(socket~close socketname)
			(socket~delete socketname)
			(omega~error "~A" c)))
   (error (c)
	  (progn (http~send-error socketname  500)
		 (socket~close socketname)
		 (socket~delete socketname)
		 (omega~error "~A" c)))))


(defun opr=list2command (liste) ;;special version of string2command for RPC, we don't want to loose strings!
  (unless (null liste)
    (let* ((com (car liste))
	   (args (cdr liste))
	   (command (com~read-command com))
	   (argtypes (com~argtypes command)))
      (values
       command
       (mapcar #'(lambda (arg type)
		   (funcall (arg~read-function (arg~find-argtype type))
			    ;(if (stringp arg) (read-from-string arg)
			      arg))
	       args argtypes)))))

(defun opr~string2command (string)          ;;; has to be made error prove!
  (unless (equal string "")
    (let* ((str-list (opr=divide-string ;; (subseq string 0 (1- (length string)))))
		                        string))
	   (com (car str-list))
	   (args (cdr str-list))
	   (command (com~read-command (read-from-string com)))
	   (argtypes (com~argtypes command)))
      (values
       command
       (mapcar #'(lambda (arg type)
		   (funcall (arg~read-function (arg~find-argtype type))
			    (read-from-string arg)))
	       args argtypes)))))
      
(defun opr=divide-string (string)
  (do ((new-str "")
       (rest-str string (string-left-trim '(#\SPACE) rest-str))
       (str-list nil (append str-list (list new-str))))
      ((equal rest-str "") str-list)
    (if (equal (char rest-str 0) #\()
	(multiple-value-bind (list length)
	    (read-from-string rest-str)
	  (setf rest-str (subseq rest-str length))

	  (setf new-str (format nil "~S" list)))
      (multiple-value-setq (new-str rest-str)
	(atptop~get-next-word rest-str #\SPACE)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A little mechanism to abort arbitrary executions. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sys~define-condition opr+abort (sys+abort)
		      ((return-object nil)
		       (caller opr*lisp-listener))
		      (lambda (cond stream)
			(format stream "Execution aborted, the runtime-object ~A was saved." (opr+abort-return-object cond))))

(defun opr~signal-interrupt (&optional (object nil))
  (declare (edited  "10-DEC-1999")
	   (authors Sorge)
	   (input   "A runtime-object.")
	   (effect  "Aborts the current execution if this was signalled by the user.")
	   (value   "Undefined."))
  (opr~release-listener)
  (opr=remove-special-commands opr*halt-lisp-listener)
  (when (opr=remove-special-commands opr*halt-loui-listener)
    (socket~read)
    (opr~release-listener opr*loui-listener))
  (opr=remove-special-commands opr*halt-service-listener)
  (let* ((command (opr=find-interrupt-on-board opr*core-blackboard))
	 (caller (third command)))
    (when command
      (sys~signal (sys~make-condition 'opr+abort
				      :return-object object
				      :caller caller
				      )))))

(defun opr=remove-special-commands (&optional (coms opr*halt-lisp-listener)
					      (bb opr*core-blackboard))
  (let* ((change nil)
	 (new-queue
	  (remove-if #'(lambda (x)
			 (let ((command (car x)))
			   (and (com~command-p command)
				(find (keim~name command) coms :test #'string-equal)
				(setf change t))))
		     (opr~bb-command-queue bb))))
    (when change
      (setf (opr~bb-command-queue bb) new-queue))
    change))

(defun opr=find-interrupt-on-board (&optional (bb opr*core-blackboard))
  (let* ((queue (opr~bb-command-queue bb))
	 (command (find-if #'(lambda (x)
			       (let ((com (car x)))
				 (and (com~command-p com)
				      (string-equal (keim~name com) opr*interrupt-command))))
			   queue)))
    (when command
      (setf (opr~bb-command-queue bb) nil)  ;;(remove command queue))
      command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; OUTPUT Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output redirection for the standard-output stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)

(defmacro opr=with-output-to-loui (body)
  `(if opr*on-loui
       (let ((*standard-output* (make-string-output-stream))
	 ;;;(*trace-output* (make-string-output-stream))
	     )
	 ,body
	 (let ((strings1 (atptop~divide-string
			  (get-output-stream-string *standard-output*)
			  #\NEWLINE :handle-break-char 'pre))
	   ;;;(str2 (get-output-stream-string *trace-output*))
	       )
	   (dolist (str1 strings1)
	     (unless (equal str1 "") (socket~write (concatenate 'string "browseOutput(" (write-to-string str1) ")"))))
       ;;;(unless (equal str2 "") (socket~write (concatenate 'string "browseTrace(" (write-to-string str2) ")")))
	   ))
     ,body))
)

(defmethod asi~output :around (interface)
  (if (opr~loui-listener-p opr*calling-listener)
      *standard-output*
    (call-next-method)))

(defmethod asi~message :around (interface)
  (if (opr~loui-listener-p opr*calling-listener)
      *standard-output*
    (call-next-method)))

(defmethod asi~error :around (interface)
  (if (opr~loui-listener-p opr*calling-listener)
      *standard-output*
    (call-next-method)))

(defmethod asi~warn :around (interface)
  (if (opr~loui-listener-p opr*calling-listener)
      *standard-output*
    (call-next-method)))

(defmethod asi~trace :around (interface)
  (if (opr~loui-listener-p opr*calling-listener)
      *standard-output*
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions adapted from the OMEGA output module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)

(defmacro omega=output (stream)
  (declare (edited  "03-MAR-1998")
	   (authors Sorge Konrad)
	   (input   "A function specifying the interface-stream.")
	   (effect  "Prints arguments to the specified stream of the OMEGA interface.")
	   (value   "NIL"))
  `(let ((interface (if (and (boundp 'comint*current-comint) comint*current-comint)
			(comint~interface comint*current-comint)
		      (asi~create))))
     (,stream interface (apply #'format nil args))
     nil))

(defmacro omega=write2loui (label)
  `(let ((str (format nil "~A~%" (apply #'format nil args))))
     (socket~write  (omega=str2msg ,label str) :inout)
  nil)))

(defun omega=str2msg (label str)
  (concatenate 'string  label "(" (write-to-string str) ")"))

(defun omega~output (&rest args)
  (cond ((opr~loui-listener-p opr*calling-listener)
	 (omega=write2loui "browseOutput"))
	(t (omega=output inter~output-object))))

(defun omega~message (&rest args)
  (cond ((opr~loui-listener-p opr*calling-listener)
	 (omega=write2loui "browseMessage"))
	(t (omega=output inter~print-message))))
  
(defun omega~warn (&rest args)
  (cond ((opr~loui-listener-p opr*calling-listener)
	 (omega=write2loui "browseWarning"))
	(t (omega=output inter~print-warning))))
		         
(defun omega~error (&rest args)
  (cond ((opr~loui-listener-p opr*calling-listener)
	 (omega=write2loui "browseError"))
	(t
	 (let ((interface (if (and (boundp 'comint*current-comint) comint*current-comint)
			      (comint~interface comint*current-comint)
			    (asi~create))))
    (cond ((typep (car args) 'condition)
	   (inter~print-error interface (car args)))
	  ((= (length args) 1) 
	   (inter~print-error interface (string (car args))))
	  (T (inter~print-error interface (apply #'format nil args))))
	   nil))))

(defun omega~trace (&rest args)
  (cond ((opr~loui-listener-p opr*calling-listener)
	 (omega=write2loui "browseTrace"))
	(t (omega=output inter~print-trace))))

(defun omega~query (&rest args)
  (declare (edited  "20-APR-2000")
	   (authors Pollet)
	   (input   "Three different possibilities:\\"
		    "1. Just the arguments STRING ARGTYPE DEFAULT\\"
		    "2. A list with one query (STRING ARGTYPE DEFAULT)\\"
		    "3. More than one list (S1 A1 D1)(S2 A2 D2)...\\"
		    "STRING and ARGTYPE are always required, DEFAULT is optional")
	   (effect  "Called from LOUI a dialog-window will be displayed,"
		    "in Emacs the user is prompted for input.")
	   (value   "If the user-input is of the right ARGTYPE\\"
		    "1. the value that the user entered\\"
		    "2. a list with one value\\"
		    "3. a list with values for each query\\"
		    "else an error-message."))
  (let ((queries (if (listp (car args)) args (list args))))
	(cond ((opr~loui-listener-p opr*calling-listener)
	       (opr~arrest-listener opr*loui-listener)
	       (opr~arrest-listener opr*service-listener)
	       (let ((prompt)(argdefaults)(argtypes))
		 (dolist (query queries)
		   (push (car query) prompt)
		   (push (if (arg~argtype-p (second query))(second query)
			   (arg~find-argtype (second query))) argtypes)
		   (push (third query) argdefaults))
		 (let ((result (omega=query  prompt argtypes argdefaults)))
		   (opr~release-listener opr*loui-listener)
		   (opr~release-listener opr*service-listener)
		   (if 	(listp (car args)) result (car result)))))
	      (t (let ((interface (if (and (not (boundp 'comint*current-comint))  comint*current-comint)
				      (comint~interface comint*current-comint)
				    (asi~create))))
		   (opr~arrest-listener)
		   (let ((result (mapcar #'(lambda (arg)
					     (if (= (length arg) 3)
						 (inter~prompt-for-input-with-default
						  interface (car arg)(second arg)(third arg))
					       (inter~input-query
						interface (car arg)(second arg))))
					 queries)))
			 (opr~release-listener)
			 (if (listp (car args)) result (car result))))))))

(defun omega=query (prompt argtypes argdefaults)
  (let ((result (loui~query prompt
			    (mapcar #'inter~get-symbol argtypes)
			    (mapcar #'inter~get-symbol argdefaults))))
    (mapcar #'(lambda (res type prom def)
		(sys~handler-case
		 (funcall (arg~read-function type) res)
		 (arg+input-error ()
				  (car
				   (omega=query
				    (list (format nil "WRONG ARGTYPE, PLEASE RE-ENTER:~%~A" prom))
				    (list  type)
				    (list  def))))))
	    result argtypes prompt argdefaults)))

(defun omega~query-choice (text choices &optional defaultnumber)
  (declare (edited  "08-MAY-2000" )
	   (authors Pollet)
	   (input "A TEXT with explanations, a list of different CHOICES, the user can"
		  "choose from, and optionally a DEFAULTNUMBER.")
	   (effect "Called from LOUI a dialog-window will be displayed, where the element"
		   "that corresponds to the DEFAULTNUMBER is highlighted, in Emacs the"
		   "user is prompted for input with the default DEFAULTNUMBER.")
	   (value   "The position of the element in the CHOICES that was chosen."
		    "(The first element has position 0 ...)"))
  (cond ((opr~loui-listener-p opr*calling-listener)
	 (opr~arrest-listener opr*loui-listener)
	 (opr~arrest-listener opr*service-listener)
	 (let ((result (loui~casequery text choices :default defaultnumber)))
	   (opr~release-listener opr*loui-listener)
	   (opr~release-listener opr*service-listener)
	   result))
	(t
	 (omega~message text)
	 (omega~message "~%")
 	 (do ((choice choices (rest choice))
	      (number 1 (1+ number)))
	     ((null choice))
	   (omega~message "(~A): ~A~%"
			  number (car choice)))
	 (let ((interface (if (and (not (boundp 'comint*current-comint))  comint*current-comint)
			      (comint~interface comint*current-comint)
			    (asi~create))))
	   (opr~arrest-listener)
	       (let ((number (1- (if defaultnumber 
				     (inter~prompt-for-input-with-default
				      interface "Enter a number: " 'posinteger (1+ defaultnumber))
				   (inter~input-query
				    interface "Enter a number: " 'posinteger)))))
		 (opr~release-listener)
		 number)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theory Output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keim::th=message (&rest args)
  (cond ((opr~loui-listener-p opr*calling-listener)
	 (omega=write2loui "browseOutput"))
	(t (omega=output inter~print-message))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locking the Listeners
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opr~arrest-listener (&optional (listener opr*lisp-listener))
  (when (proc~p listener)
    (proc~add-arrest-reason listener :send-command)))

(defun opr~release-listener (&optional (listener opr*lisp-listener))
  (when (and (proc~p listener) (proc~arrest-reasons listener))
    (proc~revoke-arrest-reason listener :send-command)))

(defgeneric opr~stop-listener (command process)
  (:method (command process)
	   (cond ((and (opr~loui-listener-p process)
		       (find command opr*halt-loui-listener :test #'string-equal))
		  (opr~arrest-listener opr*loui-listener))
		 ((and (opr~service-listener-p process)
		       (find command opr*halt-service-listener :test #'string-equal))
		  (opr~arrest-listener opr*service-listener))
		 ((and (opr~lisp-listener-p process)
		       (find command opr*halt-lisp-listener :test #'string-equal)
		       (opr~arrest-listener opr*lisp-listener)))))
  (:method ((command com+command) process)
	   (opr~stop-listener (keim~name command) process))
  (:method ((command null) process)
	   (declare (ignore process))))


(defgeneric opr~arrest-service (command)
  (:method ((command com+command))
	   (opr~arrest-service (keim~name command)))
  (:method ((command string))
	   (when (and opr*service-listener
		      (proc~is-active opr*service-listener)
		      (find command opr*halt-service-listener :test #'string-equal))
	     (opr~arrest-listener opr*service-listener)))
  (:method ((command symbol))
	   (when (and opr*service-listener
		      (proc~is-active opr*service-listener)
		      (find command opr*halt-service-listener :test #'string-equal))
	     (opr~arrest-listener opr*service-listener))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling the core blackboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opr~create-core-blackboard (commands)
  (make-instance 'opr+core-blackboard :command-queue commands))

(defun opr~make-core-blackboard ()
  (setf opr*core-blackboard (opr~create-core-blackboard nil)))

(defun opr~reset-core-blackboard ()
  (declare (edited  "13-DEC-1999")
	   (authors Sorge)
	   (input   "None.")
	   (effect  "Resets the core blackboard, i.e. removes all its command entries.")
	   (value   "Undefined."))
  (proc~with-process-lock (opr*lock)
			  (setf (opr~bb-command-queue opr*core-blackboard) nil)))

(defun opr~enqueue-command (command &optional (bb opr*core-blackboard))
  (declare (edited  "08-DEC-1999")
	   (authors Sorge)
	   (input   "A list representing a normalized command (see opr~normalize-command) and the core blackboard.")
	   (effect  "Enqueues the command in the blackboard priority queue of commands.")
	   (value   "Undefined."))
  (when bb
    (proc~with-process-lock (opr*lock)
			    (setf (opr~bb-command-queue bb)
				  (append (opr~bb-command-queue bb) (list command)))
			    )))

(defgeneric opr~normalize-command (command &key (args nil) (status nil) (process nil))
  (declare (edited  "08-DEC-1999")
	   (authors Sorge)
	   (input   "A command, a list of arguments and a symbol and a process name.")
	   (effect  "None.")
	   (value   "The normalized representation for the core blackboard:"
		    "(command args calling-process status)."
		    "If status is NIL the command can be executed, if T it still waits for"
		    "some arguments to be completed."))
  (:method ((command string) &key (args nil) (status nil) (process nil))
	   (opr~normalize-command (bb~find-command command) :args args :status status :process process))
  (:method ((command symbol) &key (args nil) (status nil) (process nil))
	   (opr~normalize-command (bb~find-command command) :args args :status status :process process))
  (:method ((command com+command) &key (args nil) (status nil) (process nil))
	   (list command args process status)))

(defun opr=command-executable-p (command)
  (declare (edited  "08-DEC-1999")
	   (authors Sorge)
	   (input   "A core bb command entry.")
	   (effect  "None.")
	   (value   "T if the command can be executed right away."))
  (not (fourth command)))

(defun opr~pop-command (&optional (bb opr*core-blackboard))
  (declare (edited  "08-DEC-1999")
	   (authors Sorge)
	   (input   "Returns the first executable command on the blackboard, if there is one.")
	   (effect  "Can remove a command entry from BB.")
	   (value   "An executable command or NIL."))
  (when bb
    (proc~with-process-lock (opr*lock)
			    (let* ((queue (opr~bb-command-queue bb))
				   (command (find-if #'opr=command-executable-p queue)))
			      (setf (opr~bb-command-queue bb) (remove command queue))
			      command))))
  

(defun opr~execute-command (command)
  (let* ((com (first command))
	 (args (second command))
	 (caller (third command))
	 (opr*calling-listener caller))
    (opr~arrest-service com)
    (unwind-protect
	(cond ((and (symbolp caller)(string<
				     (string-downcase "httpclient")
				     (string-downcase (string caller)))) ;;this is ugly, todo MP
	       (sys~handler-case
		(let ((result (list (apply (com~function com) args))))
		  (http~send-response caller                       
				      (rpc~compose-methodresponse result) 
				      :type "text/xml")                   
		  (socket~close caller)                            
		  (socket~delete caller))
		(error (c)
		       (progn (http~send-response caller
						  (rpc~compose-methodresponse
						   (list
						    ;; the XML-RPC faultCode
						    (if (simple-condition-format-arguments c)
							(first
							 (simple-condition-format-arguments c))
						      101)
						    ;; the XML-RPC faultString
						    (format nil
							    (simple-condition-format-control c)
							    (simple-condition-format-arguments c)
							    ))
						   :fault T)
						  :type "text/xml")
			      (socket~close caller)
			      (socket~delete caller)
			      (omega~error "~A" c)))))
	      ((and (opr~lisp-listener-p caller)
		    (or (find (keim~name com) opr*halt-lisp-listener :test #'string-equal)
			(comint~debugger-on-break comint*current-comint)))
	       (unwind-protect
		   (apply (com~function com) args)
		 (setf opr*active nil)
		 (opr~release-listener))
	       (setf opr*active t))
	      ((opr~lisp-listener-p caller)
	       (opr~release-listener)
	       (apply (com~function com) args))
	      ((and (opr~loui-listener-p caller)
		    (comint~debugger-on-break comint*current-comint))
	       (opr~arrest-listener)
	       (opr=with-output-to-loui
		(unwind-protect
		    (apply (com~function com) args)
		  (setf opr*active nil)
		  (opr~release-listener)
		  (opr~release-listener opr*loui-listener))))
	      ((opr~loui-listener-p caller)
	       (opr=with-output-to-loui
		(unwind-protect
		    (apply (com~function com) args)
		  (opr~release-listener opr*loui-listener))))
	      (t (apply (com~function com) args)))
      (opr~release-listener opr*service-listener))))

;;   (simple-error (c) (omega~error "Command could not be applied:~%        ~A" c))
;;   (error (c) (omega~error "Command could not be applied:~%    ~A " c))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Querying the Mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opr~active-p () opr*active)

(defun opr~runtime-object () opr*runtime-object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Handling the core process for command execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun opr~reset ()
  (setf opr*reset (not opr*reset)))

(defun opr~quit ()
  (setf opr*active nil)
  (setf opr*not-killed nil)
  (setf opr*quit t))

(defun opr~start ()
  (setf opr*quit nil)
  (setf opr*not-killed t)
  (setf opr*active t))

(defun opr=core-process-run-function ()
  (let ((reset opr*reset)
	command)
    (in-package :omega)
    (loop
     (with-simple-restart
      (abort "Return to OMEGA top level")
      (if (comint~debugger-on-break comint*current-comint)
	  (sys~handler-case
	   (progn
	     (proc~wait "OMEGA core process waits for applicable commands."
			#'(lambda () (or (not (equal reset opr*reset))
					 opr*quit
				       (let ((test (opr~pop-command)))    ;; A hack for ACL 8.0   VS
					 (if test (setf command test))
					 test))))
	     (cond (opr*quit
		    (return (omega~message "OMEGA core process is quitting job!")))
		   ((not (equal reset opr*reset))
		    (omega~warn "OMEGA core process resets.")
		    (opr~reset-core-blackboard)
		    (setf reset opr*reset))
		   (t (when opr*loui-listener
			(socket~write (format nil "indicateBusy(\"~:(~A~)\")" (keim~name (car command))) :inout))
		      (unwind-protect
			  (opr~execute-command command)
			(when opr*loui-listener (socket~write "indicateIdle" :inout))))))
	   (opr+abort (c) (progn
			    (opr~release-listener)
			    (setf opr*runtime-object (opr+abort-return-object c))
			    (let ((opr*calling-listener (opr+abort-caller c)))
			      (omega~error "Execution aborted, the runtime-object ~A was saved." (opr~runtime-object))))))
	(sys~handler-case
	 (progn
	   (proc~wait "OMEGA core process waits for applicable commands."
		      #'(lambda () (or (not (equal reset opr*reset))
				       opr*quit
				       (let ((test (opr~pop-command)))    ;; A hack for ACL 8.0   VS
					 (if test (setf command test))
					 test))))
	   (cond (opr*quit
		  (return (omega~message "OMEGA core process is quitting job!")))
		 ((not (equal reset opr*reset))
		  (omega~warn "OMEGA core process resets.")
		  (opr~reset-core-blackboard)
		  (setf reset opr*reset))
		 (t (when opr*loui-listener
		      (socket~write  (format nil "indicateBusy(\"~:(~A~)\")" (keim~name (car command))) :inout))
		      (unwind-protect
			  (opr~execute-command command)
			(when opr*loui-listener (socket~write "indicateIdle" :inout))))))
	 (opr+abort (c) (progn
			  (opr~release-listener)
			  (setf opr*runtime-object (opr+abort-return-object c))
			  (let ((opr*calling-listener (opr+abort-caller c)))
			    (omega~error "Execution aborted, the runtime-object ~A was saved." (opr~runtime-object)))))
	 (simple-error (c) (omega~error "~A" c)) 
	 (error (c) (omega~error "~A" c))   

	 ))))))
;;;))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Initialization of the mechanism 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun opr~initialize-listeners ()
  (when (opr~active-p)
    (mapcar #'(lambda (socket)
		(when (socket~active? socket)
		  (omega~message "initializing listener for ~A" socket)
		  (opr=initialize-listener socket)))
	    opr*listener-names)))

(defgeneric opr=initialize-listener (socket)
  (declare (edited  "20-DEC-1999")
	   (authors Sorge)
	   (input   "A symbol specifying a socket.")
	   (effect  "Initializes a listener for that socket.")
	   (value   "The process running the socket-listener."))
  (:method (socket)
	   (declare (ignore socket)))
  (:method ((socket symbol))
	   (opr=listener-create socket))
  (:method ((socket (eql :inout)))
	   (if (and opr*loui-listener (proc~is-active opr*loui-listener))
	      opr*loui-listener
	    (let ((new-proc (opr=listener-create socket)))
	      (setf keim::mixin*active t)
	      (setf view*on t)
	      (setf opr*loui-listener new-proc)
	      new-proc)))
  (:method ((socket (eql :http)))
	   (if (and opr*http-listener (proc~is-active opr*http-listener))
	      opr*http-listener
	    (let ((new-proc (opr=listener-create socket 'opr=http-listener-function)))
	      ;(setf keim::mixin*active t)
	      ;(setf view*on t)
	      (setf opr*http-listener new-proc)
	      new-proc)))
  (:method ((socket (eql :service)))
	   (if (and opr*service-listener (proc~is-active opr*service-listener))
	      opr*service-listener
	     (let ((new-proc (opr=listener-create socket)))
	       (setf opr*service-listener new-proc)
	       new-proc))))
	   

(defun opr~initialize-omega ()
  (cond ((and opr*omega-core-process (proc~is-active opr*omega-core-process))
	 (opr~start))
	((and opr*omega-core-process (not (proc~is-active opr*omega-core-process)))
	 (opr~start)
	 (setf opr*omega-core-process (proc~create :name "OMEGA-CORE-PROCESS"
						   :priority 100
						   :function #'opr=core-process-run-function)))
	(t
	 (opr~make-core-blackboard)
	 (setf opr*lisp-listener (proc~actual-process))
	 (proc~new-priority opr*lisp-listener 200)
	 (opr~start)
	 (setf opr*omega-core-process (proc~create :name "OMEGA-CORE-PROCESS"
						   :priority 100
						   :function #'opr=core-process-run-function)))))

(defun opr~reset-omega-processes ()
  (omega~message "Reseting OMEGA's processes!!!")
  (opr=reset-socket-listeners)
  (opr~reset-core-blackboard)
  (opr~initialize-omega))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redefining some Mixin Functions. Should be done more cleanly one day
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mixin~save-and-set-input () (opr~arrest-listener))

(defun mixin~reset-input () (opr~release-listener))

;; Some modifications to Allegro CL exit functionality

#+ALLEGRO
(eval-when (load compile eval)
  (setq sys:*exit-cleanup-forms* (list '(format t "Exiting OMEGA!!!~%")))
  (tpl::add-new-command "exit" 1 #'(lambda () (excl:exit 0 :no-unwind t)) "Exiting OMEGA's multiprocessing.")
  )


