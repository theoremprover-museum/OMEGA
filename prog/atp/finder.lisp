;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
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

(in-package :omega)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MathWeb Interface to FINDER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun finder~enter   ()       (serv~enter   "FINDER"))
(defun finder~leave   ()       (serv~leave   "FINDER"))
(defun finder~restart ()       (serv~restart "FINDER"))
(defun finder~apply   (method) (serv~apply   "FINDER" method :timeout 600))


(defun finder~call-finder (input)
  (finder~enter)
  (rest (assoc 'output (read-from-string
			(finder~apply (format nil "prove(\"~A\" replyWith: foo(state: unit output: unit))" input))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Interface to FINDER 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Read file and store it in a string
;;

(defun finder~read-file-general (file-name)
  (declare (edited  "26-OCT-1999")
           (authors CHOI)
           (input   "a file name")
           (effect  "none")
           (value   "a string with the content of the file"))
  (let ((result ""))
    (with-open-file (input-stream file-name :direction :input)
      (do ((input-char (read-char input-stream nil)
                       (read-char input-stream nil)))
          ((not input-char))
        (setq result (concatenate 'string result (string input-char)))))
    result))

(defun finder~read-file-single-string (file-name)
  (declare (edited  "26-OCT-1999")
           (authors CHOI KERBER)
           (input   "a file name for a file that consists of a single string")
           (effect  "none")
           (value   "the string which forms the content of the file"))
  (let ((result ""))
    (setq *read-suppress* nil)
    (with-open-file (input-stream file-name :direction :input)
      (setq result (read input-stream nil)))
    result))

(defun finder~read-file (file-name)
  (declare (edited  "27-OCT-1999")
           (authors CHOI)
           (input   "a file name")
           (effect  "none")
           (value   "a string with the content of the file"))
  (let ((first-char))
    (with-open-file (input-stream file-name :direction :input)
      (setf first-char (read-char input-stream nil)))
    (if (eql first-char #\")
      (finder~read-file-single-string file-name)
      (finder~read-file-general file-name))))

;;
;;  Write an output file
;;

(defun finder~write-file (filename output)
  (declare (edited  "03-JUL-2001")
	   (authors Sorge)
	   (input   "A filename and a string.")
	   (effect  "Writes the string to the given file.")
	   (value   "Undefined."))
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
		  (format out output)
		  (terpri out)))

;;
;;  Read models from a file, translate them and write them to a different file
;;
		  
(defun finder~read-models (input-file-name output-file-name model-file-name)
  (declare (edited  "04-JUL-2001" "22-OCT-1999")
           (authors Sorge CHOI)
           (input   "a file name of Finder input, a file name of Finder output")
           (effect  "none")
           (value   "a list of models"))
  (let* ((input (finder~read-file input-file-name))
	 (output (finder~read-file output-file-name))
	 (models (finder~translate-models input output)))
    (with-open-file (output-stream model-file-name :direction :output
				   :if-does-not-exist :create
				   :if-exists :supersede)
		    (format output-stream models))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating a standard FINDER input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun finder~finder-input (&key sorts constants functions clauses (solutions 0))
  (declare (edited  "02-JUL-2001")
	   (authors Sorge)
	   (input   "Four lists of strings and an integer.")
	   (effect  "None.")
	   (value   "A string containing the input specification for FINDER."))
  (concatenate
   'string
   (format nil "setting solutions ~A~%" solutions)
   (format nil "setting time-stamp~%")
   (format nil "~%sort { ~{~%      ~A~}~%          }~%" sorts)
   (format nil "~%constant { ~{~%      ~A~}~%          }~%" constants)
   (format nil "~%function { ~{~%      ~A~}~%          }~%" functions)
   (format nil "~%clause { ~{~%      ~A~}~%          }~%" clauses)
   "end"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translating FINDER output into lists of models
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun finder~translate-models (input output)
  (declare (edited  "04-JUL-2001")
           (authors Sorge)
           (input   "Two strings: the input to Finder and its computed output.")
           (effect  "none")
           (value   "A string containing a list of models."))
  (let ((domain-list (finder=get-domain input))
	(model-string-list (finder=read-model-list output)))
    (with-output-to-string (output-stream)
			   (dolist (model-string model-string-list)
			     (let ((model-head-body (finder=create-model-element-data model-string)))
			       (format output-stream "~%(~a (" (first model-head-body))
			       (format output-stream "~%  (domain  ~a)" domain-list)
			       (format output-stream "~%  ~a" (finder=get-function (second model-head-body)))
			       (format output-stream "~%))~%"))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions for translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Check a character if it is a space
;;

(defun finder=is-space (char)
  (if (or (eql char #\space) (eql char #\tab)) t nil))

(defun finder=is-newline (char)
  (if (eql char #\newline) t nil))

(defun finder=is-space-or-newline (char)
  (if (or (finder=is-space char) (finder=is-newline char)) t nil))

(defun finder=is-separator (char)
  (if (or
        (eql char #\:)
        (eql char #\,)
        (eql char #\.)
        (eql char #\()
        (eql char #\))
        (eql char #\{)
        (eql char #\})
        (eql char #\&)
        (eql char #\|)
        (eql char #\=)
        (eql char #\+)
        (eql char #\-)
        (eql char #\*)
        (eql char #\/)) t nil))

(defun finder=is-token-end (char)
  (if (or (finder=is-space-or-newline char) (finder=is-separator char)) t nil))

;;
;; Get a token from a string
;;

(defun finder=get-a-token (string)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   "the first word of the string"))
  (let ((token "") (index 0)
	(first-flag t))
    (when (not (equal string ""))
      (do ((next-char (schar string index)
                      (schar string index)))
          ((not (finder=is-space-or-newline next-char)))
        (setq index (+ index 1))
        (when (not (< index (length string)))
          (return nil)))
      (if (< index (length string))
        (do ((next-char (schar string index)
                        (schar string index)))
            ((and (not first-flag)
                  (or (finder=is-separator (schar token 0))
                      (finder=is-token-end next-char))))
          (setq token (concatenate 'string token (string next-char)))
          (setq index (+ index 1))
          (setq first-flag nil)
          (when (not (< index (length string)))
            (return nil)))
        (setq token "")))
    token))

(defun finder=get-a-token-skip-comment (input-string)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   "the first word of the string"))
  (do ((token (finder=get-a-token input-string)
              (finder=get-a-token input-string)))
      ((not (eql (schar token 0) #\%)) token)
    (setf input-string (finder=remove-a-token input-string))
    (let ((index (search (string #\newline) input-string)))
      (when (eql index nil)
	(setf token "") (return token))
      (setf input-string (subseq input-string index)))))

;;
;; Remove a token from a string
;;

(defun finder=remove-a-token (string)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   "the string with the first word removed"))
  (let ((index 0)
	(first t))
    (do ((next-char (schar string index)
                    (schar string index)))
        ((not (finder=is-space-or-newline next-char)))
      (setq index (+ index 1))
      (when (not (< index (length string)))
        (return nil)))
    (when (< index (length string))
      (do ((next-char (schar string index)
                      (schar string index)))
          ((and (not first)
                (or (finder=is-separator (schar string (- index 1)))
                    (finder=is-token-end next-char))))
        (setq index (+ index 1))
        (setq first nil)
        (when (not (< index (length string)))
          (return nil))))
    (subseq string index)))

(defun finder=remove-a-token-skip-comment (input-string)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   "the string with the first word removed"))
  (let (index)
    (do ((token (finder=get-a-token input-string)
		(finder=get-a-token input-string)))
	((not (eql (schar token 0) #\%)) (finder=remove-a-token input-string))
      (setf input-string (finder=remove-a-token input-string))
      (setf index (search (string #\newline) input-string))
      (when (eql index nil) (return ""))
      (setf input-string (subseq input-string index)))))

;;
;; Get a domain list from an input file for Finder
;;

(defun finder=get-domain-enum (input-string)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   ""))
  (let ((domain-value-list nil)
	(token (finder=get-a-token-skip-comment input-string)))
    (cond
      ((equal token "enum")
        (setf input-string (finder=remove-a-token-skip-comment input-string))
        (setf token (finder=get-a-token-skip-comment input-string))
        (unless (equal token ":") (return-from finder=get-domain-enum nil))
        (setf input-string (finder=remove-a-token-skip-comment input-string))
        (do ((token (finder=get-a-token-skip-comment input-string)
                    (finder=get-a-token-skip-comment input-string)))
            ((equal token "."))
          (setf domain-value-list (append domain-value-list (list token)))
          (setf input-string (finder=remove-a-token-skip-comment input-string))
          (if (equal (finder=get-a-token-skip-comment input-string) ",")
            (setf input-string
              (finder=remove-a-token-skip-comment input-string)))))
      ((equal token "cardinality")
        (setf input-string (finder=remove-a-token-skip-comment input-string))
        (setf token (finder=get-a-token-skip-comment input-string))
        (unless (equal token "=") (return-from finder=get-domain-enum  nil))
        (setf input-string (finder=remove-a-token-skip-comment input-string))
        (setf token (finder=get-a-token-skip-comment input-string))
        (dotimes (index (parse-integer token) nil)
          (setf domain-value-list (append domain-value-list (list index)))))
      (t nil))
    domain-value-list))

(defun finder=get-multiple-domains (input-string)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   ""))
  (let (domain-list
	domain-values
	domain
	(token (finder=get-a-token-skip-comment input-string)))
    (unless (equal token "{") (return-from finder=get-multiple-domains nil))
    (setf input-string (finder=remove-a-token input-string))
    (do ((token (finder=get-a-token-skip-comment input-string)
                (finder=get-a-token-skip-comment input-string)))
        ((or (equal token "}") (equal token "")))
      (setf domain-values
        (finder=get-domain-enum
          (finder=remove-a-token-skip-comment input-string)))
      (setf domain (list token domain-values))
      (setf domain-list (append domain-list (list domain)))
      (do ((token (finder=get-a-token-skip-comment input-string)
                  (finder=get-a-token-skip-comment input-string)))
          ((eql (schar token 0) #\.))
        (setf input-string (finder=remove-a-token-skip-comment input-string)))
      (setf input-string (finder=remove-a-token input-string)))
    domain-list))

(defun finder=get-domain (input-string)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   ""))
  (let ((domain-list nil) (token ""))
    (do ((token (finder=get-a-token-skip-comment input-string)
                (finder=get-a-token-skip-comment input-string)))
        ((equal (string-downcase token) "sort"))
      (setf input-string (finder=remove-a-token-skip-comment input-string)))
    (setf input-string (finder=remove-a-token-skip-comment input-string))
    (setf token (finder=get-a-token-skip-comment input-string))
    (if (equal token "{")
	(setf domain-list (finder=get-multiple-domains input-string))
      (setf domain-list (list (list token
        (finder=get-domain-enum
          (finder=remove-a-token-skip-comment input-string))))))
    domain-list))

;;
;; Search a 'Model ' from a string
;;

(defun finder=get-model-pos (input-string current-pos)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "")
           (effect  "none")
           (value   ""))
  (search "Model " input-string :start2 current-pos))

(defun finder=get-model (input-string start-offset)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "")
           (effect  "none")
           (value   ""))
  (let ((first-offset (finder=get-model-pos input-string start-offset)))
    (unless first-offset (return-from finder=get-model nil))
    (let ((next-offset (finder=get-model-pos input-string (+ first-offset 1))))
      (when (not next-offset)
	(setf next-offset (search "Solutions found: " input-string
				  :start2 first-offset)))
      (subseq input-string first-offset next-offset))))

(defun finder=read-model-list (input-string)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   ""))
  (let ((start-offset 0)
	model-list model-string)
    (do ((model-pos (finder=get-model-pos input-string 0)
                    (finder=get-model-pos input-string start-offset)))
        ((not model-pos))
      (setf model-string (finder=get-model input-string start-offset))
      (setf model-list (append model-list (list model-string)))
      (setf start-offset (+ start-offset (length model-string))))
    model-list))

(defun finder=get-model-element-name (model-element)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   ""))
  (let ((start-pos (search "Model " model-element))
	(end-pos (search (string #\newline) model-element)))
    (concatenate 'string "Model_"
		 (subseq model-element (+ start-pos 6) end-pos))))

(defun finder=get-model-element-body (model-element)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   ""))
  (let ((start-pos (search (string #\newline) model-element :start2 (search "Model " model-element))))
    (subseq model-element (+ start-pos 1))))

(defun finder=create-model-element-data (model-string)
  (declare (edited  "28-OCT-1999")
           (authors CHOI)
           (input   "a string")
           (effect  "none")
           (value   "a list that contains a name and a body"))
    (list (finder=get-model-element-name model-string)
	  (finder=get-model-element-body model-string)))

;;
;; Read a function
;;

; (defun finder=get-simple-function (data-string)
;  (declare (edited  "22-OCT-1999")
;           (authors CHOI)
;           (input   "a string")
;           (effect  "none")
;           (value   ""))
;  (let ((value nil))
;    (setf data-string (finder=remove-a-token data-string))
;    (setf token (finder=get-a-token data-string))
;    (setf value (list token))
;    value))

(defun finder=is-end-of-function (data-string)
  (declare (edited  "15-NOV-1999")
           (authors CHOI)
           (input   "")
           (effect  "none")
           (value   ""))
  (let ((i 0))
    (do ((ch (schar data-string i) (schar data-string i)))
        ((not (finder=is-space ch)))
      (setq i (+ i 1)))
    (if (or (and (eql (schar data-string i) #\newline)
                 (eql (schar data-string (+ i 1)) #\newline))
            (not (> (length data-string) i))) t nil)))

(defun finder=get-two-par-function (parameter params list data-string)
  (declare (edited  "15-NOV-1999")
           (authors CHOI)
           (input   "a string with the content of a model")
           (effect  "none")
           (value   ""))
  (let (results param-value token)
    (dolist (value list)
      (setf results (append results (list (list
					   (list (first (first value)) parameter)
					   (second value))))))
    (do ((eof (finder=is-end-of-function data-string)
              (finder=is-end-of-function data-string)))
        ((eql eof t))
      (setf param-value (finder=get-a-token data-string))
      (setf data-string (finder=remove-a-token data-string))
      ;; Read '|'
      (when (finder=get-a-token data-string)
        (setf data-string (finder=remove-a-token data-string)))
      ;; Read the results
      (dotimes (index (length params) results)
        (setf token (finder=get-a-token data-string))
        (setf results (append results
			      (list (list (list (first (nthcdr index params)) param-value)
					  token))))
        (setf data-string (finder=remove-a-token data-string))))
    results))

(defun finder=get-function (data-string)
  (declare (edited  "22-OCT-1999")
           (authors CHOI)
           (input   "a string with the content of a model")
           (effect  "none")
           (value   ""))
  (let ((function-list nil) function-name)
    (do ((token (finder=get-a-token data-string)
                (finder=get-a-token data-string)))
        ((equal token ""))
      (let ((params nil) (function nil)
	    ;; Read the domain name
	    (domain-name (finder=get-a-token data-string)))
	(setf data-string (finder=remove-a-token data-string))
        ;; 
        (setf token (finder=get-a-token data-string))
        (cond
	 ((equal token "=")
	  ;; Skip '='
	  (setf data-string (finder=remove-a-token data-string))
	  ;; Read a simple function
	  (setf token (finder=get-a-token data-string))
	  (setf function (append (list domain-name) (list (list token))))
	  (setf data-string (finder=remove-a-token data-string))
	  )
	 ((equal token "|")
	  ;; Skip '|'
	  (setf data-string (finder=remove-a-token data-string))
	  ;; Read parameters
	  (do ((token (finder=get-a-token data-string)
		      (finder=get-a-token data-string)))
	      ((finder=is-separator (schar token 0)))
	    (setf params (append params (list token)))
	    (setf data-string (finder=remove-a-token data-string)))
	  ;; Read '----+--------'
	  (do ((token (finder=get-a-token data-string)
		      (finder=get-a-token data-string)))
	      ((not (finder=is-separator (schar token 0))))
	    (setf data-string (finder=remove-a-token data-string)))
	  ;; Read the function name
	  (setf function-name (finder=get-a-token data-string))
	  (setf data-string (finder=remove-a-token data-string))
	  ;; Read '|'
	  (when (finder=get-a-token data-string)
	    (setf data-string (finder=remove-a-token data-string)))
	  ;; Read the results
	  (let ((results nil))
	    (dotimes (index (length params) results)
	      (setf token (finder=get-a-token data-string))
	      (setf results (append results
				    (list (list (list (first (nthcdr index params))) token))))
	      (setf data-string (finder=remove-a-token data-string)))
	    ;; Check if there is more line
	    (when (not (finder=is-end-of-function data-string))
	      ;; If there is more line
	      (setq results (finder=get-two-par-function
			     function-name params results data-string))
	      (setq function-name domain-name)
	      (do ((eof (finder=is-end-of-function data-string)
			(finder=is-end-of-function data-string)))
		  ((eql eof t))
		(setq data-string (finder=remove-a-token data-string))))
	    (setf function (append (list function-name) (list results))))))
        (setf function-list (append function-list (list function)))))
    function-list))

