;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff to generate testbeds etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)


(com~defcommand generate-set-examples
  (argnames num-of-vars depth list-of-ops example-file result-file)
  (argtypes number number  symbol-list pathname pathname)
  (arghelps "The number of variables." "The maximum construction depth."
	    "The set operators to be employed."
	    "Filename for the example file." "Filename for the result-file")
  (frag-cats examples)
  (function agplan~analyse-automatic-generated-set-examples)
  (defaults (4 3 '(union intersection setminus exclunion) "/tmp/set-ex" "/tmp/set-ex-out" ))
  (Log-p T) 
  (help ""))    
	

(com~defcommand pds2outline
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats examples)
  (function agplan~pds2outline)
  (defaults )
  (Log-p T) 
  (help "Shows the OUTLINE (in the sense of Mateja Learning project) of the current pds."))


(com~defcommand explore-set-examples
                (argnames file outfile)
                (argtypes pathname pathname)
                (arghelps "The pathname of the set examples" "The output pathname")
                (frag-cats examples)
                (function sex~explore-testbed)
		(defaults ("/tmp/set-ex" "/tmp/set-ex-out"))
                (log-p t)
                (help "Automatically explore a file with set examples."))     

(com~defcommand explore-set-examples-new
                (argnames file outfile)
                (argtypes pathname pathname)
                (arghelps "The pathname of the set examples" "The output pathname")
                (frag-cats examples)
                (function sex~explore-testbed-new)
		(defaults ("/tmp/set-ex" "/tmp/set-ex-out"))
                (log-p t)
                (help "Automatically explore a file with set examples."))     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some special commands for the group theory examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand group-examples-settings
		(frag-cats examples)
		(function grp~settings-group-examples)
		(log-p t)
		(help "Use settings for group examples."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grosses Commando zum Explorieren                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand explore
		(argnames set operation) 
		(argtypes anything anything) 
                (arghelps "The set" "The operation")
		(function expl~explore-znz-command)
		(defaults ('(resclass-set 2) '(lam (x (o num))(y (o num))(plus-resclass (times-resclass x y) (resclass 2 1)))))
		(frag-cats examples)
		(log-p T)
		(help ""))


(com~defcommand show-expl-table
		(argnames name)
		(argtypes string)
		(arghelps "The name of the exploration table to show")
		(function expl=show-table)
		(defaults expl=show-expl-table-default)
		(frag-cats examples)
		(log-p T)
		(help ""))

(com~defcommand explore-testbed
		(argnames set which from to out-file) 
		(argtypes number number number number pathname) 
                (arghelps "The general set" "The exact set" "Starting with operation" "Ending with operation" "out-file")
		(function expl~set-with-operations)
		(frag-cats examples)
		(log-p T)
		(help ""))

