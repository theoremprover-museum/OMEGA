(in-package :omega)

(setq pres*theoryinfo	
      '(base (multipliers (and)
			  equalityoperators (= equiv)
			  negations (not))
	     rational (multipliers (divide times-frac)
				   equalityoperators nil
				   negations nil)
	     integer (multipliers (div times-int)
				  equalityoperators nil
				  negations nil)
	     natural (multipliers (times)
				  equalityoperators (leq less greater geq leq-nat)
				  negations nil)
	     lueneburg (multipliers (*)
				    equalityoperators (<)
				    negations nil)
	     metric (multipliers nil
				 equalityoperators (triangle-eq)
				 negations nil)
	     field (multipliers (field-div)
				equalityoperators nil
				negations nil)))
