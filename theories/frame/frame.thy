;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
(in-package :omega)


(th~deftheory frame
	      (uses generic))

(compile-file (make-pathname :directory *omegahome* :name "omega-3/theories/frame/frame" :type "lisp"))
(load (make-pathname :directory *omegahome* :name "omega-3/theories/frame/frame" :type "fasl"))

(compile-file (make-pathname :directory *omegahome* :name "omega-3/theories/frame/tu" :type "lisp"))
(load (make-pathname :directory *omegahome* :name "omega-3/theories/frame/tu" :type "fasl"))

(compile-file (make-pathname :directory *omegahome* :name "omega-3/theories/frame/modifications" :type "lisp"))
(load (make-pathname :directory *omegahome* :name "omega-3/theories/frame/modifications" :type "fasl"))

(th~deftheorem test
	       (in frame) 
	       (category THEOREM)
	       (constants (form o))
	       (conclusion (all-types bb (forall (lam (Q (o bb))
						      (forall (lam (x bb)
								   (implies (and (q x) form)
									    (and form (q x)))))))))
	       (help ""))
