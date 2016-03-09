(load (sys:getenv "BOOTLISP"))

;(compile-sys 'omega-3)
;(compile-sys 'multi)
;(compile-sys 'gui-3)

(load-sys 'omega-3)
(load-sys 'multi)
(load-sys 'gui-3)

(in-package "OMEGA")

(format t "~% Writing dumps temporarily to /tmp/")
(sys~dump-system 'omega-3
		 "/tmp/temp-omega-emacs"
		 #'(lambda () (in-package :omega)))

; BTW: we don't need this dump
;(setf excl::*restart-app-function* (lambda () (omega~loui)))
;(sys~dump-system 'omega-3
;                 "/tmp/temp-omega-quiet"
;                 #'(lambda () (in-package :omega)))
