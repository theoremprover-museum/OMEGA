(load "/home/chris/omega/ags/sys/boot.lisp") (format t "omegahome: ~S" *omegahome*)  (in-package :user) (load-sys 'omega-3) (load-sys 'gui-3) (load-sys 'multi) (in-package :omega)
(sys~dump-system 'omega-3 "/tmp/tmp-omega-3-emacs-linux-chris"  #'(lambda () (in-package :omega)))
