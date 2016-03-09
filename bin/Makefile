#!/bin/tcsh

HERE 		= $(shell pwd)
OMEGA_HOME	= $(HERE)/../..

BOOT_LISP 	  = $(OMEGA_HOME)/ags/sys/boot.lisp
ALLEGRO_LOAD_FILE = $(HERE)/omega-load.lisp
OMEGA_LOAD_USER   = $(HOME)/.mathweb/omega-load.lisp

COPY            = \cp -f 
MOVE		= \mv -f
REMOVE		= \rm -f

-include $(OMEGA_HOME)/omega-3/config/omega-config.$(OSTYPE)
-include             $(HOME)/.mathweb/omega-config.$(OSTYPE)


all: dumps

dumps: 
	@echo "********************************************"
	@echo "*** Environment: $(OSTYPE)"       
	@echo "***"
	@echo "*** Using ACL at $(ACLHOME)"
	@echo "***"
	@echo "*** Loading omega system at $(ALLEGRO)"
	@echo "***"
	@echo "*** Writing system dumps to" 
	@echo "***     $(OMEGA_DUMP_EMACS) and"
	@echo "***     $(OMEGA_DUMP_QUIET)."
	@echo "***" 
	@echo "********************************************"
	@echo ""

## first write the load-sys commands in the temporary lisp file
	@echo "(load \"$(BOOT_LISP)\")\
	        (in-package :user)\
	        (load-sys 'omega-3)\
	        (load-sys 'gui-3)\
	        (load-sys 'multi)\
	        (in-package :omega)"  > $(ALLEGRO_LOAD_FILE)   

## then add the users file which may contain additional statements
	@if (ls $(OMEGA_LOAD_USER) >/dev/null);  then \
	    cat $(OMEGA_LOAD_USER) >> $(ALLEGRO_LOAD_FILE); fi

## finally add the dump writing commands 
	@echo "(sys~dump-system 'omega-3 \"${TEMP_OMEGA_DUMP_EMACS}\"\
					 #'(lambda () (in-package :omega)))\
	       (setf excl::*restart-app-function* (lambda () (omega~loui)))\
	       (sys~dump-system 'omega-3 \"${TEMP_OMEGA_DUMP_QUIET}\"\
					 #'(lambda () (in-package :omega)))"\
	       >> $(ALLEGRO_LOAD_FILE)

## call ACL with the created temporary lisp file...
	@/bin/sh -ec 'echo "(load \"$(ALLEGRO_LOAD_FILE)\") \
			    (ags::sys~exit-from-lisp)"' | $(ALLEGRO)

## make a copy of the old omega dumps 
	@if (ls $(OMEGA_DUMP_EMACS) >/dev/null); then \
	    $(MOVE) $(OMEGA_DUMP_EMACS) $(OMEGA_DUMP_EMACS).old; fi
	@if (ls $(OMEGA_DUMP_QUIET) >/dev/null); then \
	    $(MOVE) $(OMEGA_DUMP_QUIET) $(OMEGA_DUMP_QUIET).old; fi

## then move the temporary dumps to the final position
	@if (ls ${TEMP_OMEGA_DUMP_EMACS} >/dev/null); then \
	    $(COPY) ${TEMP_OMEGA_DUMP_EMACS} $(OMEGA_DUMP_EMACS);\
	 else echo "There went something wrong..."; fi

	@if (ls ${TEMP_OMEGA_DUMP_QUIET} >/dev/null); then \
	    $(COPY) ${TEMP_OMEGA_DUMP_QUIET} $(OMEGA_DUMP_QUIET);\
	 else echo "There went something wrong..."; fi

## finally, remove the temporary dumps and '.additives' files
	$(REMOVE) ${TEMP_OMEGA_DUMP_EMACS}
	$(REMOVE) ${TEMP_OMEGA_DUMP_QUIET}
	$(REMOVE) ${TEMP_OMEGA_DUMP_EMACS}.additives
	$(REMOVE) ${TEMP_OMEGA_DUMP_QUIET}.additives

fasl:
## first write the compile-sys commands in the temporary lisp file
	@echo "(load \"$(BOOT_LISP)\")\
	        (in-package :user)\
	        (compile-sys 'omega-3)\
	        (compile-sys 'gui-3)\
	        (compile-sys 'multi)"> $(ALLEGRO_LOAD_FILE)   

## then add the users file which may contain additional statements
	@if (ls $(OMEGA_LOAD_USER) >/dev/null);  then \
	    cat $(OMEGA_LOAD_USER) >> $(ALLEGRO_LOAD_FILE); fi

## call ACL with the created temporary lisp file...
	@/bin/sh -ec 'echo "(load \"$(ALLEGRO_LOAD_FILE)\") \
			    (ags::sys~exit-from-lisp)"' | $(ALLEGRO)

clean:
	\rm -f $(HERE)/*.dxl














