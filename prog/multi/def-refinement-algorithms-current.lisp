(in-package :omega)

;;;;;;;;;;;;;;
;; PPLANNER ;;
;;;;;;;;;;;;;;

;; The code itself is in pplanner.lisp

(refalg~define-refinement-algorithm
 (name pplanner)
 (parameter-list (methods
		  normalization-methods
		  restriction-methods
		  control-rules
		  termination-check
		  loop-detection
		  randomization-rules
		  selection))
 (invokation-function pplan~pplanner-invokation-function)
 (reinvokation-function pplan~pplanner-reinvokation-function)
 )


;;;;;;;;;;;;;;
;; InstMeta ;;
;;;;;;;;;;;;;;

;; The code itself is in instmeta.lisp

(refalg~define-refinement-algorithm
 (name instmeta)
 (parameter-list (compute-instantiation-function))
 (invokation-function instmeta~instmeta-invokation-function)
 (reinvokation-function instmeta~instmeta-invokation-function)
 )

;;;;;;;;;;;;;;
;; BackTrack ;;
;;;;;;;;;;;;;;

;; The code itself is in backtrack.lisp

(refalg~define-refinement-algorithm
 (name backtrack)
 (parameter-list (compute-backtrack-steps-function))
 (invokation-function back~backtrack-invokation-function)
 (reinvokation-function back~backtrack-reinvokation-function)
 )

;;;;;;;;;;;;;;
;; ANALOGY  ;;
;;;;;;;;;;;;;;

;; The code itself is in analogy-as-strategy.lisp

(refalg~define-refinement-algorithm
 (name Analogy)
 (parameter-list )
 (invokation-function anastrat~anastrat-invokation-function)
 (reinvokation-function anastrat~anastrat-invokation-function)
 )

;;;;;;;;;;;;;
;; EXP     ;;
;;;;;;;;;;;;;

;; The code itself is in exp.lisp

(refalg~define-refinement-algorithm
 (name exp)
 (parameter-list )
 (invokation-function expexp~exp-invokation-function)
 (reinvokation-function expexp~exp-invokation-function)
 )

;;;;;;;;;;;;;;
;; LPLANNER ;;
;;;;;;;;;;;;;;

;; ;; The code itself is at LASSAAD + SIEGFRIED

;;(refalg~define-refinement-algorithm
;; (name lplanner)
;; (parameter-list ())
;; (invokation-function lplan~lplanner-invokation-function)
;; (reinvokation-function lplan~lplanner-invokation-function)
;; )



;;;;;;;;;;;;;
;; ATP     ;;
;;;;;;;;;;;;;

;; The code itself is in atpa.lisp

(refalg~define-refinement-algorithm
 (name ATP)
 (parameter-list (apply-atp-function check-atp-out))
 (invokation-function atpa~atpa-invokation-function)
 (reinvokation-function atpa~atpa-invokation-function)
 )

;;;;;;;;;;;;;
;; CPLANNER;;
;;;;;;;;;;;;;

;; The code itself is in cplanner.lisp

(refalg~define-refinement-algorithm
 (name cplanner)
 (parameter-list (parameters
		  cycle-algs
		  crules
		  termination-condition))
 (invokation-function ana~invokation)
 (reinvokation-function ana~reinvokation)
 )
