;(load "~/omega/leo-3/prog/leo-3/leo-tactics.lisp")

(defun landau=prove (probname tactic expanded-theory)
	(progn (oc=prove-pre (ot~read-proof-plan probname))
               (oc=call-leo-on-node (pds~label2node 'conc) tactic nil 300 expanded-theory '(= DEFINED EQUIV) nil)))

(oc=require-theory 'landau0)
(th~load-problems 'landau0)

(setq landau0-probs '(_EC3E13_HTHM _R_ITE_T6_THM _NOTIS_TH1_THM _10_T5_THM _ISSET_T1_THM _ONEI_THM _NOTIS_TH3_THM))


;(format t "**** Applying LEO in modus :fo-atp-cooperation '(landau0) to 'landau0-probs ****~%") 
;(mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation '(landau0))) landau0-probs) 

;(format t "**** Applying LEO in modus :standard '(landau0) to 'landau0-probs ****~%") 
;(mapcar #'(lambda (X) (landau=prove X :standard '(landau0))) landau0-probs)

;(format t "**** Applying LEO in modus :fo-atp-cooperation-st  '(landau0) to 'landau0-probs ****~%") 
;(mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-st '(landau0))) landau0-probs)

;(format t "**** Applying LEO in modus :fo-atp-cooperation-eir  '(landau0) to 'landau0-probs ****~%") 
;(mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-eir '(landau0))) landau0-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation  '(landau0) to 'landau0-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation '(landau0))) landau0-probs)

;; (format t "**** Applying LEO in modus :standard  '(landau0) to 'landau0-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :standard '(landau0))) landau0-probs)


(format t "**** Applying LEO in modus :fo-atp-cooperation-def  '() to 'landau0-probs ****~%") 
(mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-def '())) landau0-probs)

(Oc=require-theory 'landau1)
(th~load-problems 'landau1)

(setq landau1-probs '(_24_T9_THM _SATZ32K_HTHM _428_T2_HTHM _SINGLET_T1_HTHM  _SATZ31_HTHM _327_T16_HTHM _SATZ9A_HTHM))

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-st  '(landau0 landau1) to 'landau1-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-st '(landau0 landau1))) landau1-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-eir  '(landau0 landau1) to 'landau1-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-eir '(landau0 landau1))) landau1-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation  '(landau0 landau1) to 'landau1-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation '(landau0 landau1))) landau1-probs)

;; (format t "**** Applying LEO in modus :standard  '(landau0 landau1) to 'landau1-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :standard '(landau0 landau1))) landau1-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-st  '(landau1) to 'landau1-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-st '(landau1))) landau1-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-eir  '(landau1) to 'landau1-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-eir '(landau1))) landau1-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation  '(landau1) to 'landau1-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation '(landau1))) landau1-probs)

;; (format t "**** Applying LEO in modus :standard  '(landau1) to 'landau1-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :standard '(landau1))) landau1-probs)

(format t "**** Applying LEO in modus :fo-atp-cooperation-def  '() to 'landau1-probs ****~%") 
(mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-def '())) landau1-probs)

(oc=require-theory 'landau2)
(th~load-problems 'landau2)

(setq landau2-probs '(_SATZ73C_HTHM _255_T2_HTHM _54_T18_HTHM _EQPF12_HTHM _SATZ67E_HTHM _59_T11_HTHM _59_T16_HTHM))

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-st  '(landau2) to 'landau2-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-st '(landau2))) landau2-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-eir  '(landau2) to 'landau2-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-eir '(landau2))) landau2-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation  '(landau2) to 'landau2-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation '(landau2))) landau2-probs)

;; (format t "**** Applying LEO in modus :standard  '(landau2) to 'landau2-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :standard '(landau2))) landau2-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-st  '(landau2 landau1) to 'landau2-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-st '(landau2 landau1))) landau2-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-eir  '(landau2 landau1) to 'landau2-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-eir '(landau2 landau1))) landau2-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation  '(landau2 landau1) to 'landau2-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation '(landau2 landau1))) landau2-probs)

;; (format t "**** Applying LEO in modus :standard  '(landau2 landau1) to 'landau2-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :standard '(landau2 landau1))) landau2-probs)

(format t "**** Applying LEO in modus :fo-atp-cooperation-def  '() to 'landau2-probs ****~%") 
(mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-def '())) landau2-probs)


(oc=require-theory 'landau3)
(th~load-problems 'landau3)

(setq landau3-probs '(_4152_T31_HTHM _4150_T7_HTHM _3129_T8_HTHM _4153_T2_HTHM _5155_T14_HTHM _5160_T31_HTHM _SATZ148B_HTHM))

;; (format t "**** Applying LEO in modus :fo-atp-cooperation  '(landau3) to 'landau3-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation '(landau3))) landau3-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-st  '(landau3) to 'landau3-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-st '(landau3))) landau3-probs)


(format t "**** Applying LEO in modus :fo-atp-cooperation-def  '() to 'landau3-probs ****~%") 
(mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-def '())) landau3-probs)

(oc=require-theory 'landau4)
(th~load-problems 'landau4)

(setq landau4-probs '(_4D192_T8_HTHM _REMARK3B_HTHM _SATZD171_HTHM _INTD_T7_HTHM _ABSN_HTHM _M0DEQB_HTHM _SATZD197F_HTHM))

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-eir  '(landau4) to 'landau4-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-eir '(landau4))) landau4-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-st  '(landau4) to 'landau4-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-st '(landau4))) landau4-probs)

(format t "**** Applying LEO in modus :fo-atp-cooperation-def  '() to 'landau4-probs ****~%") 
(mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-def '())) landau4-probs)


(oc=require-theory 'landau5)
(th~load-problems 'landau5)

(setq landau5-probs '(_8283_T23_HTHM _C_SHIFTINV1_HTHM _8281_T12_HTHM _8285_T23_HTHM _T136_HTHM _DISTPO_HTHM _SATZ295_HTHM))

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-st  '(landau5) to 'landau5-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-st '(landau5))) landau5-probs)

;; (format t "**** Applying LEO in modus :fo-atp-cooperation-eir  '(landau5) to 'landau5-probs ****~%") 
;; (mapcar #'(lambda (X) (landau=prove X :fo-atp-cooperation-eir '(landau5))) landau5-probs)





