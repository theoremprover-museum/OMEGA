(in-package :omega)

;(cri~def-control-rule =ref-a-pos
;                      (kind methods)
;                      (if  (goal-matches ("goal" (= "b" "a"))))
;                      (then
;                       (prefer (
;                                (=ref-m-a ()("a"))
;                                ))))

(cri~def-control-rule idr-pos
		      (kind methods)
		      (if (and (and (thegoal ("goalformula"))
				    (assumption-matches ("ass" (group "set" "op" "e" "inv"))))
			       (positions-in-unquantified-form-of ("goalformula" ("op" "a1" "e") "pos"))))
		      (then
		       (prefer (
				(idr-i-m-b () ("pos"))
				(idr-e-m-f () ("pos"))
				(idr-i-m-a () ("pos"))
				(idr-e-m-a () ("pos"))
				))))

(cri~def-control-rule idl-pos
		      (kind methods)
		      (if (and (and (thegoal ("goalformula"))
				    (assumption-matches ("ass" (group "set" "op" "e" "inv"))))
			       (positions-in-unquantified-form-of ("goalformula" ("op" "e" "a1") "pos"))))
		      (then
		       (prefer (
				(idl-i-m-b () ("pos"))
				(idl-e-m-f () ("pos"))
				(idl-i-m-a () ("pos"))
				(idl-e-m-a () ("pos"))
				))))

(cri~def-control-rule invl-pos
		      (kind methods)
		      (if (and (and (thegoal ("goalformula"))
				    (assumption-matches ("ass" (group "set" "op" "e" "inv"))))
			       (positions-in-unquantified-form-of ("goalformula" ("op" ("inv" "a1") "a1") "pos"))))
		      (then
		       (prefer (
				(invl-i-m-b () ("pos"))
				(invl-e-m-f () ("pos"))
				(invl-i-m-a () ("pos"))
				(invl-e-m-a () ("pos"))
				))))

(cri~def-control-rule invr-pos
		      (kind methods)
		      (if (and (and (thegoal ("goalformula"))
				    (assumption-matches ("ass" (group "set" "op" "e" "inv"))))
			       (positions-in-unquantified-form-of ("goalformula" ("op" "a1" ("inv" "a1")) "pos"))))
		      (then
		       (prefer (
				(invr-i-m-b () ("pos"))
				(invr-e-m-f () ("pos"))
				(invr-i-m-a () ("pos"))
				(invr-e-m-a () ("pos"))
				))))

(cri~def-control-rule assoc-l-b-pos
		      (kind methods)
		      (if (and (and (thegoal ("goalformula"))
				    (assumption-matches ("ass" (group "set" "op" "e" "inv"))))
			       (positions-in-unquantified-form-of ("goalformula" ("op" ("op" "a1" "b1") "c1") "pos"))))
		      (then
		       (prefer (
				(assoc-l-m-b () ("pos"))
				))))

(cri~def-control-rule assoc-r-b-pos
		      (kind methods)
		      (if (and (and (thegoal ("goalformula"))
				    (assumption-matches ("ass" (group "set" "op" "e" "inv"))))
			       (positions-in-unquantified-form-of ("goalformula" ("op" "c1" ("op" "a1" "b1")) "pos"))))
		      (then
		       (prefer (	   
				 (assoc-r-m-b () ("pos"))
				))))
