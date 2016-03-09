(cri~def-control-rule reject-intersectionI
		      (kind method)
		      (type normal)
		      (if (last-method intersectioni-m-b)
			  )
		      (then
		       (reject (intersectioni-m-b))))

(cri~def-control-rule reject-intersectionE
		      (kind method)
		      (type normal)
		      (if (last-method intersectione-m-f)
			  )
		      (then
		       (reject (intersectione-m-f))))


(cri~def-control-rule prefer-andI-m-b
		      (kind method)
		      (type normal)
		      (if (goal-matches ("goal" (and "o.o1" "o.o2"))))
		      (then
		       (prefer (andI-m-b))))

(cri~def-control-rule prefer-andE-m-f
		      (kind method)
		      (type normal)
		      (if (assumption-matches ("ass" (and "o.o1" "o.o2"))))
		      (then
		       (prefer (andE-m-f))))
