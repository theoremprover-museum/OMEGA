(cri~def-control-rule select-hua-methods
		      (kind methods)
		      (if (always-true))
		      (then
		       (select (
				WEAKEN-M-A
				BIG-IMPE-M-B 
				FORALLI*-M 
				IMPI-M-B 
				ANDI-M-B 
				ANDE-M-F 
				DEFNI-M 
				DEFNE-M 
				)
		)))

(cri~set-used-control-rules! '(select-hua-methods))
