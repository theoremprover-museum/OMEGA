(th~defagentdefault typed-set
		    (nic~use-nic-agents)
		    (oc=stop-use-resources)
		    (auto~set-auto-default-interval 10)
		    (nic~use-nic-heuristics)
		    (nic~add-command-agents '(solved-by-pl-atp))
		    ; (nic~add-command-agents '(solved-by-pl-atp counterexample-by-satchmo))
		    ;(csm~set-considered-commands '(solved-by-tps kappai kappae))
		    )
