(in-package :omega)

;;****************************************************************************
;;                                                                           *
;;  the* methods and supermethods needed for planning limit-the*orems.         *
;;___________________________________________________________________________*
;;                                                                           *
;; the* names of low level methods start with a capital letter as             *
;; opposed to tactics. the* names if supermethods (subplans) are completely   *
;; written in capital letters.                                               *
;;                                                                           *
;; the* manual entry of the* methods is extracted and presented in HTML        *
;; at http://www.ags.uni-sb.de/~omega/omegaindex/methods/methods-index.html  *
;;                                                                           *
;; Please send questions and comments to jzimmer@ags.uni-sb.de               *
;;****************************************************************************


;;*********************************************************
;;*  the* Solve-methods tell primitive                     *
;;*  equality- and inequality-constraints to the*          *
;;*  constraint solver and decompose complex constraints. *
;;*********************************************************

(infer~defmethod TellCS-m
		 ;(outline-mappings (((existent existent) TellCS-m-b)
		 (outline-mappings (((existent) TellCS-m-b)
				    ((closed) TellCS-m-f)))
		 (help "Tests consistency of inequalities and equations with constraint store."))

(meth~defmethod TellCS-m-b TellCS-m 
		(in limit)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (Meta-CS o metavar)
		  (rel (o num num) term)
		  (a num term) (b num term)
		  (result o term)
		  )
		 )
		
		;(premises L0)
		(application-condition
		 (mand (meta-p Meta-CS)
		       (var-in (prlnformula l1))
		       (test-CS (:symbol :CoSIE) L1)
		       ;;(tell-CS (:symbol :CoSIE) L1)
		       )
		 )
		(outline-computations
		 (result (tell-CS (:symbol :CoSIE) L1))
		 )
		(conclusions (- L1))
		(decl-content
		 ;(L0 ()  Meta-CS)
		 (L1 () (rel a b)   ("prove-CS" () ()))   
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit the*orems")
			(documentation
			 "This method - next to the* method <A HREF=\"./askcs-m-b.lml\">AskCS-m-b</A> -
provides the* main interface to the* constraint solver
<A HREF=\"http://www.ags.uni-sb.de/~~jzimmer/cosie/cosie.html\"> CoSIE </A> <BR> the*
method has the* precondition <FONT COLOR=\"#0000ff\">l0</FONT>
which has to be the* Meta-Variable introduced as a placeholder for the* answer constraint by the*
method <A HREF=\"./initialize-CS-m-b.lml\">Initialize-CS-m-b</A>. <BR>
the* method is applicable, if the* formula of the* conclusion <FONT COLOR=\"#0000ff\">l1</FONT> is a valid constraint
wrt. the* constraint language of CoSIE (this is checked by the* conditional test-CS),
the* constraint contains variables (var-in) and is consistent with the* current constraint store of CoSIE (tell-CS)."))
		(remark ("<U>Solve:</U><BR>
We restrict <TERM>(rel a b)</TERM>."
					;We choose <LISP>(get-term-to-restrict a b)</LISP> to be
					;<LISP>(get-verbalization-for-predicate rel a b)</LISP>
					;<LISP>(get-other-term a b)</LISP>.
			 
			 "")
			("<B>We restricted <TERM>(rel a b)</TERM></B>.<BR>"
			 "<B>We restrict <TERM>(rel a b)</TERM></B>.<BR>"
			 ))
		)

(meth~defmethod TellCS-m-f TellCS-m  
		(in limit)
		(rating 41)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (rel (o num num) term)
		  (a num term) (b num term)
		  (out o term)
		  (result o term)
		  )
		 )
		
		(premises L1)
		(application-condition
		 (mand (mnot (mequal (prlnhyps L1) (mlist L1)))  ;; if L1 is not a hypothesis node
		       (test-ass-cs (:symbol :CoSIE) L1))
		 )
		
		(outline-computations
		 (result (tell-ass-cs (:symbol :CoSIE) L1))
		 )
		
		(conclusions )
		
		(decl-content
		 (l1 () (rel a b))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation
			 "Like the method <A HREF=\"./tellcs-m-b.lml\">TellCS-m-b</A>, this
method provides an interface to the constraint solver CoSIE. <BR>
The method checks whether the formula of the premise <FONT COLOR=\"#0000ff\">l1</FONT> is a valid constraint wrt.
the constraint language of CoSIE (test-cs). If the test succeeds, the constraint is added to the current constraint
store, if possible. <BR>
This is important, because assumptions that contain constraints for CoSIE add relevant information
to the constraint store." ))
		
		(remark "" ;<BR>The assumption <TERM>(rel a b)</TERM>is introduced into the constraint store.<BR>
			)
		)


(infer~defmethod AskCS-m
		 ;(outline-mappings (((existent existent) AskCS-m-b)))
		 (outline-mappings (((existent) AskCS-m-b)))
		 (help "Tests entailment of a constraint."))

(meth~defmethod AskCS-m-b AskCS-m 
		(in limit)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (Meta-CS o metavar)
		  (rel (o num num) term)
		  (a num term) (b num term)
		  )
		 )
		
		;(premises L0)
		(application-condition
		 (mand (meta-p Meta-CS)
		       (mnot (var-in (prlnformula l1)))
		       ;;(test-CS (:symbol :CoSIE) (prlnformula L1))
		       (ask-CS (:symbol :CoSIE) L1)
		       )
		 )
		(conclusions (- L1))
		(decl-content
		 ;(L0 ()  Meta-CS)
		 (L1 () (rel a b)   ("prove-CS" () ()))   
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation
			 "This method - next to the method <A HREF=\"./tellcs-m-b.lml\">TellCS-m-b</A> -
provides the main interface to the constraint solver <A HREF=\"http://www.ags.uni-sb.de/~~jzimmer/cosie/cosie.html\"> CoSIE </A> <BR>
The method has the precondition <FONT COLOR=\"#0000ff\">l0</FONT>
which has to be the Meta-Variable introduced as a placeholder for the answer constraint by the
method <A HREF=\"./initialize-CS-m-b.lml\">Initialize-CS-m-b</A>. <BR>
The method is applicable, if the formula of the conclusion <FONT COLOR=\"#0000ff\">l1</FONT> is a valid constraint
wrt. the constraint language of CoSIE (this is checked by the conditional test-CS),
the constraint is a ground formula and is entailed by the current constraint store (ask-CS). <BR>
When AskCS-m-b is applied, the conclusion is removed from the planning goals."))

		(remark ("<U>Ask-CS:</U><BR>
The constraint <TERM>(rel a b)</TERM> is entailed by the constraint store."
;We choose <LISP>(get-term-to-restrict a b)</LISP> to be
;<LISP>(get-verbalization-for-predicate rel a b)</LISP>
;<LISP>(get-other-term a b)</LISP>.

			 "")
			(""
			 "";<B>The constraint <TERM>(rel a b)</TERM> is entailed by the constraint store</B>.<BR>
			 ))
		)

(infer~defmethod Solve*<-m
		 (outline-mappings (((existent existent nonexistent nonexistent) Solve*<-m-b)
				    )))


(meth~defmethod Solve*<-m-b Solve*<-m  
		(in limit)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables

		  (occs o poslist)
		  (sigma o sub)                               
		  (aa num term)(a1 num term) (b num term)(c num term)  
		  (=conjunct o term)
		  (asigma num term)
		  (bsigma num term) (csigma num term)
		  )
		 
		 )
		(premises L3 (+ L1) (+ L2))
		(application-condition
		 (mand (mnot (meta-p aa))
		       (mnot (meta-p a1))
		       (alphaunify aa a1 sigma)
		       )              ;;unify aa and a1
		 )
                (outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 (asigma (subst-apply sigma a1))
		 (bsigma (subst-apply sigma b))
		 (csigma (subst-apply sigma c))
		 )
		
		(conclusions (- L6))
		(decl-content
		 (L1 () =conjunct                    ("OPEN" () ()))
		 (L2 () (leq bsigma csigma)          ("OPEN" () ()))
		 
		 (L3 () (less aa b))
		 (L4 () (less asigma bsigma)         ("subst-forw" () (L3 L1)))
		 (l5 () (less asigma csigma)         ("<trans<=" () (L4 L2)))	
		 (L6 () (less a1 c)                  ("subst-back" () (L5 L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method Solve*<-m-b tries to reduce complex inequalities (that contain
the absolute value of a term) to simpler ones via unification with an assumption. <BR>
This assumption is given as a precondition <FONT COLOR=\"#0000ff\">l3</FONT>. If left hand side of the inequality
of the conclusion <FONT COLOR=\"#0000ff\">l6</FONT> can be unified with the left hand side of the assumption with
the MGU sigma, the method is applicable. <BR>
The application of the method produces two new subgoals: first the substitution sigma (written as conjunction of
equations <FONT COLOR=\"#0000ff\">l1</FONT>), and second the hopefully simpler inequality in <FONT COLOR=\"#0000ff\">l2</FONT>.
"))
		
		(remark ("
<U>Solve*<:</U><BR>
We want to prove <TERM>(less a1 c)</TERM>.<BR>
If <A HREF=\"ltp:get-local-verbalization L2\"> <TERM>(leq bsigma csigma)</TERM></A>
<A HREF=\"ltp:get-local-verbalization L1\"><LISP>(verbalize=conjunct =conjunct)</LISP></A>,<BR>
then as <A HREF=\"ltp:get-local-verbalization L3\"> <TERM>(less aa b)</TERM> </A><BR>
the goal <TERM>(less a1 c)</TERM> follows.
<BR><A HREF=\"ltp:get-detailed-verbalization L6\">-> More details</A>"
			 "
<U>Solve*<:</U><BR>
We want to prove <TERM>(less a1 c)</TERM>.<BR>
If <A HREF=\"ltp:get-local-verbalization L2\"> <TERM>(leq bsigma csigma)</TERM></A>
<A HREF=\"ltp:get-local-verbalization L1\"><LISP>(verbalize=conjunct =conjunct)</LISP></A>,<BR>
then with <A HREF=\"ltp:get-local-verbalization L3\"> <TERM>(less aa b)</TERM> </A><BR> we
can conclude:<BR>
  <TAB>  <TERM>a1</TERM><BR>
<TAB><TERM>=</TERM>       
    <TERM>aa</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>b</TERM><BR>
<TAB><TERM>leq</TERM>
    <TERM>c</TERM>.
")
			("<LISP>(verbalize-text-subproof-with-hyps l3)</LISP>
<LISP>(get-conclusion)</LISP><TERM>(less aa b)</TERM>.
<BR><LISP>(get-cause)</LISP><TERM>(leq bsigma csigma)</TERM>,
<BR><LISP>(get-conclusion)</LISP><TERM>(less a1 c)</TERM>.<BR>
"
			 "<LISP>(verbalize-cons-subproof-with-hyps l3)</LISP>
<LISP>(get-conclusion)</LISP>that <TERM>(less aa b)</TERM>.
<BR><LISP>(get-motivation)</LISP><LISP>(get-estimate-variants)</LISP><TERM>(less a1 c)</TERM>,
<BR><B>we must restrict <TERM>(leq bsigma csigma)</TERM><LISP>(verbalize=conjunct =conjunct)</LISP></B>.<BR>
")
;						 "First we show that <TERM>(less aa b)</TERM>.
;<BLOCKQUOTE>Proof:<LISP>(verbalize-cons-subproof-with-hyps l3)</LISP></BLOCKQUOTE>
;<LISP>(get-reasoning-variants)</LISP><B>we restrict <TERM>(leq bsigma csigma)</TERM></B>."

			))

(infer~defmethod Solve*<-<=-m
		 (outline-mappings (((existent existent nonexistent nonexistent) Solve*<-<=-m-b)
				    )))


(meth~defmethod Solve*<-<=-m-b Solve*<-<=-m  
		(in limit)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables

		  (occs o poslist)
		  (sigma o sub)                               
		  (aa num term)(a1 num term) (b num term)(c num term)  
		  (=conjunct o term)
		  (asigma num term)
		  (bsigma num term) (csigma num term)
		  )
		 
		 )
		(premises L3 (+ L1) (+ L2))
		(application-condition
		 (mand (mnot (meta-p aa))  ;; do not accept the trivial cases
		       (mnot (meta-p a1))
		       (alphaunify aa a1 sigma))
		 )
                (outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 (asigma (subst-apply sigma a1))
		 (bsigma (subst-apply sigma b))
		 (csigma (subst-apply sigma c))
		 )
		
		(conclusions (- L6))
		(decl-content
		 (L1 () =conjunct                    ("OPEN" () ()))
		 (L2 () (leq bsigma csigma)          ("OPEN" () ()))
		 
		 (L3 () (less aa b))
		 (L4 () (less asigma bsigma)         ("subst-forw" () (L3 L1)))
		 (l5 () (leq asigma csigma)          ("solved" () (L4 L2)))	;; inequality-trans
		 (L6 () (leq a1 c)                   ("subst-back" () (L5 L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method Solve*<-m-b tries to reduce complex inequalities (that contain
the absolute value of a term) to simpler ones via unification with an assumption. <BR>
This assumption is given as a precondition <FONT COLOR=\"#0000ff\">l3</FONT>. If left hand side of the inequality
of the conclusion <FONT COLOR=\"#0000ff\">l6</FONT> can be unified with the left hand side of the assumption with
the MGU sigma, the method is applicable. <BR>
The application of the method produces two new subgoals: first the substitution sigma (written as conjunction of
equations <FONT COLOR=\"#0000ff\">l1</FONT>), and second the hopefully simpler inequality in <FONT COLOR=\"#0000ff\">l2</FONT>.
"))
		
		(remark ("
<U>Solve*<-<=:</U><BR>
We want to prove <TERM>(leq a1 c)</TERM>.<BR>
If <A HREF=\"ltp:get-local-verbalization L2\"> <TERM>(leq bsigma csigma)</TERM></A>
<A HREF=\"ltp:get-local-verbalization L1\"><LISP>(verbalize=conjunct =conjunct)</LISP></A>,<BR>
then as <A HREF=\"ltp:get-local-verbalization L3\"> <TERM>(less aa b)</TERM> </A><BR>
the goal <TERM>(leq a1 c)</TERM> follows.
<BR><A HREF=\"ltp:get-detailed-verbalization L6\">-> More details</A>"
			 "
<U>Solve*<:</U><BR>
We want to prove <TERM>(leq a1 c)</TERM>.<BR>
If <A HREF=\"ltp:get-local-verbalization L2\"> <TERM>(leq bsigma csigma)</TERM></A>
<A HREF=\"ltp:get-local-verbalization L1\"><LISP>(verbalize=conjunct =conjunct)</LISP></A>,<BR>
then with <A HREF=\"ltp:get-local-verbalization L3\"> <TERM>(less aa b)</TERM> </A><BR> we
can conclude:<BR>
  <TAB>  <TERM>a1</TERM><BR>
<TAB><TERM>=</TERM>       
    <TERM>aa</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>b</TERM><BR>
<TAB><TERM>leq</TERM>
    <TERM>c</TERM>.
")
			("<LISP>(verbalize-text-subproof-with-hyps l3)</LISP>
<LISP>(get-conclusion)</LISP><TERM>(less aa b)</TERM>.
<BR><LISP>(get-cause)</LISP><TERM>(leq bsigma csigma)</TERM>,
<BR><LISP>(get-conclusion)</LISP><TERM>(leq a1 c)</TERM>.<BR>
"
			 "<LISP>(verbalize-cons-subproof-with-hyps l3)</LISP>
<LISP>(get-conclusion)</LISP>that <TERM>(less aa b)</TERM>.
<BR><LISP>(get-motivation)</LISP><LISP>(get-estimate-variants)</LISP><TERM>(leq a1 c)</TERM>,
<BR><B>we must restrict <TERM>(leq bsigma csigma)</TERM><LISP>(verbalize=conjunct =conjunct)</LISP></B>.<BR>
")
			))

(infer~defmethod Solve*<=-<-m
		 (outline-mappings (((existent existent nonexistent nonexistent) Solve*<=-<-m-b)
				    )))


(meth~defmethod Solve*<=-<-m-b Solve*<=-<-m  
		(in limit)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables

		  (occs o poslist)
		  (sigma o sub)                               
		  (aa num term)(a1 num term) (b num term)(c num term)  
		  (=conjunct o term)
		  (asigma num term)
		  (bsigma num term) (csigma num term)
		  )
		 
		 )
		(premises L3 (+ L1) (+ L2))
		(application-condition
		 (mand (mnot (meta-p aa))  ;; do not accept the trivial cases
		       (mnot (meta-p a1))
		       (alphaunify aa a1 sigma))
		 )
                (outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 (asigma (subst-apply sigma a1))
		 (bsigma (subst-apply sigma b))
		 (csigma (subst-apply sigma c))
		 )
		
		(conclusions (- L6))
		(decl-content
		 (L1 () =conjunct                    ("OPEN" () ()))
		 (L2 () (less bsigma csigma)         ("OPEN" () ()))
		 
		 (L3 () (leq aa b))
		 (L4 () (leq asigma bsigma)          ("subst-forw" () (L3 L1)))
		 (l5 () (less asigma csigma)         ("<=trans<" () (L4 L2)))	
		 (L6 () (less a1 c)                  ("subst-back" () (L5 L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method Solve*<-m-b tries to reduce complex inequalities (that contain
the absolute value of a term) to simpler ones via unification with an assumption. <BR>
This assumption is given as a precondition <FONT COLOR=\"#0000ff\">l3</FONT>. If left hand side of the inequality
of the conclusion <FONT COLOR=\"#0000ff\">l6</FONT> can be unified with the left hand side of the assumption with
the MGU sigma, the method is applicable. <BR>
The application of the method produces two new subgoals: first the substitution sigma (written as conjunction of
equations <FONT COLOR=\"#0000ff\">l1</FONT>), and second the hopefully simpler inequality in <FONT COLOR=\"#0000ff\">l2</FONT>.
"))
		(remark ("
<U>Solve*<=-<:</U><BR>
We want to prove <TERM>(less a1 c)</TERM>.<BR>
If <A HREF=\"ltp:get-local-verbalization L2\"> <TERM>(less bsigma csigma)</TERM></A>
<A HREF=\"ltp:get-local-verbalization L1\"><LISP>(verbalize=conjunct =conjunct)</LISP></A>,<BR>
then as <A HREF=\"ltp:get-local-verbalization L3\"> <TERM>(leq aa b)</TERM> </A><BR>
the goal <TERM>(less a1 c)</TERM> follows.
<BR><A HREF=\"ltp:get-detailed-verbalization L6\">-> More details</A>"
			 "
<U>Solve*<:</U><BR>
We want to prove <TERM>(less a1 c)</TERM>.<BR>
If <A HREF=\"ltp:get-local-verbalization L2\"> <TERM>(less bsigma csigma)</TERM></A>
<A HREF=\"ltp:get-local-verbalization L1\"><LISP>(verbalize=conjunct =conjunct)</LISP></A>,<BR>
then with <A HREF=\"ltp:get-local-verbalization L3\"> <TERM>(leq aa b)</TERM> </A><BR> we
can conclude:<BR>
  <TAB>  <TERM>a1</TERM><BR>
<TAB><TERM>=</TERM>       
    <TERM>aa</TERM><BR>
<TAB><TERM>leq</TERM>     
    <TERM>b</TERM><BR>
<TAB><TERM>less</TERM>
    <TERM>c</TERM>.
")
			("<LISP>(verbalize-text-subproof-with-hyps l3)</LISP>
<LISP>(get-conclusion)</LISP><TERM>(leq aa b)</TERM>.
<BR><LISP>(get-cause)</LISP><TERM>(less bsigma csigma)</TERM>,
<BR><LISP>(get-conclusion)</LISP><TERM>(less a1 c)</TERM>.<BR>
"
			 "<LISP>(verbalize-cons-subproof-with-hyps l3)</LISP>
<LISP>(get-conclusion)</LISP>that <TERM>(leq aa b)</TERM>.
<BR><LISP>(get-motivation)</LISP><LISP>(get-estimate-variants)</LISP><TERM>(less a1 c)</TERM>,
<BR><B>we must restrict <TERM>(less bsigma csigma)</TERM><LISP>(verbalize=conjunct =conjunct)</LISP></B>.<BR>
")

		)
		)



(infer~defmethod Solve*>-m
		 (outline-mappings (((existent existent nonexistent nonexistent) Solve*>-m-b)
				    )))

(meth~defmethod Solve*>-m-b Solve*>-m 
		(in limit)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables

		  (occs o poslist)
		  (sigma o sub)                               
		  (a num term)(a1 num term) (b num term)(c num term)  
		  (=conjunct o term)
		  (asigma num term)
		  (bsigma num term) (csigma num term)
		  )
		 
		 )
		(premises L3 (+ L1) (+ L2))
		(application-condition
		 (mand (mnot (meta-p a))  ;; do not accept the trivial cases
		       (mnot (meta-p a1))
		       (alphaunify a a1 sigma))              ;;unify a and a1
		 )
		
                (outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 (asigma (subst-apply sigma a1))
		 (bsigma (subst-apply sigma b))
		 (csigma (subst-apply sigma c))
		 )
		
		(conclusions (- L8))
		(decl-content
		 (L1 () =conjunct                    ("OPEN" () ()))
		 (L2 () (leq csigma bsigma)          ("OPEN" () ()))
		 
		 (L3 () (greater a b))
		 (L4 () (less b a)                      ("solved" () (L3))) ;; eigentlich greater2less
		 (L5 () (less bsigma asigma)            ("subst-forw" () (L4 L1)))
		 (l6 () (less csigma asigma)            ("<=trans<" () (L2 L5)))
		 (L7 () (greater asigma csigma)         ("solved" ()  (L6)))   ;;eigentlich less2greater
		 (L8 () (greater a1 c)                  ("subst-back" () (L7 L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "This method does is very similar to the method
<A HREF=\"./initialize-cs-m-b.lml\">Solve*<-m-b</A>, but it handles inequalities of the form a1 > c"))
		
		(remark ("
<U>Solve*>:</U><BR>
We want to prove <TERM>(greater a1 c)</TERM>.<BR>
If <A HREF=\"ltp:get-local-verbalization L2\"> <TERM>(leq csigma bsigma)</TERM></A>
<A HREF=\"ltp:get-local-verbalization L1\"><LISP>(verbalize=conjunct =conjunct)</LISP></A>,<BR>
then as <A HREF=\"ltp:get-local-verbalization L3\"> <TERM>(greater a b)</TERM> </A>.<BR>
the goal <TERM>(greater a1 c)</TERM> follows.
<BR><A HREF=\"ltp:get-detailed-verbalization L8\">-> More details</A>"
			 "
<U>Solve*>:</U><BR>
We want to prove <TERM>(greater a1 c)</TERM>.<BR>
If <A HREF=\"ltp:get-local-verbalization L2\"> <TERM>(leq csigma bsigma)</TERM></A>
<A HREF=\"ltp:get-local-verbalization L1\"><LISP>(verbalize=conjunct =conjunct)</LISP></A>,<BR>
then with <A HREF=\"ltp:get-local-verbalization L3\"> <TERM>(greater a b)</TERM> </A> we
can conclude:<BR>
  <TAB>  <TERM>a1</TERM><BR>
<TAB><TERM>=</TERM>       
    <TERM>a</TERM><BR>
<TAB><TERM>greater</TERM>     
    <TERM>b</TERM><BR>
<TAB><TERM>geq</TERM>
    <TERM>c</TERM>.
")("<LISP>(verbalize-text-subproof-with-hyps l3)</LISP>
<LISP>(get-conclusion)</LISP><TERM>(greater a b)</TERM>.
<BR><LISP>(get-cause)</LISP><TERM>(leq csigma bsigma)</TERM>,
<BR><LISP>(get-conclusion)</LISP><TERM>(greater a1 c)</TERM>.<BR>
"
   "<LISP>(verbalize-cons-subproof-with-hyps l3)</LISP>
<LISP>(get-conclusion)</LISP>that <TERM>(greater a b)</TERM>.
<BR><LISP>(get-motivation)</LISP><LISP>(get-estimate-variants)</LISP><TERM>(greater a1 c)</TERM>,
<BR><B>we must restrict <TERM>(leq csigma bsigma)</TERM><LISP>(verbalize=conjunct =conjunct)</LISP></B>.<BR>
"
;   "First we show that <TERM>(greater a b)</TERM>.
;<BLOCKQUOTE>Proof:<LISP>(verbalize-cons-subproof-with-hyps l3)</LISP></BLOCKQUOTE>
;<LISP>(get-reasoning-variants)</LISP><B>we restrict <TERM>(leq csigma bsigma)</TERM></B>.
;"
   ))
		)

(infer~defmethod Solve*>=->=-m
		 (outline-mappings (((existent existent nonexistent nonexistent) Solve*>=->=-m-b)
				    )))

(meth~defmethod Solve*>=->=-m-b Solve*>=->=-m 
		(in limit)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables

		  (occs o poslist)
		  (sigma o sub)                               
		  (a num term)(a1 num term) (b num term)(c num term)  
		  (=conjunct o term)
		  (asigma num term)
		  (bsigma num term) (csigma num term)
		  )
		 
		 )
		(premises L3 (+ L1) (+ L2))
		(application-condition
		 (mand (mnot (meta-p a))  
		       (mnot (meta-p a1))
		       (alphaunify a a1 sigma))     
		 )
		
                (outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 (asigma (subst-apply sigma a1))
		 (bsigma (subst-apply sigma b))
		 (csigma (subst-apply sigma c))
		 )
		
		(conclusions (- L8))
		(decl-content
		 (L1 () =conjunct                    ("OPEN" () ()))
		 (L2 () (leq csigma bsigma)          ("OPEN" () ()))
		 
		 (L3 () (geq a b))
		 (L4 () (leq b a)                      ("solved" () (L3))) 
		 (L5 () (leq bsigma asigma)            ("subst-forw" () (L4 L1)))
		 (l6 () (leq csigma asigma)            ("<=trans" () (L2 L5)))
		 (L7 () (geq asigma csigma)         ("solved" ()  (L6)))   
		 (L8 () (geq a1 c)                  ("subst-back" () (L7 L1)))
		 )
		(manual (author "MP JZ")
			(examples "Limit theorems")
			(documentation "This method does is very similar to the method
<A HREF=\"./initialize-cs-m-b.lml\">Solve*<-m-b</A>.")))

(infer~defmethod Solve*>->=-m
		 (outline-mappings (((existent existent nonexistent nonexistent) Solve*>=->=-m-b)
				    )))

(meth~defmethod Solve*>->=-m-b Solve*>->=-m 
		(in limit)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables

		  (occs o poslist)
		  (sigma o sub)                               
		  (a num term)(a1 num term) (b num term)(c num term)  
		  (=conjunct o term)
		  (asigma num term)
		  (bsigma num term) (csigma num term)
		  )
		 
		 )
		(premises L3 (+ L1) (+ L2))
		(application-condition
		 (mand (mnot (meta-p a))  
		       (mnot (meta-p a1))
		       (alphaunify a a1 sigma))     
		 )
		
                (outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 (asigma (subst-apply sigma a1))
		 (bsigma (subst-apply sigma b))
		 (csigma (subst-apply sigma c))
		 )
		
		(conclusions (- L8))
		(decl-content
		 		 (L1 () =conjunct                    ("OPEN" () ()))
		 (L2 () (leq csigma bsigma)          ("OPEN" () ()))
		 
		 (L3 () (greater a b))
		 (L4 () (less b a)                      ("solved" () (L3))) ;; eigentlich greater2less
		 (L5 () (less bsigma asigma)            ("subst-forw" () (L4 L1)))
		 (l6 () (less csigma asigma)            ("<=trans<" () (L2 L5)))
		 (L7 () (geq asigma csigma)         ("solved" ()  (L6)))   ;;eigentlich less2greater
		 (L8 () (geq a1 c)                  ("subst-back" () (L7 L1)))
		 )
		(manual (author "MP JZ")
			(examples "Limit theorems")
			(documentation "This method does is very similar to the method
<A HREF=\"./initialize-cs-m-b.lml\">Solve*<-m-b</A>.")))


;;*********************************************
;;*       Initialize-Constraint-Solver,       *
;;*     CaseSplit and Chaining and Indirect   *
;;*********************************************
(infer~defmethod Initialize-CS-m
		 (outline-mappings (((existent nonexistent nonexistent) Initialize-CS-m-b)
				    )))
(meth~defmethod Initialize-CS-m-b
		Initialize-CS-m
		(in limit)
		(rating 0)
		(reasoning :planning :middle-out)
		(declarations
		 
		 (sorted-meta-variables
		  (CS-meta o term)
		  (Thm o term)
		  (hypline o prln)
		  ))
		
		(premises (+ L1) (+ L2))
		
		(application-condition
		 (mand (mnot (meta-p Thm))
		       (mnot (meta-var-in-nodes (prlnhyps l4)))
                       (initialize-cs (:symbol :CoSIE)
                                      CS-meta))
		 )
		(outline-actions
		 (L2 (sponsor L1))
		 )
		(conclusions (- L4))
		
		(decl-content
		 (L1 ()   CS-meta                    ("OPEN" () ())) 
		 (L2 (L1) Thm                        ("OPEN" () ()))
		 (L3 ()   (implies CS-meta Thm)      ("ImpI" () (L2)))
		 (L4 ()   Thm                        ("ImpE" () (L3 L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method Initialize-CS-m-b intializes the constraint
solver CoSIE. It also introduces a metavariable <FONT COLOR=\"#00aa10\">CS-meta</FONT> for the
answer constraint of CoSIE (i.e. the conjunction of all constraint accumulated during planning). <BR>
(While planning a limit-theorem, the subgoal <FONT COLOR=\"#ee0000\">l1</FONT> is closed by the method
<A HREF=\"./prove-cs-answer-m-b.lml\">Prove-cs-answer-m-b</A>). <BR>
The method is applicable, if the goal is not a meta-variable, there's no meta-variable in the hypotheses
of the goal and if the constraint solver is reachable. The method deletes the conclusion
<FONT COLOR=\"#0000dd\">l4</FONT> from the planning state and adds the same formula as a subgoal
<FONT COLOR=\"#ee0000\">l2</FONT> with <FONT COLOR=\"#ee0000\">l1</FONT> added to the set of hypotheses.
"))
		
		(remark ("<U>Initialize-CS</U><BR>Let <TERM>CS-meta</TERM>.")
			("Let <TERM>CS-meta</TERM>.
<BR><LISP>(verbalize-text-next l2)</LISP>" ""))
		)

(infer~defmethod Prove-cs-answer-m
		 (outline-mappings (((existent) Prove-cs-answer-m-b)
				    ))
		 (help "Proves the answer constraint."))

(meth~defmethod Prove-cs-answer-m-b Prove-cs-answer-m
		(in limit)
		(rating 10)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (CS-meta o term)
		  
		  )
		 )
		
		(application-condition
		 (meta-p CS-meta)  ;(mequal CS-meta (cs-get-meta (:symbol :CoSIE)))
		 )                 ;;noch nicht richtig
		
		(conclusions (- L2))
		(decl-content
		 (l2 () CS-meta                        ("prove-cs" () ()))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark "")
		)


(infer~defmethod DomainCaseSplit-m
		 (outline-mappings (((existent nonexistent nonexistent nonexistent) DomainCaseSplit-m-b)
				    ))
		 (parameter-types term term)
		 (help "The domain specific Case-Split inference."))

(meth~defmethod DomainCaseSplit-m-b DomainCaseSplit-m
		(in limit)
	        (rating 0)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (param o prln)
		  (term1 num term)
		  (term2 num term)
		  (theorem o term)
		  (params o termlist)
		  (Thm o term)
		  (assu o prln)
		  (assumption o prln)
		  (Delta o prlnlist)
		  (positions o poslist)
		  )
		 
		 )

		(parameters term1 term2)
		
		(premises (+ L5) (+ L6) (+ L7))
		(application-condition
		 (mand (bound-p term1)
		       (bound-p term2)))
		
		(expansion-computations
		 (params (mlist term1 term2))
		 ;;(theorem (th-ass (:string "less-greater-equal")))
		 )
		(conclusions (- L10))
		
		(decl-content
		 (L1 () (less term1 term2)                             ("HYP" () ()))
		 (L2 () (or (greater term1 term2) (= term1 term2))     ("HYP" () ()))
		 (L3 () (greater term1 term2)                          ("HYP" () ()))
		 (L4 () (= term1 term2)                                ("HYP" () ()))
		 
		 (L5 (L2 L3) Thm                   ("OPEN" () ()))
		 (L6 (L2 L4) Thm                   ("OPEN" () ()))
		 (L7 (L1)    Thm                   ("OPEN" () ()))
		 
		 (L8 (L2) Thm                      ("solved" () (L2 L5 L6)))
		 (L10 ()  Thm                      ("solved" () (L7 L8)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark "Doing a domain specific case split over (term 1 < term2) || (term1 = term2) || (term1 > term2).")
		)


(infer~defmethod Imp2or-m
		 (outline-mappings (((existent nonexistent) Imp2or-m-b)))
		 ;;(parameter-types term)
		 (help "Tactic imp2or."))


(meth~defmethod imp2or-m-b imp2or-m
		(in limit)
		(rating 55)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (psi o term)
		  (alpha o term)
		  (beta o term)
		  (param o term)
		       )
		 )

		;;(parameters param)
		
		(premises (+ L10))
		(conclusions (- L30))

		(application-condition
		 )
		 
		(outline-computations
		 )
		
		(decl-content
		 (L10 () (implies phi psi)            ("OPEN" () ()))
		 
		 (l30 () (or (not phi) psi)      ("closed" ()(l10)))
		 )
		
		(manual (author "MP")
			(examples "all kind of problems")
			(documentation ""))
		
		(remark "")
		)




(infer~defmethod Chain-m
		 (outline-mappings (((existent existent nonexistent) Chain-m-b)
				    ))
		 (help "The Chaining inference."))

(meth~defmethod Chain-m-b Chain-m
		(in limit)
	        (rating 1)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (phi o)
		  (psi o)
		  (zeta o)
		  )
		 )
		(premises L3 (+ L4))
		(conclusions (- L7))
		
		(decl-content
		 (L1 () phi              ("Hyp" () ()))
		 (L2 () psi              ("Hyp" () ()))
		 
		 (L3 ()       (implies phi psi))
		 
		 (L4 (L1 L2) zeta                 ("OPEN" () ()))
		 
		 (L5 (L1)     (implies psi zeta)  ("ImpI" () (L4)))
		 
		 (L6 (L1)     zeta                ("ImpE" () (L2 L5)))
		 
		 (L7 ()       (implies phi zeta)  ("ImpI" () (L6)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark "");Chaining (implies A B) and (implies B C) to (implies A C) .)
		)


;;*********************************************
;;*  Complex Estimate with CAS                *
;;*********************************************
(infer~defmethod ComplexEstimate<-m
		 (outline-mappings (((existent
				      ;;existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     ComplexEstimate<-m-b)))
		 (parameter-types ndline position))

(meth~defmethod ComplexEstimate<-m-b ComplexEstimate<-m
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (result o list)  (a num)       (Asigma num)   (B num)        (k num)  
		  (l num)          (lsigma num)  (epsilon num)  (sigma o sub)  (ksigma num)
		  (M num const)    (F o term)    (assumption o prln)
		  (e1 num) (position o pos)
		  (just-params o termlist)
		  (=conjunct o term)
		  ))
		
		(parameters assumption position)

		(premises (+ L01) (+ L02) (+ L03) (+ L04))
		
		(application-condition
		 (mand (bound-p assumption) ;; HACK bis parameter-Behandlung richtig
		       (mbind A (termatpos (prlnformula assumption)
					   (posappend position (:position (1 1)))))
		       
		       (getcassubst A B sigma)           ;; get the mystic substitution
		       (mbind Asigma (subst-apply sigma A))
		       (casextract Asigma B k l sigma)   ;; compute linear combination
		       
		       (mnot (mequal k (:term 0)))       ;; do not accept the trivial case
		       ;;(mnot (mand (mequal k (:term 1)) ;; but do accept this one (this is important for
		       ;;	   (mequal l (:term 0)))) ;; some problems... :-(
		       (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		       (no-Eigenvars-in (:symbol CoSIE) M)
		       )
		 )
		(outline-computations
		 (just-params (mcons assumption (mlist position)))
		 )
		
		(expansion-computations
		 (pos1 (:position (2 1)))
		 (pos (:position ()))
		 (the-cas (:symbol mass)))
		
		(conclusions (- Lthm))
		(decl-content
		 ;;(L00 () (less (absval A) e1))
		 (L01 () (leq (absval k) M)                               ("OPEN" () ()))
		 (L02 () (less (absval Asigma) (div epsilon (times 2 M))) ("OPEN" () ()))
		 (L03 () (less (absval l) (div epsilon 2))                ("OPEN" () ()))
		 (L04 () (less 0 M)                                       ("OPEN" () ()))
		 ;;(L05 () (leq 0 (absval Asigma))                          ("OPEN" () ()))
		 
		 (L0 () (= B B)                                           ("=ref" (B) ()))
		 (L1 () (= B (plus (times k Asigma) l))                   ("cas" (pos the-cas) (L0)))
		 (L1 () (= B (plus (times k Asigma) l))                   ("solved" () ()))
		 
		 (L2 () (leq (absval B) (plus (absval (times k Asigma))
					      (absval l)))                ("triangle" () (L1)))
		 
		 (L3 () (leq (absval B) (plus (times (absval k)
						     (absval Asigma))
					      (absval l)))                ("abs-mult" (pos1) (L2)))
		 
		 (L4a () (leq (times (absval k)
				     (absval Asigma))
			      (times M
				     (absval Asigma)))                    ("solved" ()
									   (L01)))
		                                              ;;ist zu viel /statt <=mult-r L05
		 (L4 () (leq (plus (times (absval k)
					  (absval Asigma))
				   (absval l))
			     (plus (times M (absval Asigma))
				   (absval l)))                           ("<=add-r" ()
									   (L4a)))
		 (L5 () (leq (absval B)
			     (plus (times M (absval Asigma))
				   (absval l)))                           ("<=trans" () (L3 L4)))
		 (L6 () (less (times M (absval Asigma))
			      (times M (div epsilon (times 2 M))))        ("<mult-l" ()
									   (L02 L04)))
		 (L7 () (less (plus (times M (absval Asigma))
				    (absval l))
			      (plus (times M (div epsilon (times 2 M)))
				    (absval l)))                          ("<add-r" () (L6)))
		 (L8 () (less (absval B)
			      (plus (times M (div epsilon (times 2 M)))
				    (absval l)))                          ("<=trans<" () (L5 L7)))
		 (L9 () (less (plus (times M (div epsilon (times 2 M)))
				    (absval l))
			      (plus (times M (div epsilon (times 2 M)))
				    (div epsilon 2)))                     ("<add-l" () (L03)))
		 (L10 () (less (absval B)
			       (plus (times M (div epsilon (times 2 M)))
				     (div epsilon 2)))                    ("<trans" () (L8 L9)))
		 
		 (Lthm () (less (absval B) epsilon)                       ("solved" ()
									   (L10)));;statt "fixer-arith"
		 )
		
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method ComplexEstimate<-m-b is a complex estimation method
that tries to prove the goal <FONT COLOR=\"#0000ff\">lthm</FONT>, by rewriting the term <FONT COLOR=\"#00aa10\">B</FONT>
as a linear combination of the subterm <FONT COLOR=\"#00aa10\">A</FONT> of an assumption after the application
of the mystical substitution <FONT COLOR=\"#00aa10\">sigma</FONT> that is computed by the conditional 'getcassubst'. <BR>
The method is applicable, if MAPLE(tm) is able to compute the linear combination shown in line <FONT COLOR=\"#0000ff\">l1</FONT>.
Additionally, the linear combination must not be trivial (i.e., <FONT COLOR=\"#00aa10\">k</FONT> is 0 or
<FONT COLOR=\"#00aa10\">k</FONT> is 1 and  <FONT COLOR=\"#00aa10\">l</FONT> is 0). 
The application of this method produces four (simpler) inequalities as subgoals, involving an auxiliary variable
<FONT COLOR=\"#00aa10\">M</FONT>.
"))
		
		(remark 
("<U>ComplexEstimate<:</U><BR>
We need to estimate the magnitude of
<BR><TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>.<BR>
To do this, we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>This goal can be shown in three steps:
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists a real number <TERM>M</TERM> such that <TERM> (less (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L02\"> 2. </A> <TERM> (less (absval Asigma) (div epsilon (times 2 M))) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (less (absval l) (div epsilon 2))</TERM>,
</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR><A HREF=\"ltp:get-detailed-verbalization Lthm\">-> More details</A>"

"<U>ComplexEstimate<(detailed):</U><BR>
In order to estimate the magnitude of the complicated term
<BR><TERM>(less (absval B) epsilon)</TERM>, <BR>
we represent it as the linear combination
<BR><TERM>(absval (plus (times k Asigma) l))</TERM>.
<BR><BR>Now we use the Triangle Inequality and obtain<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>The magnitude of <BR><TERM>(plus (absval (times k Asigma)) (absval l))</TERM><BR>
can be estimated easily by the following three steps:<BR>
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists a real number <TERM>M</TERM> such that <TERM> (less (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L02\"> 2. </A> <TERM> (less (absval Asigma) (div epsilon (times 2 M))) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (less (absval l) (div epsilon 2))</TERM>,
</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>=</TERM>   
<TERM>(plus (absval (times k Asigma)) l))</TERM>
<BR>
<TAB><TERM>leq</TERM>   
<TERM>(plus (absval (times k Asigma)) (absval l)))</TERM>
<BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>,
<BR>and therefore
<TERM>(less (absval B) epsilon)</TERM>,<BR>
which was to be shown.
")
("
<LISP>(header)</LISP>
<TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>,<BR>
we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR><LISP>(get-complex-estimate-variants)</LISP>
<LISP>(increase-depth)</LISP><BR><BLOCKQUOTE>
<LISP>(item)</LISP> There exists a real number <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l01)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon (times 2 M)))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l02)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval l) (div epsilon 2))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l03)</LISP></BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
</BLOCKQUOTE>
<LISP>(get-reasoning-variants)</LISP><LISP>(get-conclusion)</LISP> <BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR>"
 "
<LISP>(header)</LISP>
<TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>,<BR>
we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR><LISP>(get-motivation)</LISP><LISP>(get-action-variants)</LISP>
<TERM>(less (plus (absval (times k Asigma)) (absval l)) epsilon)</TERM>,
<BR><LISP>(get-motivation2)</LISP>show that:<LISP>(increase-depth)</LISP><BR>
<BLOCKQUOTE>
<LISP>(item)</LISP> There exists a real number <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l01)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon (times 2 M)))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l02)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval l) (div epsilon 2))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l03)</LISP></BLOCKQUOTE>
</BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
<LISP>(get-restriction-variants)</LISP><BR>
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR>")
))

(infer~defmethod ComplexEstimate<=-m
		 (outline-mappings (((existent
				      ;;existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     ComplexEstimate<=-m-b)))
		 (parameter-types ndline position))

(meth~defmethod ComplexEstimate<=-m-b ComplexEstimate<=-m
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (result o list)  (a num)       (Asigma num)   (B num)        (k num)  
		  (l num)          (lsigma num)  (epsilon num)  (sigma o sub)  (ksigma num)
		  (M num const)    (F o term)    (assumption o prln)
		  (e1 num) (position o pos)
		  (just-params o termlist)
		  (=conjunct o term)
		  ))
		
		(parameters assumption position)

		(premises (+ L01) (+ L02) (+ L03) (+ L04))
		
		(application-condition
		 (mand (bound-p assumption)
		       (mbind A (termatpos (prlnformula assumption)
					   (posappend position (:position (1 1)))))
		       
		       (getcassubst A B sigma)           ;; get the mystic substitution
		       (mbind Asigma (subst-apply sigma A))
		       (casextract Asigma B k l sigma)   ;; compute linear combination
		       
		       (mnot (mequal k (:term 0)))       ;; do not accept the trivial case
		       ;;(mnot (mequal k (:term 1))) ;; but do accept this one (this is important for
		       ;;	   (mequal l (:term 0)))) ;; some problems... :-(
		       (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		       (no-Eigenvars-in (:symbol CoSIE) M)
		       )
		 )
		(outline-computations
		 (just-params (mcons assumption (mlist position)))
		 )
		
		(expansion-computations
		 (pos1 (:position (2 1)))
		 (pos (:position (2)))
		 (the-cas (:symbol mcas)))
		
		(conclusions (- Lthm))
		(decl-content
		 ;;(L00 () (less (absval A) e1))
		 (L01 () (leq (absval k) M)                              ("OPEN" () ()))
		 (L02 () (leq (absval Asigma) (div epsilon (times 2 M))) ("OPEN" () ()))
		 (L03 () (leq (absval l) (div epsilon 2))                ("OPEN" () ()))
		 (L04 () (less 0 M)                                      ("OPEN" () ()))
		 ;;(L05 () (leq 0 (absval Asigma))                       ("OPEN" () ()))
		 
		 (L0 () (= B B)                                           ("=ref" (B) ()))
		 ;;(L1 () (= B (plus (times k Asigma) l))                   ("cas" (pos the-cas) (L0)))
		 (L1 () (= B (plus (times k Asigma) l))                   ("solved" ()()))
		 
		 (L2 () (leq (absval B) (plus (absval (times k Asigma))
					      (absval l)))                ("triangle" () (L1)))
		 
		 (L3 () (leq (absval B) (plus (times (absval k)
						     (absval Asigma))
					      (absval l)))                ("abs-mult" (pos1) (L2)))
		 
		 (L4a () (leq (times (absval k)
				     (absval Asigma))
			      (times M
				     (absval Asigma)))                    ("solved" ()
									   (L01)))
		                                              ;;ist zu viel /statt <=mult-r L05
		 (L4 () (leq (plus (times (absval k)
					  (absval Asigma))
				   (absval l))
			     (plus (times M (absval Asigma))
				   (absval l)))                           ("<=add-r" ()
									   (L4a)))
		 (L5 () (leq (absval B)
			     (plus (times M (absval Asigma))
				   (absval l)))                           ("<=trans" () (L3 L4)))
		 (L6 () (leq (times M (absval Asigma))
			      (times M (div epsilon (times 2 M))))        ("<mult-l" ()
									   (L02 L04)))
		 (L7 () (leq (plus (times M (absval Asigma))
				    (absval l))
			      (plus (times M (div epsilon (times 2 M)))
				    (absval l)))                          ("<add-r" () (L6)))
		 (L8 () (leq (absval B)
			      (plus (times M (div epsilon (times 2 M)))
				    (absval l)))                          ("<=trans<" () (L5 L7)))
		 (L9 () (leq (plus (times M (div epsilon (times 2 M)))
				    (absval l))
			      (plus (times M (div epsilon (times 2 M)))
				    (div epsilon 2)))                     ("<add-l" () (L03)))
		 (L10 () (leq (absval B)
			      (plus (times M (div epsilon (times 2 M)))
				    (div epsilon 2)))                    ("<trans" () (L8 L9)))
		 
		 (Lthm () (leq (absval B) epsilon)                       ("solved" ()
										   (L10)));;statt "fixer-arith"
		 )
		
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method ComplexEstimate<-m-b is a complex estimation method
that tries to prove the goal <FONT COLOR=\"#0000ff\">lthm</FONT>, by rewriting the term <FONT COLOR=\"#00aa10\">B</FONT>
as a linear combination of the subterm <FONT COLOR=\"#00aa10\">A</FONT> of an assumption after the application
of the mystical substitution <FONT COLOR=\"#00aa10\">sigma</FONT> that is computed by the conditional 'getcassubst'. <BR>
The method is applicable, if MAPLE(tm) is able to compute the linear combination shown in line <FONT COLOR=\"#0000ff\">l1</FONT>.
Additionally, the linear combination must not be trivial (i.e., <FONT COLOR=\"#00aa10\">k</FONT> is 0 or
<FONT COLOR=\"#00aa10\">k</FONT> is 1 and  <FONT COLOR=\"#00aa10\">l</FONT> is 0). 
The application of this method produces four (simpler) inequalities as subgoals, involving an auxiliary variable
<FONT COLOR=\"#00aa10\">M</FONT>.
"))
		
		(remark 
("<U>ComplexEstimate<:</U><BR>
We need to estimate the magnitude of
<BR><TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>.<BR>
To do this, we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>This goal can be shown in three steps:
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists a real number <TERM>M</TERM> such that <TERM> (less (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L02\"> 2. </A> <TERM> (less (absval Asigma) (div epsilon (times 2 M))) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (less (absval l) (div epsilon 2))</TERM>,
</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR><A HREF=\"ltp:get-detailed-verbalization Lthm\">-> More details</A>"

"<U>ComplexEstimate<(detailed):</U><BR>
In order to estimate the magnitude of the complicated term
<BR><TERM>(less (absval B) epsilon)</TERM>, <BR>
we represent it as the linear combination
<BR><TERM>(absval (plus (times k Asigma) l))</TERM>.
<BR><BR>Now we use the Triangle Inequality and obtain<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>The magnitude of <BR><TERM>(plus (absval (times k Asigma)) (absval l))</TERM><BR>
can be estimated easily by the following three steps:<BR>
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists a real number <TERM>M</TERM> such that <TERM> (less (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L02\"> 2. </A> <TERM> (less (absval Asigma) (div epsilon (times 2 M))) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (less (absval l) (div epsilon 2))</TERM>,
</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>=</TERM>   
<TERM>(plus (absval (times k Asigma)) l))</TERM>
<BR>
<TAB><TERM>leq</TERM>   
<TERM>(plus (absval (times k Asigma)) (absval l)))</TERM>
<BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>,
<BR>and therefore
<TERM>(less (absval B) epsilon)</TERM>,<BR>
which was to be shown.
")
("
<LISP>(header)</LISP>
<TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>,<BR>
we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR><LISP>(get-complex-estimate-variants)</LISP>
<LISP>(increase-depth)</LISP><BR><BLOCKQUOTE>
<LISP>(item)</LISP> There exists a real number <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l01)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon (times 2 M)))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l02)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval l) (div epsilon 2))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l03)</LISP></BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
</BLOCKQUOTE>
<LISP>(get-reasoning-variants)</LISP><LISP>(get-conclusion)</LISP> <BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR>"
 "
<LISP>(header)</LISP>
<TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>,<BR>
we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR><LISP>(get-motivation)</LISP><LISP>(get-action-variants)</LISP>
<TERM>(less (plus (absval (times k Asigma)) (absval l)) epsilon)</TERM>,
<BR><LISP>(get-motivation2)</LISP>show that:<LISP>(increase-depth)</LISP><BR>
<BLOCKQUOTE>
<LISP>(item)</LISP> There exists a real number <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l01)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon (times 2 M)))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l02)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval l) (div epsilon 2))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l03)</LISP></BLOCKQUOTE>
</BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
<LISP>(get-restriction-variants)</LISP><BR>
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR>")
))


;;*******************************************************
;;*       The ComplexEstimate>-method                    *
;;*******************************************************
(infer~defmethod ComplexEstimate>-m
		 (outline-mappings (((existent
				      nonexistent
				      nonexistent
				      nonexistent
					;nonexistent
				      nonexistent)
				     ComplexEstimate>-m-b)))
		 (parameter-types ndline position)
		 )

(meth~defmethod ComplexEstimate>-m-b ComplexEstimate>-m
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (result o list)  (A num)       (Asigma num)   (B num)        (k num)  
		  (l num)          (lsigma num)  (epsilon num)  (sigma o sub)  (ksigma num)
		  (e1 num)   	   (M num const) (positions o pos)  (F o term)
		  (just-params o termlist) (assumption o prln) (position o pos)
		  ))
		
		(parameters assumption position)

		(premises (+ L01) (+ L02) (+ L03) (+ L04))
		
		(application-condition
		 (mand (bound-p assumption)
		       (mnot (mequal epsilon (:term 0)))
		       (mbind A (termatpos (prlnformula assumption)
					   (posappend position (:position (1 1)))))
		       
		       (getcassubst A B sigma)
		       (mbind Asigma (subst-apply sigma A))
		       (casextract Asigma B k l sigma)
		       
		       (mnot (mequal k (:term 0)))
		       (mnot (mand (mequal k (:term 1))
				   (mequal l (:term 0))))
		       (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		       (no-Eigenvars-in (:symbol CoSIE) M)
		       )
		 )
		
		(outline-computations
		 (just-params (mcons assumption (mlist position)))
		 )
		
		(expansion-computations
		 (pos (:position (2)))
		 (the-cas (:symbol mycas)))
		
		(conclusions (- Lthm))
		(decl-content
		 ;(L00 () (less (absval A) e1))
		 (L01 () (less (absval k) M)                                ("OPEN" () ()))
		 (L02 () (less (absval Asigma) (div epsilon M))             ("OPEN" () ()))
		 (L03 () (greater (absval l) (times epsilon 2))             ("OPEN" () ()))
		 (L04 () (less 0 M)                                         ("OPEN" () ()))

		 (Lthm () (greater (absval B) epsilon)                      ("solved" ()
									     (L00 L01 L02 L03 L04)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method ComplexEstimate>-m-b is a complex estimation method
that tries to prove the goal <FONT COLOR=\"#0000ff\">lthm</FONT>, by rewriting the term <FONT COLOR=\"#00aa10\">B</FONT>
as a linear combination of the subterm <FONT COLOR=\"#00aa10\">A</FONT> of an assumption after the application
of the mystical substitution <FONT COLOR=\"#00aa10\">sigma</FONT> that is computed by the conditional 'getcassubst'. <BR>
The method is applicable, if MAPLE(tm) is able to compute the linear combination shown in line <FONT COLOR=\"#0000ff\">l1</FONT>
with a factor <FONT COLOR=\"#00aa10\">k</FONT> that is not 0. <BR>
The application of this method produces four (simpler) inequalities as subgoals, involving an auxiliary variable
<FONT COLOR=\"#00aa10\">M</FONT>.
"))
		
		(remark
		 ("<U>ComplexEstimate>:</U><BR>
We need to estimate the magnitude of
<BR><TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>.<BR>
<BR><BR>This goal can be shown in three steps:
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists a real number <TERM>M</TERM> such that <TERM> (less (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L02\"> 2. </A> <TERM> (less (absval Asigma) (div epsilon M)) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (greater (absval l) (times epsilon 2))</TERM>,
</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>greater</TERM>     
    <TERM>(plus(times M (div epsilon M))(times epsilon 2))</TERM>
<BR>
<TAB><TERM>greater</TERM>    
    <TERM>epsilon</TERM>.
<BR>")
("
<LISP>(header)</LISP>
<TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>.
<BR><LISP>(get-complex-estimate-variants)</LISP><LISP>(increase-depth)</LISP><BR>
<BLOCKQUOTE>
<LISP>(item)</LISP> There exists a real number <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l01)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon M))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l02)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(greater (absval l) (times epsilon 2))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l03)</LISP></BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
</BLOCKQUOTE>
<LISP>(get-reasoning-variants)</LISP><LISP>(get-conclusion)</LISP> <BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>greater</TERM>     
    <TERM>(plus(times M (div epsilon M))(times epsilon 2))</TERM>
<BR>
<TAB><TERM>greater</TERM>    
    <TERM>epsilon</TERM>.
<BR>"
"
<LISP>(header)</LISP>
<TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>.
<BR><LISP>(get-complex-estimate-variants)</LISP><LISP>(increase-depth)</LISP><BR>
<BLOCKQUOTE>
<LISP>(item)</LISP> There exists a real number <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l01)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon M))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l02)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(greater (absval l) (times epsilon 2))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l03)</LISP></BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
</BLOCKQUOTE>
<LISP>(get-reasoning-variants)</LISP><LISP>(get-conclusion)</LISP> <BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>greater</TERM>     
    <TERM>(plus(times M (div epsilon M))(times epsilon 2))</TERM>
<BR>
<TAB><TERM>greater</TERM>    
    <TERM>epsilon</TERM>.
<BR>"
)		 

))


;;*******************************************************
;;*       The ComplexEstimate>=-method                    *
;;*******************************************************

(infer~defmethod ComplexEstimate>=-m
		 (outline-mappings (((existent
				      nonexistent
				      nonexistent
				      nonexistent
					;nonexistent
				      nonexistent)
				     ComplexEstimate>-m-b)))
		 (parameter-types ndline position)
		 )

(meth~defmethod ComplexEstimate>=-m-b ComplexEstimate>-m
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (result o list)  (A num)       (Asigma num)   (B num)        (k num)  
		  (l num)          (lsigma num)  (epsilon num)  (sigma o sub)  (ksigma num)
		  (e1 num)   	   (M num const) (positions o pos)  (F o term)
		  (just-params o termlist) (assumption o prln) (position o pos)
		  ))
		
		(parameters assumption position)

		(premises (+ L01) (+ L02) (+ L03) (+ L04))
		
		(application-condition
		 (mand (bound-p assumption)
		       (mnot (mequal epsilon (:term 0)))
		       (mbind A (termatpos (prlnformula assumption)
					   (posappend position (:position (1 1)))))
		       
		       (getcassubst A B sigma)
		       (mbind Asigma (subst-apply sigma A))
		       (casextract Asigma B k l sigma)
		       
		       (mnot (mequal k (:term 0)))
		       (mnot (mand (mequal k (:term 1))
				   (mequal l (:term 0))))
		       (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		       (no-Eigenvars-in (:symbol CoSIE) M)
		       )
		 )
		
		(outline-computations
		 (just-params (mcons assumption (mlist position)))
		 )
		
		(expansion-computations
		 (pos (:position (2)))
		 (the-cas (:symbol mycas)))
		
		(conclusions (- Lthm))
		(decl-content
		 ;(L00 () (less (absval A) e1))
		 (L01 () (less (absval k) M)                                ("OPEN" () ()))
		 (L02 () (less (absval Asigma) (div epsilon M))             ("OPEN" () ()))
		 (L03 () (greater (absval l) (times epsilon 2))             ("OPEN" () ()))
		 (L04 () (less 0 M)                                         ("OPEN" () ()))

		 (Lthm () (geq (absval B) epsilon)                      ("solved" ()
									     (L00 L01 L02 L03 L04)))
		 )
		(manual (author "MP, JZ")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark
		 ("")))


#|(infer~defmethod RDL-m
		 (outline-mappings (((existent nonexistent) RDL-m-b)
				    ))
		 (help "Try to implify to true with the RDL system."))

(meth~defmethod RDL-m-b RDL-m 
		(in limit)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (rel (o num num) term)
		  (A num term)
		  (B num term)
		  (R num term)
		  )
		 )
		
		(premises (+ L1))
		(application-condition
		 (mand (mor
			(mequal (:term less) rel)
			(mequal (:term leq) rel)
			(mequal (:term =) rel)
			(mequal (:term greater) rel))
		       (rdl-simplify L2 R)))
		
		(conclusions (- l2))
		(decl-content
		 (L1 () R                 ("OPEN" () ()))
		 (l2 () (rel A B)         ("solved" () (L1)))
		 )
		
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method RDL-m-b tries to simplify a formula to true using the RDL system which is based on constraint contextual rewriting (CCR). CCR is proved to be correct, but we cannot tranform RDL proof into ND-calculus at the moment."))
		
		(remark ("<U>Simplify:</U><BR>
We simplify <TERM>(rel A B)</TERM> to <TERM>true</TERM> using the RDL system.")
			)
		)



(infer~defmethod Factorize-m
		 (outline-mappings (((existent nonexistent) Factorize-m-b)
				    ))
		 (help "Try to factorize a polynomial."))

(meth~defmethod Factorize-m-b Factorize-m 
		(in limit)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (rel (o num num) term)
		  (A num term)
		  (B num term)
		  (R num term)
		  )
		 )
		
		(premises (+ L1))
		(application-condition
                 (mand (mor
                        (mequal (:term less) rel)
                        (mequal (:term leq) rel)
                        (mequal (:term =) rel)
                        (mequal (:term greater) rel))
                       (factorize A R))
                 )
                 
		(conclusions (- l2))
		(decl-content
		 (L1 () (rel R B)      ("OPEN" () ()))
		 (l2 () (rel A B))     ("solved" () (L1))
                 )
                
                (manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation ""))
		(remark ("<U>Factorize:</U><BR>
We factorize the polynomial <TERM>A</TERM> and get <TERM>(rel R B)</TERM>.")
			)
		)

|#
(infer~defmethod Simplify-m
		 (outline-mappings (((existent nonexistent) Simplify-m-b)
				    ((nonexistent closed) Simplify-m-f)
				    ))
		 (help "Simplify with Maple."))

(meth~defmethod Simplify-m-b Simplify-m 
		(in limit)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (rel (o num num) term)
		  (A num term)
		  (B num term)
		  (C num term)
		  (Ch num term)
		  (D num term)
		  (Dh num term)
		  )
		 )
		
		(premises (+ L1))
		(application-condition
		 (mand (mor
			(mequal (:term less) rel)
			(mequal (:term leq) rel)
			(mequal (:term =) rel)
			(mequal (:term greater) rel))
		       (mor (mand (cas-simplify A C)
				  (mbind D B)
				  (mbind position (:position (1))))
			    (mand (cas-simplify B D)
				  (mbind C A)
				  (mbind position (:position (2)))))))

		 
		(expansion-computations
		 (the-cas (:symbol mcas)))

		(conclusions (- l2))
		(decl-content
		 (L1 () (rel C D)                       ("OPEN" () ()))
		 (l2 () (rel A B)                       ("solved" (position the-cas) (L1)))
		 )
		
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method Simplify-m-b tries to simplify a term
<FONT COLOR=\"#00aa10\">B</FONT> on the left hand side of the inequality or an equation in the conclusion
<FONT COLOR=\"#0000ff\">l2</FONT>. It uses the term simplication algorithm of MAPLE(tm). <BR>
The method ist applicable, if the resulting term <FONT COLOR=\"#00aa10\">C</FONT> differs
from <FONT COLOR=\"#00aa10\">B</FONT>.<BR> The application of Simplify-m-b introduces
<FONT COLOR=\"#0000ff\">l1</FONT> as a new subgoal.
"))
		
		(remark ("<U>Simplify:</U><BR>
We simplify <TERM>(rel A B)</TERM> to <TERM>(rel C D)</TERM>.")
			("We simplify <TERM>(rel A B)</TERM> to <TERM>(rel C D)</TERM>.
<BR><LISP>(verbalize-text-next l1)</LISP>"
			 "We simplify <TERM>(rel A B)</TERM> to <TERM>(rel C D)</TERM>.
<BR><LISP>(verbalize-cons-next l1)</LISP>"
			 ))
		)


(meth~defmethod Simplify-m-f Simplify-m 
		(in limit)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (rel (o num num) term)
		  (A num term)
		  (B num term)
		  (C num term)
		  (Ch num term)
		  (D num term)
		  (Dh num term)
		  )
		 )
		
		(premises (- L1))
		(application-condition
		 (mand (mor
			(mequal (:term less) rel)
			(mequal (:term leq) rel)
			(mequal (:term =) rel)
			(mequal (:term greater) rel))
		       (mor (mand (cas-simplify A C)
				  (mbind D B)
				  (mbind position (:position (1))))
			    (mand (cas-simplify B D)
				  (mbind C A)
				  (mbind position (:position (2)))))))

		 
		(expansion-computations
		 (the-cas (:symbol mcas)))

		(conclusions (+ l2))
		(decl-content
		 (L1 () (rel A B)                       )
		 (l2 () (rel C D)                       ("solved" (position the-cas) (L1)))
		 )
		
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method Simplify-m-b tries to simplify a term
<FONT COLOR=\"#00aa10\">B</FONT> on the left hand side of the inequality or equation in the conclusion
<FONT COLOR=\"#0000ff\">l2</FONT>. It uses the term simplication algorithm of MAPLE(tm). <BR>
The method ist applicable, if the resulting term <FONT COLOR=\"#00aa10\">C</FONT> differs
from <FONT COLOR=\"#00aa10\">B</FONT>.<BR> The application of Simplify-m-b introduces
<FONT COLOR=\"#0000ff\">l1</FONT> as a new subgoal.
"))
		
		(remark ("<U>Simplify:</U><BR>
We simplify <TERM>(rel A B)</TERM> to <TERM>(rel C D)</TERM>.")
			("We simplify <TERM>(rel A B)</TERM> to <TERM>(rel C D)</TERM>.
<BR><LISP>(verbalize-text-next l1)</LISP>"
			 "We simplify <TERM>(rel A B)</TERM> to <TERM>(rel C D)</TERM>.
<BR><LISP>(verbalize-cons-next l1)</LISP>"
			 ))
		)

(infer~defmethod SimplifyInequality-m
		 (outline-mappings (((existent nonexistent) SimplifyInequality-m-b)
				    ((nonexistent closed) SimplifyInequality-m-f)
				    ))
		 (help "Simplifies an inequality."))

(meth~defmethod SimplifyInequality-m-b SimplifyInequality-m 
		(in limit)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (prems o prlnlist)
		  (subgoals o termlist)
		  (phi o term)
		  (psi o term)
		  (Eigenvars o termlist)
		  (occs o poslist)
		  )
		 )
		
		(premises (+ L1) (+ prems))
		(application-condition
		 (mand (mnot (symboloccs (:symbol and) phi occs))
		       (mnot (symboloccs (:symbol or) phi occs))
		       (mnot (symboloccs (:symbol forall) phi occs))
		       (mnot (symboloccs (:symbol exists) phi occs))
		       (mbind Eigenvars (cs-EigenVars (:symbol CoSIE)))
		       (Simplify-Inequality phi Eigenvars psi subgoals)
		       (mif (bound-p subgoals)
			    (mif (mequal subgoals (mnil))
				 (mtrue)
				 (mbind prems (premises-for l2 subgoals)))
			    (mtrue))
		       )
		 )
		
		(conclusions (- l2))
		(decl-content
		 (L1 () psi                             ("OPEN" () ()))
		 (l2 () phi                             ("solved" () (L1)))
		 )
		
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method SimplifyInequality-m-b tries to Simplify an inequality(equation),
i.e. to transform it in a normal form with the usual rules of computions for inequlities(equations).
<BR> The application of SimplifyInequality-m-b introduces <FONT COLOR=\"#0000ff\">l1</FONT> as a new subgoal.
"))
		
		(remark ("<U>SimplifyInequality:</U><BR> We simplify <TERM>phi</TERM> to <TERM>psi</TERM>.")
			("We simplify <TERM>phi</TERM> to <TERM>psi</TERM>.<BR><LISP>(verbalize-text-next l1)</LISP>"
			 "We simplify <TERM>phi</TERM> to <TERM>psi</TERM>.<BR><LISP>(verbalize-cons-next l1)</LISP>"
			 ))
		)

(meth~defmethod SimplifyInequality-m-f SimplifyInequality-m 
		(in limit)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (prems o prlnlist)
		  (subgoals o termlist)
		  (phi o term)
		  (psi o term)
		  (Eigenvars o termlist)
		  )
		 )
		
		(premises (- L1) (+ prems))
		(application-condition
		 (mand (mnot (symboloccs (:symbol and) psi occs))
		       (mnot (symboloccs (:symbol or) psi occs))
		       (mnot (symboloccs (:symbol forall) psi occs))
		       (mnot (symboloccs (:symbol exists) psi occs))
		       (mbind Eigenvars (cs-EigenVars (:symbol CoSIE)))
		       (Simplify-Inequality psi Eigenvars phi subgoals)
		       (mif (bound-p subgoals)
			    (mif (mequal subgoals (mnil))
				 (mtrue)
				 (mbind prems (premises-for l1 subgoals)))
			    (mtrue))
		       )
		 )
		
		(conclusions (+ l2))
		(decl-content
		 (L1 () psi                             )
		 (l2 () phi                             ("solved" () (L1)))
		 )
		
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method SimplifyInequality-m-f tries to Simplify an inequality(equation),
i.e. to transform it in a normal form with the usual rules of computions for inequlities(equations).
<BR> The application of SimplifyInequality-m-f introduces <FONT COLOR=\"#0000ff\">l1</FONT> as a new assumption.
"))
		
		(remark ("<U>SimplifyInequality:</U><BR> We simplify <TERM>phi</TERM> to <TERM>psi</TERM>.")
			("We simplify <TERM>phi</TERM> to <TERM>psi</TERM>.<BR><LISP>(verbalize-text-next l1)</LISP>"
			 "We simplify <TERM>phi</TERM> to <TERM>psi</TERM>.<BR><LISP>(verbalize-cons-next l1)</LISP>"
			 ))
		)


(infer~defmethod FactorialEstimate-m
		 (outline-mappings (((existent nonexistent nonexistent nonexistent) FactorialEstimate-m-b)
				    ))
		 (help "Handles Factorials."))

(meth~defmethod FactorialEstimate-m-b FactorialEstimate-m 
		(in limit)
		(rating 39)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (meta o metavar)
		  (M num const)
		  (C num term)
		  (e num term)
		  (Numer num term)
		  (Denom num term)
		  (F num term)
		  )
		 )
		
		(premises (+ L1) (+ L2) (+ l3))
		
		(application-condition
		 ;;(mand
		 (mbind M (newmetavar (:symbol :F) (termtype e)))
		 ;;(no-Eigenvars-in (:symbol CoSIE) M))  ;; ZU RESTRIKTIV z.B. FUER LIM-EXC3.1.10 !!!
		 )
		(conclusions (- l4))

		(decl-content
		 (L1 () (less 0 M)                                ("OPEN" () ()))
		 (L2 () (greater (absval Denom) M)                ("OPEN" () ()))
		 (L3 () (less (absval Numer) (times e M))         ("OPEN" () ()))
		 (l4 () (less (absval (div Numer Denom)) e)       ("solved" () ()))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "This method introduces an auxiliary variable
<FONT COLOR=\"#00aa10\">M</FONT> to split the fraction in the conclusion
<FONT COLOR=\"#0000ff\">l4</FONT> into two inequalities without fractions. These inequalities are
introduced in the planning state as the new subgoals <FONT COLOR=\"#0000ff\">l2</FONT> and
<FONT COLOR=\"#0000ff\">l3</FONT>. Additionally, the constraint  M&gt 0 must be fulfilled and is given
as a subgoal <FONT COLOR=\"#0000ff\">l1</FONT>. <BR>
The method has no application conditions. 
  "))
		(remark ("In order to estimate
<TERM>(less (absval (div Numer Denom)) e)</TERM>,<BR>
we estimate the numerator to be smaller than <TERM>(times e M)</TERM><BR>
and the denominator to be bigger than <TERM>M</TERM>:
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization l3\"> 1. </A> <TERM>(less (absval Numer) (times e M))</TERM><BR>
<A HREF=\"ltp:get-local-verbalization l2\"> 2. </A><TERM>(greater (absval Denom) M)</TERM><BR>
</BLOCKQUOTE>
Then we know that the factorial is smaller than<BR>
<TERM>(=(div (times e M)M) e)</TERM>.
")
	("<LISP>(get-motivation)</LISP>estimate
<TERM>(less (absval (div Numer Denom)) e)</TERM>,<BR>
we estimate the numerator to be smaller than <TERM>(times e M)</TERM><BR>
and the denominator to be bigger than <TERM>M</TERM>:
<LISP>(increase-depth)</LISP><BLOCKQUOTE>
<LISP>(item)</LISP> <TERM>(less (absval Numer) (times e M))</TERM>
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l3)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(greater (absval Denom) M)</TERM>
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l2)</LISP></BLOCKQUOTE><BR>
</BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
<LISP>(get-reasoning-variants)</LISP><LISP>(get-conclusion)</LISP> that the factorial is smaller than<BR>
<TERM>(=(div (times e M)M) e)</TERM>.
<BR>
"
	 "<LISP>(get-motivation)</LISP>estimate
<TERM>(less (absval (div Numer Denom)) e)</TERM>,<BR>
we estimate the numerator to be smaller than <TERM>(times e M)</TERM><BR>
and the denominator to be bigger than <TERM>M</TERM>:
<BLOCKQUOTE>
<LISP>(increase-depth)</LISP>
<LISP>(item)</LISP> <TERM>(less (absval Numer) (times e M))</TERM>
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l3)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(greater (absval Denom) M)</TERM>
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l2)</LISP></BLOCKQUOTE><BR>
</BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
<LISP>(get-reasoning-variants)</LISP><LISP>(get-conclusion)</LISP> that the factorial is smaller than<BR>
<TERM>(=(div (times e M)M) e)</TERM>.
<BR>
")		
	)
		)

;;*******************************************************
;;*            The UWRAPHYP-S-F supermethod.            *
;;*    unwraps positive subformulas of assumptions.     *
;;*******************************************************
(infer~defsupermethod UNWRAPHYP-S
		      (supermethod UNWRAPHYP-S-F)
		      (help "The UNWRAPHYP inference."))


(meth~defsupermethod UNWRAPHYP-S-F
		     UNWRAPHYP-S
		     (in limit)
		     (rating 0)
		     (reasoning :planning :middle-out)
		     (declarations
		      (sorted-meta-variables
		       (psi o term)
		       (phi o term)
		       (goal o prln)
		       
		       (result o planlist)
		       (prec o prlnlist)
		       (prems o prlnlist)
		       (concls o prlnlist)
		       (hyps o prlnlist)
		       )
		      
		      )
		     
		     (premises (- L1) (+ prems)) 
		     (conclusions (+ hyps) (+ concls))
		     
		     (application-condition
		      (mand (meval result)
			    )
		      )
		     (outline-computations
		      (prec (msecond result))
		      (hyps (mthird result))
		      (prems (mfourth result))
		      (concls (mfifth result))
		      )
		     (expansion-function (plan~expand-supermethod (first concls) result))
		     (decl-content
		      (L1 () psi)
		      )
		     (proc-content
		      (plan~iterate-planner '()
					    L1
					    '(Focus-m-f
					      AndE-m-f
					      IncreaseHyp-m-f
					      ImpE-m-rd
					      RemoveFocus-m-f
					      )
					    '(Choose-focus
					      Increase-hyps
					      Attack-latest
					      )
					    )
		      )
		     (manual (author "Juergen Zimmer")
			     (examples "Limit theorems")
			     (documentation "The supermethod UNWRAPHYP-S-F plans only with the methods
<A HREF=\"./focus-m-f.lml\">Focus-m-f</A>, <A HREF=\"./increasehyp-m-f.lml\">IncreaseHyp-m-f</A>,
<A HREF=\"./ande-m-f.lml\">AndE-m-f</A>, <A HREF=\"./impe-m-rd.lml\">ImpE-m-rd</A>, and
<A HREF=\"./removefocus-m-f.lml\">RemoveFocus-m-f</A>. <BR>
The planning process is guided by special control knowledge encoded in the control rules Choose-focus,
Increase-hyps, and Attack-latest. <BR>
Please have a look at the documentation in the file limit-crules.thy for further information. <BR>
This supermethod is applicable, if at least one of the given
methods is applicable. <BR>
The method is expanded by a special expansion-function.
"))
		     
		     (remark
		      ("<U>Unwrap-Hypothesis:</U><BR>From <LISP>(verbalize-prec prec)</LISP><BR>
follows <LISP>(verbalize-concls concls)</LISP>,<BR>if <LISP>(verbalize-prems
prems)</LISP>.")
		      ("By hypothesis <LISP>(verbalize-hyp prec)</LISP>"
		       "By hypothesis <LISP>(verbalize-hyp prec)</LISP>
<LISP>(collect-restrictions prems)</LISP>")
		      );;"By hypothesis <LISP>(verbalize-hyp prec)</LISP><LISP>(verbalize-prems2 concls prems)</LISP>")
		     )


;;*******************************************************
;;*       The Focus-methods, Increasehyp and ImpE       *
;;*          for the UWRAPHYP supermethod.              *
;;*******************************************************
(infer~defmethod Focus-m
		 (outline-mappings (((nonexistent closed) Focus-m-f)))
		 (parameter-types position)
		 (help "The focus-m inference."))

(meth~defmethod Focus-m-f Focus-m  
		(in limit)
		(rating 1)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (phi o term)
		  (annotated o term)
		  (focus (o o) term)
		  (chosen o term)
		  (focused o term)
		  (psi o term)
		  (focus-position o pos)
		  (focus-position-list o list)
		  )		 
		 )
		(parameters focus-position)
		(premises (- L1))
		
		(application-condition
		 (mand
		  (bound-p focus-position)
		  (mnot (mequal focus-position (mnil)))
		  (mnot (mequal focus-position (emptypos))))
		 )

		(outline-computations
		 (focus-position-list (mlist focus-position))
		 (chosen (termatpos phi focus-position))
		 (focused (appl-create (:term focus) (mlist chosen)))
		 (psi (termrploccs phi chosen focused))
		 )
		(conclusions (+ L2))
		(decl-content
		 (L1 () phi)
		 (L2 () psi          ("Focus" (focus-position) (L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method Focus-m-f focusses on a certain subformula of its
premise <FONT COLOR=\"#0000ff\">l1</FONT>. <BR>
This is done by literally inserting the symbol <FONT COLOR=\"#00dd10\">focus</FONT> at
the <FONT COLOR=\"#00dd10\">focus-position</FONT> given as a parameter to the method. <BR>
The method is applicable, if the given position is not NIL and not the empty position (because it
make no sense to focus on the whole formula PHI). <BR>
This method is currently used in the <A HREF=\"./unwraphyp-s-f.lml\">UNWRAPHYP-S-F</A> supermethod,
which unwraps a subformula from an assumption."))
		
		(remark "");<BR> Focus on <TERM>chosen</TERM>.<BR>)
		)


(infer~defmethod IncreaseHyp-m
		 (outline-mappings (((nonexistent closed) IncreaseHyp-m-f)))
		 (parameter-types ndline-list)
		 )


(meth~defmethod IncreaseHyp-m-f	IncreaseHyp-m
		(in limit)
		(rating 1)
		(reasoning :planning)
		(declarations
		 (sorted-meta-variables
		  (F o term)
		  (add-hyps o prlnlist)
		  (add-hyps-list o list)
		  (positions o poslist)
		  )
		 
		 )
		(parameters add-hyps)
		
		(premises (- l1)
			  )
		(application-condition
		 (mand (bound-p add-hyps)
		       (mnot (mequal add-hyps (mnil)))
		       (symboloccs (:symbol focus) F positions))
		 )
		(outline-computations
		 (add-hyps-list (mlist add-hyps)))
		
		(conclusions (+ l2)
			     )
		(decl-content
		 (l1 ()         F)
		 (l2 (add-hyps) F  ("weaken" () (l1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method IncreaseHyp-m-f adds the hypotheses given as a
parameter <FONT COLOR=\"#00dd10\">add-hyps</FONT> to the set of hypotheses of the premise 
<FONT COLOR=\"#0000ff\">l1</FONT>. <BR>
The new assumption is added as a new conclusion <FONT COLOR=\"#0000ff\">l2</FONT>. <BR>
Due to the use in the supermethod <A HREF=\"./unwraphyp-s-f.lml\">UNWRAPHYP-S-F</A>, this method
is only applicable, if the <FONT COLOR=\"#00dd10\">add-hyps</FONT> are not NIL and if the
formula F has already been focused by the <A HREF=\"./focus-m-f.lml\">Focus-m-f</A> method. <BR>

"))
		
		(remark "")
		)

(infer~defmethod RemoveFocus-m
		 (outline-mappings (((nonexistent closed) RemoveFocus-m-f)
				    ((existent existent) RemoveFocus-m-f) ;; helpful.........
				    )))

(meth~defmethod RemoveFocus-m-f RemoveFocus-m 
		(in limit)
		(rating 1)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (ass o term)
		  (annotated o term)
		  (focus (o o) term)
		  (chosen o term)
		  (positions o poslist)
		  (focused o term)
		  (annotated o term)
		  (focus-position o pos)
		  )
		 )
		(premises (- L1))
		
		(conclusions (+ L2))
		(decl-content
		 (L1 () (focus ass))
		 (L2 () ass         ("RemoveFocus" () (L1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "The method RemoveFocus-m-f removes the annotation
<FONT COLOR=\"#00aa10\">focus</FONT> from the top position of the formula of the premise
<FONT COLOR=\"#0000ff\">l1</FONT>. <BR>
This method is applied in the <A HREF=\"./unwraphyp-s-f.lml\">UNWRAPHYP-S-F</A> supermethod
after the subformula <FONT COLOR=\"#00aa10\">ass</FONT> is totally unwrapped.
"))
		
		(remark "");<BR>Annotation is removed from <TERM>annotated</TERM>.<BR>)
		)




;;*******************************************************
;;*       The EnvEstimate methods eliminate absolute   *
;;*        values using the definition of 'absval'.     *
;;*******************************************************
(infer~defmethod EnvI<-m 
		 (outline-mappings (((existent nonexistent nonexistent) EnvI<-m-b)
				    )))

#|(meth~defmethod EnvI<-m-b
		EnvI<-m
		(in limit)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (a num term)
		  (c num term)
		  (b num term)
		  (lhs num term)
		  (rhs num term)
		  (term1 num term)
		  (term2 num term)
		  (dummy (num num num))
		  (epsilon num term)  (sigma o sub)
		  ))

		
		(premises (+ L10) (+ L20))
		(conclusions (- L30))

		(application-condition
		 (mif (mand (appl-p c)
       			    (mequal (applfunc c) (:term minus))
			    )
		      (mand (mbind a (mfirst (applargs c)))
			    (mbind lhs (appl-create (:term minus) (mlist (msecond (applargs c)) epsilon)))
			    (mbind rhs (appl-create (:term plus) (mlist (msecond (applargs c)) epsilon))))
		      (mand (mbind a c)
			    (mbind lhs (appl-create (:term minus) (mlist (:term 0) epsilon)))
			    (mbind rhs epsilon))))
		
		(outline-computations
		 )
		      
		(decl-content
		 (L10 ()    (less lhs a)                                 ("open" () ())) 
		 (L20 ()    (less a rhs)                                 ("open" () ()))
		 (L30 ()    (less (absval c) epsilon)                    ("solved" () ()))
		 )

		(remark ("<U>EnvI<-b</U><BR>
In order to prove <TERM>(less (absval c) epsilon)</TERM> we decompose the environment by
eliminating the absolute value and get the two subgoals:
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L10\"> 1. </A> <TERM>(less lhs a)</TERM>, and<BR>
<A HREF=\"ltp:get-local-verbalization L20\"> 2. </A> <TERM>(less a rhs)</TERM>.<BR>
</BLOCKQUOTE>")
			("In order to prove <TERM>(less (absval c) epsilon)</TERM> we decompose the environment by
eliminating the absolute value and prove the two subgoals:
<LISP>(increase-depth)</LISP><BR><BLOCKQUOTE>
<LISP>(item)</LISP> <TERM>(less lhs a)</TERM>
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l10)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less a rhs)</TERM>
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l20)</LISP></BLOCKQUOTE><BR>
<LISP>(decrease-depth)</LISP>
</BLOCKQUOTE>"
			 "In order to prove <TERM>(less (absval c) epsilon)</TERM> we decompose the environment by
eliminating the absolute value and prove the two subgoals:
<LISP>(increase-depth)</LISP><BR><BLOCKQUOTE>
<LISP>(item)</LISP> <TERM>(less lhs a)</TERM>
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l10)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less a rhs)</TERM>
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l20)</LISP></BLOCKQUOTE><BR>
<LISP>(decrease-depth)</LISP>
</BLOCKQUOTE>"
			 ))
		(manual (author "MP EM")
			(examples "Limit theorems")
			(documentation "")))|#



(meth~defmethod EnvI<-m-b
		EnvI<-m
		(in limit)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (a num term)
		  (c num term)
		  (b num term)
		  (lhs num term)
		  (rhs num term)
		  (term1 num term)
		  (term2 num term)
		  (dummy (num num num))
		  (epsilon num term)  (sigma o sub)
		  (rel1 (o num num) term)
		  (rel2 (o num num) term)
		  ))

		
		(premises (+ L10) (+ L20))
		(conclusions (- L30))

		(application-condition
		 (mor (mand (mequal rel1 (:term less))
			    (mbind rel2 (:term greater)))
		      (mand (mequal rel1 (:term leq))
			    (mbind rel2 (:term geq)))))
		
		(decl-content
		 (L10 ()    (rel1 c epsilon)                                      ("open" () ())) 
		 (L20 ()    (rel2 c (minus 0 epsilon))                            ("open" () ()))
		 (L30 ()    (rel1 (absval c) epsilon)                             ("solved" () ()))
		 )
		)




(infer~defmethod EnvE<-m 
		 (outline-mappings (((nonexistent nonexistent existent) EnvE<-m-f)
				    )))

#|(meth~defmethod EnvE<-m-f
                EnvE<-m
                (in limit)
                (rating 60)
                (reasoning :planning :middle-out)
                (declarations
                 (sorted-meta-variables
                  (a num term)
                  (c num term)
                  (b num term)
                  (lhs num term)
                  (rhs num term)
                  (term1 num term)
                  (term2 num term)
                  (dummy (num num num))
                  (epsilon num term)  (sigma o sub)
                  ))

                
                (premises (- L10))
                (conclusions (+ L20)(+ L30))
		
		(application-condition
		 (mif (mand (appl-p c)
       			    (mequal (applfunc c) (:term minus))
			    )
		      (mand (mbind a (mfirst (applargs c)))
			    (mbind lhs (appl-create (:term minus) (mlist (msecond (applargs c)) epsilon)))
			    (mbind rhs (appl-create (:term plus) (mlist (msecond (applargs c)) epsilon))))
		      (mand (mbind a c)
			    (mbind lhs (appl-create (:term minus) (mlist (:term 0) epsilon)))
			    (mbind rhs epsilon))))
		
		(decl-content
		 (L10 ()    (less (absval c) epsilon)                    )
		 (L20 ()    (greater a lhs)                                 ("EnvE<-m" () (l10))) 
		 (L30 ()    (less a rhs)                                 ("EnvE<-m" () (l10)))
		 )
                (remark ("<U>EnvE<-f</U><BR>
We obtain <TERM>(greater a lhs)</TERM> and <TERM>(less a rhs)</TERM>
by decomposing the environement <A HREF=\"ltp:get-local-verbalization L10\">
<TERM>(less (absval c) epsilon)</TERM></A>.")
			("We obtain <TERM>(greater a lhs)</TERM> and <TERM>(less a rhs)</TERM>
by decomposing the environement <TERM>(less (absval c) epsilon)</TERM>.<BR>
<LISP>(verbalize-text-next l10)</LISP>"
			 "We obtain <TERM>(greater a lhs)</TERM>and <TERM>(less a rhs)</TERM>
by decomposing the environment <TERM>(less (absval c) epsilon)</TERM>.<BR>
<LISP>(verbalize-cons-next l10)</LISP>"))
                (manual (author "MP EM")
                        (examples "Limit theorems")
                        (documentation "")))|#


(meth~defmethod EnvE<-m-f
                EnvE<-m
                (in limit)
                (rating 60)
                (reasoning :planning :middle-out)
                (declarations
                 (sorted-meta-variables
                  (a num term)
                  (c num term)
                  (b num term)
                  (lhs num term)
                  (rhs num term)
                  (term1 num term)
                  (term2 num term)
                  (dummy (num num num))
                  (epsilon num term)  (sigma o sub)
		  (rel1 (o num num) term)
		  (rel2 (o num num) term)
                  ))
		                
                (premises (- L10))
                (conclusions (+ L20)(+ L30))

		(application-condition
		 (mor (mand (mequal rel1 (:term less))
			    (mbind rel2 (:term greater)))
		      (mand (mequal rel1 (:term leq))
			    (mbind rel2 (:term geq)))))
				
		(decl-content
		 (L10 ()    (rel1 (absval c) epsilon)                    )
		 (L20 ()    (rel1 c epsilon)                             ("EnvE<-m" () (l10))) 
		 (L30 ()    (rel2 c (minus 0 epsilon))                   ("EnvE<-m" () (l10)))
		 )
		)



(infer~defmethod EnvI>-m 
		 (outline-mappings (((existent nonexistent) EnvI>-m-b)
				    )))

#|(meth~defmethod EnvI>-m-b
		EnvI>-m
		(in limit)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (rel1 (o num num) term)
		  (rel2 (o num num) term)
		  (a num term)
		  (c num term)
		  (b num term)
		  (lhs num term)
		  (rhs num term)
		  (term1 num term)
		  (term2 num term)
		  (dummy (num num num))
		  (epsilon num term)  (sigma o sub)
		  ))

		
		(premises (+ L10))
		(conclusions (- L30))
		
		(application-condition
		 (mand (mor (mand (mequal rel1 (:term greater))
				  (mbind rel2 (:term less)))
			    (mand (mequal rel1 (:term geq))
				  (mbind rel2 (:term leq))))
		       (mif (mand
			     (appl-p c)
			     (mequal (applfunc c) (:term minus))
			    )
		      (mand (mbind a (mfirst (applargs c)))
			    (mbind lhs (appl-create (:term plus) (mlist (msecond (applargs c)) epsilon)))
			    (mbind rhs (appl-create (:term minus) (mlist (msecond (applargs c)) epsilon))))
		      (mand (mbind a c)
			    (mbind lhs epsilon)
			    (mbind rhs (appl-create (:term minus) (mlist (:term 0) epsilon)))))))
		
		(outline-computations
		 )
		
		(decl-content
		 (L10 ()    (or (rel1 a lhs) (rel2 a rhs))               ("open" () ())) 
		 (L30 ()    (rel1 (absval c) epsilon)                    ("solved" () ()))
		 )
				
		(remark "")
		(manual (author JM MP "Jzimmer")
			(examples "Limit theorems")
			(documentation "")))|#

(meth~defmethod EnvI>-m-b
		EnvI>-m
		(in limit)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (rel1 (o num num) term)
		  (rel2 (o num num) term)
		  (a num term)
		  (c num term)
		  (b num term)
		  (lhs num term)
		  (rhs num term)
		  (term1 num term)
		  (term2 num term)
		  (dummy (num num num))
		  (epsilon num term)  (sigma o sub)
		  ))

		
		(premises (+ L10))
		(conclusions (- L30))

		(application-condition
		 (mor (mand (mequal rel1 (:term greater))
			    (mbind rel2 (:term less)))
		      (mand (mequal rel1 (:term geq))
			    (mbind rel2 (:term leq)))))
		 
		(decl-content
		 (L10 ()    (or (rel1 c epsilon) (rel2 c (minus 0 epsilon)))             ("open" () ())) 
		 (L30 ()    (rel1 (absval c) epsilon)                                    ("solved" () (l10)))
		 )
		
		(remark "")
		(manual (author JM MP "Jzimmer")
			(examples "Limit theorems")
			(documentation "")))




(infer~defmethod EnvE>-m 
		 (outline-mappings (((nonexistent existent) EnvE>-m-f)
				    )))

(meth~defmethod EnvE>-m-f EnvE>-m
		(in limit)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (rel1 (o num num) term)
		  (rel2 (o num num) term)
		  (a num term)
		  (c num term)
		  (b num term)
		  (lhs num term)
		  (rhs num term)
		  (term1 num term)
		  (term2 num term)
		  (dummy (num num num))
		  (epsilon num term)  (sigma o sub)
		  ))

		
		(premises (- L10))
		(conclusions (+ L30))

		(application-condition
		 (mor (mand (mequal rel1 (:term greater))
			    (mbind rel2 (:term less)))
		      (mand (mequal rel1 (:term geq))
			    (mbind rel2 (:term leq)))))
		 
		(decl-content
		 (L10 ()    (rel1 (absval c) epsilon)                                    )
		 (L30 ()    (or (rel1 c epsilon) (rel2 c (minus 0 epsilon)))             ("solved" () (l10)))
		 )
		
		(remark "")
		(manual (author "Meier")
			(examples "Limit theorems")
			(documentation "")))




(infer~defmethod NotEqual-m
		 (outline-mappings (((existent nonexistent) NotEqual-m-b)))
		 (help " bla bla."))

(meth~defmethod NotEqual-m-b NotEqual-m
		(in limit)
		(rating 45)
		(reasoning :planning :middle-out)		
		(declarations
		 (sorted-meta-variables
		  (a num term)
		  (b num term)
		  )
		 )
		(premises (+ L1))

		(conclusions (- L2))
		
		(decl-content
		 (L1 () (or (greater a b) (less a b))      ("OPEN" () ()))
		 (L2 () (not (= a b))                      ("solved" () ()))
		 )
		
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation ""))
		
		(remark "")
		)


;;*******************************************************
;;*  Domainspecific Methods for quanitifier elimination *
;;*******************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forall
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(infer~defmethod "ForallI-m"
		 (outline-mappings (((existent nonexistent) Foralli-m-b)))
		 (help "The method for FORALL introduction"))

(meth~defmethod Foralli-m-b ForallI-m
		(in limit)
		(rating 15)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o)
		  (phi1 o)
		  (newc aa const)
		  (meta aa metavar)
		  ))
		(premises (+ l1))
		(conclusions (- l2))
		(application-condition
		 (mand (mbind newc (NewConstForVar x))
		       (mforall meta (metavars phi)
				(mand (not-subterm newc meta)
				      (Eigenvariable (:symbol CoSIE) newc meta))))
		 )
		(outline-computations
		 (phi1 (subst-apply (subst-create (mlist x) (mlist newc)) phi))
		 )
		(decl-content 
		 (l1 () phi1                          ("Open" () ()))
		 (l2 () (forall (lam (x aa var) phi)) ("ForallI" (newc) (l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Backward application of the tactic ForallI")
		)

(infer~defmethod ForallE-meta-m
		 (outline-mappings (((nonexistent existent) ForallE-meta-m-f)))
		 (parameter-types term)
		 (help "The method for FORALL elemination with meta-variables"))

(meth~defmethod ForallE-meta-m-f
		ForallE-meta-m
		(in limit)
		(rating 1)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables dd)
		 (sorted-meta-variables
		  (phi o term)
		  (phi1 o term)
		  (params o termlist)
		  (meta o metavar)
		  )
		 )
		
		(parameters meta)
		(premises (- l1))
		
		(outline-computations
		 (meta (NewMetaVarForVar x))
		 (params (mlist meta))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist meta))
				    phi))
		 )
		(conclusions (+ l2))
		(decl-content 
		 (l1 () (forall (lam (x dd var) phi)))         
		 (l2 () phi1                               ("ForallE" (params) (l1)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "Substantially, this method is a partial specification of the
<A HREF=\"../commands/foralle.lml\">ForallE</A> ND-rule. <BR>
It introduces a new meta-variable <FONT COLOR=\"#00aa10\">M_x</FONT> for the universally quantified variable
<FONT COLOR=\"#00aa10\">x</FONT>.
"))
		
		(remark "");ForallE introducing the term meta .)
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(infer~defmethod "ExistsI-m"
		 (outline-mappings (((existent nonexistent) Existsi-meta-m-b)))
		 (help "The method for EXISTS introduction"))

(meth~defmethod Existsi-meta-m-b ExistsI-m
		(in limit)
		(rating 15)
		(reasoning :middle-out :planning)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o)
		  (phi1 o)
		  (mv-x aa metavar)
		  )
		 )
		(premises (+ l1))
		(conclusions (- l2))
		(outline-computations
		 (mv-x (NewMetaVarForVar x))
		 (params (mlist mv-x))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist mv-x)) phi)))
		
		(decl-content 
		 (l1 () phi1                          ("Open" () ()))
		 (l2 () (exists (lam (x aa var) phi)) ("ExistsI" (mv-x) (l1)))
		 )
		(proc-content schema-interpreter)
		(remark "Application of the tactic ExistsI")
		)



(infer~defmethod ExistsE-m
		 (outline-mappings (((existent existent nonexistent) ExistsE-m-a)
				    ))
		 ;;(parameter-types term)
		 (help "The inference for the Exists-elimination method."))

(meth~defmethod ExistsE-m-a
		ExistsE-m
		(in limit)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables aa dd)
		 (sorted-meta-variables
		  (psi o term)
		  (phi o term)
		  (goal o term)
		  (params o termlist)
		  (newc o term)
		  (subst o sub)
		  (meta aa metavar)
		  )
		 
		 )
		;;(parameters newc)
		
		(application-condition
		 (mand (mbind newc (NewConstForVar v))
		       (mand (mforall meta (metavars phi)
				      (mand (not-subterm newc meta)
					    (Eigenvariable (:symbol CoSIE) newc meta)))
			     (mforall meta (metavars goal)
				      (mand (not-subterm newc meta)
					    (Eigenvariable (:symbol CoSIE) newc meta)))
			     ))
		 )
		(outline-computations 
		 (params (mlist newc))
		 (psi (subst-apply (subst-create (mlist v) (mlist newc)) phi))
		 )
		
		(premises (- L2) (+ L3))
		
		(conclusions (- L4))
		(decl-content
		 (L1 () psi             ("Hyp" () ()))
		 
		 (L2 ()   (exists (lam (v dd var) phi)))
		 
		 (L3 (L1) goal      ("OPEN" () ()))
		 (L4 ()   goal      ("ExistsE" params (L2 L3)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "Substantially, this method is a partial specification of the
<A HREF=\"../commands/existse.lml\">ExistsE</A> ND-rule. It introduces a new constant for the
variable<FONT COLOR=\"#00aa10\">v</FONT>, existentially quantified  in the premise
<FONT COLOR=\"#0000ff\">l1</FONT>.
"))
		
		(remark "");Exists-Elimination introducing the new hypothesis psi .)
		)



;;*******************************************************
;;*    Supermethods for planning limit theorems         *
;;*******************************************************


(infer~defsupermethod SOLVE*-S
		      (supermethod SOLVE*-S-B)
		      (help "The SOLVE* inference."))

(meth~defsupermethod SOLVE*-S-B
		     SOLVE*-S
		     (in limit)
		     (rating 35)
		     (reasoning :planning :middle-out )
		     (declarations
		      (sorted-meta-variables
		       (x num term) 
		       (y num term)
		       (rel (o num num) term)
		       (result o planlist)
		       (prec o prlnlist)
		       (prems o prlnlist)
		       (concls o prlnlist)
		       )
		      
		      )
		     (premises (0 prec) (+ prems))
		     (conclusions (- l1) (+ concls))
		     
		     (application-condition
		      (mand (mor
			     (mequal (:term less) rel)
			     (mequal (:term greater) rel))
			    (meval result)
			    (mbind prec (msecond result)))
		      )
		     (outline-computations
		      (prems (mfourth result))
		      (concls (mfifth result))
		      )
		     (expansion-function (plan~expand-supermethod l1 result))
		     (decl-content
		      (l1 () (rel x y)     ("solved" () prems))
		      )
		     (proc-content
		      (plan~iterate-planner l1
					    '()
					    '(Solve*-m-b 
					      Solve*2-m-b
					;Solve*-leq-m-b
					      AndI-m-b
					      Solve-m-b
					;Solve*<-m-b
					      )
					    '())
		      )
		     (manual (author "Juergen Zimmer")
			     (examples "Limit theorems")
			     (documentation ""))
		
		     (remark "<U>Solve*:</U><BR>
<TERM>(rel x y)</TERM> follows from<BR><LISP>(get-first-verb-for-solve prec l1)</LISP>,  if<BR>
<LISP>(get-second-verb-for-solve prec l1)</LISP>.")
		     )

(infer~defsupermethod DOMAINCASESPLIT-B-S
		      (supermethod DOMAINCASESPLIT-S-B)
		      (help "The DomainCaseSplit inference."))

(meth~defsupermethod DOMAINCASESPLIT-S-B
		     DOMAINCASESPLIT-B-S
		(in limit)
		(rating 0)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (ass o term)
		  (thm o term)
		  
		  (result o planlist)
		  (prems o prlnlist)
		  (hyps o prlnlist)
		  )
		 )
		(premises (+ prems))
 		(conclusions (- l1) (+ hyps))
		
		(application-condition
		 (mand (meval result)
		       )
		 )
		(outline-computations
		 (prems (mfourth result))
		 (hyps (mthird result))
		 )
		
		(expansion-function (plan~expand-supermethod l1 result))
		(decl-content
		 (l1 () Thm)
		 )
		(proc-content
		 (plan~iterate-planner l1      ;;goal
				       ()      ;; assumptions
				       '(DomainCaseSplit-m-b     ;;methods
					 ExFalsoQuodlibet-m-b
					 )

				       '(Choose-DomainCaseSplit-I
					 ))           ;; crules
		 )		
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark ""); We can conclude psi from the constraint state.)
		)


(infer~defsupermethod SOLVE-B-S
		      (supermethod SOLVE-S-B)
		      (help "The SOLVE<-BACKWARD inference."))

(meth~defsupermethod SOLVE-S-B
		     SOLVE-B-S
		(in limit)
		(rating 43)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (psi o term)
		  
		  (result o planlist)
		  (prems o prlnlist)
		  (prec o prlnlist)
		  )
		 )
		(premises (0 prec) (+ prems))
 		(conclusions (- l1))
		
		(application-condition
		 (mand (meval result)
		       (mbind prec (msecond result)))
		 )
		(outline-computations
		 (prems (mfourth result))
		 )
		
		(expansion-function (plan~expand-supermethod l1 result))
		(decl-content
		 (l1 () psi)
		 )
		(proc-content
		 (plan~iterate-planner l1 '() '(Solve-m-b AndI-m-b) '())
		 )		
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark ""); We can conclude psi from the constraint state.)
		)

(infer~defsupermethod SOLVE-F-S
		      (supermethod SOLVE-S-F)
		      (help "The SOLVE<-FORWARD inference."))

(meth~defsupermethod SOLVE-S-F
		     SOLVE-F-S
		     (in limit)
		     (rating 44)
		     (reasoning :planning :middle-out)
		     (declarations
		      (sorted-meta-variables
		       ;;(meta-var type sort)
		       (psi o term)
		       
		       (result o planlist)
		       (concls o prlnlist)
		       )
		      
		      )
		     (premises (- l1))
		     (conclusions (+ concls))
		     
		     (application-condition
		      (mand (meval result)
			    )
		      )
		     (outline-computations
		      (concls (mfifth result))
		      )
		     
		     (expansion-function (plan~expand-supermethod (first concls) result))
		     (decl-content
		      (l1 () psi)
		      )
		     (proc-content
		      (plan~iterate-planner '() L1 '(Solve-m-f AndE-m-f) '())
		      )		
		     (manual (author "Juergen Zimmer")
			     (examples "Limit theorems")
			     (documentation ""))
		
		     (remark "");The conjuncts of psi are introduced into constraint store.)
		     )


(infer~defsupermethod SKOLEMIZE-S
		      (supermethod SKOLEMIZE-S-B)
		      (help "The SKOLEMIZE backward inference."))

(meth~defsupermethod SKOLEMIZE-S-B SKOLEMIZE-S
		     (in limit)
		     (rating 20)
		     (declarations
		      (type-variables aa)
		      (sorted-meta-variables
		       
		       (quant (o (o aa)) term)
		       (phi o term)
		       
		       (result o planlist)
		       (prems o prlnlist)
		       (hyps o prlnlist)
		       (concls o prlnlist)
		       (prec o prlnlist)
		       )
		      
		      )
		     (premises (+ prems) )
		     (conclusions (- L2))
		     
		     (application-condition
		      (meval result)
		      )
		     (outline-computations
		      (prems (mfourth result))
		      )
		     
		     (expansion-function (plan~expand-supermethod L2 result))
		     (decl-content
		      (l2 () (quant (lam (x aa var) phi)))
		      )
		     (proc-content
		      (plan~iterate-planner L2 '() '(ForallI-m-b ExistsI-meta-m-b) '())
		      )		
		     (remark "");Calls the planner iteratively with the specified methods.)
		     )

;;;;;;;;;;;;; Methods for Limits (Chapter 2)
;;;;;;;;;;;;; by Mateja Jamnik and Martin Pollet

(infer~defmethod apply-lim-m
		 (outline-mappings (((existent nonexistent
					       nonexistent nonexistent) apply-lim-m-b)))
		 (help "Applies ...."))

(meth~defmethod apply-lim-m-b apply-lim-m
		(in limit)
		(rating 100)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (l1 num term)
		  (l2 num term)
		  (l3 num term)
		  (p1 num term)
		  (p2 (num num) term)
		  (p3 (num num) term)
		  (op (num num num) term)
		  (a num term)
		  (result num term)

		  )
		 )
		
		(premises (+ l10) (+ l20) (+ l30))

		(application-condition
		 (mand (appl-p p1)
		       (mbind op (applfunc p1))
		       (mor
			(mequal op (:term div))
			(mequal op (:term minus))
			(mequal op (:term plus))
			(mequal op (:term times)))
		       (cas-limit (mfirst (applargs p1)) x a l2)  ;compute by maple l2
		       (cas-limit (msecond (applargs p1)) x a l3) ;compute by maple l3
		       (mif (mequal op (:term div))
			    (mbind result (appl-create (:term and)
						       (mlist (appl-create (:term =)
									   (mlist (appl-create op (mlist l2 l3))
										  l1))
							      (appl-create (:term not)
									   (mlist (appl-create (:term =)
											       (mlist l3 (:term 0))))))))
			    (mbind result (appl-create (:term =)
						       (mlist (appl-create op (mlist l2 l3))
							      l1))))
		       ))
		
		(outline-computations
		 (y (vars-newvars (mlist x)))
		 (z (vars-newvars (mlist x)))
		 (p2 (abstr-create y
				   (subst-apply (subst-create (mlist x) y) (mfirst (applargs p1)))))
		 (p3 (abstr-create z
				   (subst-apply (subst-create (mlist x) z) (msecond (applargs p1)))))
		 )
		
		(conclusions (- L100))
		
		(decl-content
		 (L10 ()  result                   ("OPEN" () ()))
		 (L20 ()  (lim p3 a l3)            ("OPEN" () ()))
		 (L30 ()  (lim p2 a l2)            ("OPEN" () ()))
 		 (l100 () (lim (lam (x num var) p1) a l1)     ("SOLVED" () (L10 L20 L30) ))
		 )
		
		(manual (author "Mateja Jamnik and Martin Pollet")
			(examples "Limit theorems")
			(documentation "")
			)
		(remark ""
			)
		)


(infer~defmethod apply-dominance-m
		 (outline-mappings (((existent ;nonexistent
				               nonexistent) apply-dominance-m-b)))
		 (parameter-types term)
		 (help "Applies ...."))

(meth~defmethod apply-dominance-m-b apply-dominance-m
		(in limit)
		(rating 100)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (L num term)
		  (f (num num) term)
		  (x2 num var)

		  (c num metavar)  
		  (n num const)
		  (seq1 num term)
		  (seq2 num term)
		  (expseq num term)
		  )
		 )
		
		(parameters expseq)

		(application-condition 
		 (mand (bound-p expseq)
		       (abstr-p expseq))
		 )

		(outline-computations
		 (c (NewMetaVar (:symbol :c)  (:type num)))
		 (n (type-newconst (:type num)))
		 (seq2 (appl-create expseq (mlist n)))
		 (seq1 (beta-normalize (appl-create f (mlist n)))))
		 
		(premises (+ l10))
      
		(conclusions (- L100))
		
		(decl-content
		 (L10 ()  (less (div (minus seq1 l) seq2) c)                   ("OPEN" () ()))
					;(L20 ()  (limseq expseq 0)            ("OPEN" () ()))
 		 (l100 () (limseq f l)     ("SOLVED" () (L10) ))
		 )
		
		(manual (author "EM & MP")
			(examples "Limit theorems")
			(documentation "")
			)
		(remark ""
			)
		)



;;;;;;;;;;;;; Elementary Method to apply simplification tactics (eg. =reflexivity)
;;;;;;;;;;;;; by Mateja Jamnik and Martin Pollet

(infer~defmethod elementary-m
		 (outline-mappings (((existent ) elementary-m-b)))
		 (help "Applies ...."))

(meth~defmethod elementary-m-b elementary-m
		(in limit)
		(rating 100)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (exp1 num term)
		  (op (num num num) term)
		  (simp-exp1 num term)
		  (simp-exp2 num term)
		  (param o termlist)
		  )
		 )
		
		(premises )

		(application-condition
		 (mand (appl-p exp1)
		       (mbind op (applfunc exp1))
		       (mequal op (:term =))
					;(mor
		       (mequal (mfirst (applargs exp1)) (msecond (applargs exp1)))
		       ;;			(mand
		       ;;                         (mif (mnot (cas-simplify (mfirst (applargs exp1)) simp-exp1))
		       ;;                              (mbind simp-exp1 (mfirst (applargs exp1))))
;;                         (mif (mnot (cas-simplify (msecond (applargs exp1)) simp-exp2))
		       ;;                              (mbind simp-exp1 (msecond (applargs exp1))))
		       ;;                         (mequal simp-exp1 simp-exp2)))
		       ))
		
		(outline-computations
		 (param (mlist (mfirst (applargs exp1))))
		 )
		
		(conclusions (- L100))

		(decl-content
 		 (l100 () exp1 ("=ref" param () ))
		 )
		
		(manual (author "Mateja Jamnik and Martin Pollet")
			(examples "Limit theorems")
			(documentation "")
		)
		(remark ""
		)
	      )
#|
(infer~defsupermethod CALLPLANNER-S
		      (supermethod CALLPLANNER-S-B)
		      (help "The CALLPLANNER inference."))


(meth~defsupermethod CALLPLANNER-S-B CALLPLANNER-S
		     (in limit)
		     (rating 15)
		     (declarations
		      (sorted-meta-variables
		       (psi o term)
		       
		       (result o planlist)
		       (prec o prlnlist)
		       (prems o prlnlist)
		       (hyps o prlnlist)
		       (concls o prlnlist)
		       )
		      
		      )
		     (premises (0 prec) (+ prems)) 
		     (conclusions (- L1) (+ hyps))
		     
		     (application-condition
		      (mand (meval result)
			    (mnot (mequal result (mnil))))
		      )
		     (outline-computations
		      (prec (msecond result))
		      (hyps (mthird result))
		      (prems (mfourth result))
		      )
		     
		     (expansion-function (plan~expand-supermethod L1 result))
		     (decl-content
		      (l1 () psi)
		      )
		     (proc-content
		      (plan~call-planner-on-subproblem L1)
		      )
		     (remark "");We try to prove psi ".")
		     )
(infer~defmethod Solve=-m
		 (outline-mappings (((existent existent) Solve=-m-b)))
		 (help "Solve= tests consistency of an equality with constraint store."))


(meth~defmethod Solve=-m-b Solve=-m 
		(in limit)
		(rating 45)
		(declarations
		 (sorted-meta-variables
		  (meta o metavar)
		  (Mvar o metavar)
		  (val o term)
		  (a num)
		  (b num)
		  )
		 )
		(reasoning :planning :middle-out)
		(premises L0)
		(application-condition
		 (mand (meta-p meta)
		       (tell-cs (:symbol :CoSIE) L1) ;(meval result)
		       (if (meta-p a)
			   (mand (mbind Mvar a)
				 (mbind val b))
			 (if (meta-p b)
			     (mand (mbind Mvar b)
				   (mbind val a))
			   (mtrue))))
		 )
		
		(application-constraint
					;		 (cbind Mvar val)
		 )
		(conclusions (- L1))
		(decl-content
		 (L0 () meta)
		 (L1 () (= a b)       ("solver-CS" () (L0)))
		 )
		(remark "");(= a b) "is introduced into the constraint store.")
		)


(infer~defmethod Solve*<-m
		 (outline-mappings (((existent existent) Solve*<-m-b)
				    ((existent existent nonexistent) Solve*<-m-b))))

(meth~defmethod Solve*<-m-b Solve*<-m  
		(in limit)
		(rating 31)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (result o var)
		  (a num term)
		  (b num term)
		  (meta o metavar)
		  (prem o term)
		  (prems o prlnlist)
		  )
		 
		 )
		(premises L0 (+ prems))
		(application-condition
		 (mand (meta-p meta)
		       (if (tell-cs (:symbol :CoSIE) L1) ;; if the inequality could be solved   
			   (mand (mbind prems (mnil))    ;; there are no new subgoals
				 (mbind prem L0))        ;;
			 (mand (mbind prem (newprln (prlnhyps L1)
						    (appl-create (:term =) (mlist a b))))
			       (mbind prems (mcons prem (mnil)))))
		       ))
		(conclusions (- L1))
		(decl-content
		 (L0 () meta)
		 (L1 () (leq a b)  ("solve-disj" () (prem)))
		 )
		(remark "" ;<BR><TERM>(less a b)</TERM>is introduced into constraint store if
			;possible. Otherwise, the equality (= a b) becomes a new goal.<BR>)
			)
		)


;;*******************************************************
;;*       The ComplexEstimate-method                    *
;;*******************************************************

(meth~defmethod ComplexEstimate<-m-b ComplexEstimate<-m
		(in limit) 
		(rating 25)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (result o list) 
		  (rel (o num num) term)
		  (A num)
		  (Asigma num)
		  (B num) 
		  (k num)
		  (ksigma num)
		  (l num)
		  (lsigma num) 
		  (epsilon num)
		  (sigma o sub)   
		  (e1 num)
		  (M num const)
		  (positions o pos)
		  (F o term)
		  )
		  
		 )
		(premises L00 (+ L01) (+ L02) (+ L03) (+ L04))
		(application-condition
		 (mand (mor (mequal (:term less) rel)
			    (mequal (:term leq) rel))
		       (meval result)
		       (mbind k (mfirst result))
		       (mnot (mequal k (:term 0))))
		 )
		
		(outline-computations
		 (pos1 (:position (2 1)))
		 (M (newmetavar (:symbol :M) (termtype epsilon)))
		 (l (msecond result))
		 (sigma (mthird result))
		 (ksigma (subst-apply sigma k))
		 (Asigma (subst-apply sigma A))
		 (lsigma (subst-apply sigma l))
		 (F (mfourth result))
		 )   ;; wir sollten sigma auch noch als =-conjunct als neues subgoal
		;; einfuehren, oder?
		
		(conclusions (- Lthm))
		(decl-content
		 (L00 () (less (absval A) e1))
		 
		 (L01 () (less  (absval ksigma) M)                           ("OPEN" () ()))
		 (L02 () (less (absval Asigma) (div epsilon (times 2 M))) ("OPEN" () ()))
		 (L03 () (less (absval lsigma) (div epsilon 2))           ("OPEN" () ()))
		 (L04 () (less 0 M)                                      ("OPEN" () ()))
  		 ;(L05 () (leq 0 (absval Asigma))                         ("OPEN" () ()))
		 
		 (L1 () (= B (plus (times ksigma Asigma) lsigma))            ("solved" () ()));;; statt "CASextract"
		 
		 (L2 () (leq (absval B) (plus (absval (times ksigma Asigma))
					      (absval lsigma)))              ("triangle" () (L1)))
		 
		 (L3 () (leq (absval B) (plus (times (absval ksigma)
						     (absval Asigma))
					      (absval lsigma)))              ("abs-mult" (pos1) (L2)))
		 
		 (L4a () (leq (times (absval ksigma)
				     (absval Asigma))
			      (times M
				     (absval Asigma)))                    ("solved" ()
									   (L00
									    L01)));;L00
		 ;;ist zu
										  ;;viel /statt <=mult-r L05
		 (L4 () (leq (plus (times (absval ksigma)
					  (absval Asigma))
				   (absval lsigma))
			     (plus (times M (absval Asigma))
				   (absval lsigma)))                      ("<=add-r" ()
									   (L4a)))
		 (L5 () (leq (absval B)
			     (plus (times M (absval Asigma))
				   (absval lsigma)))                      ("<=trans" () (L3 L4)))
		 (L6 () (less (times M (absval Asigma))
			      (times M (div epsilon (times 2 M))))      ("<mult-l" ()
									   (L02 L04)))
		 (L7 () (less (plus (times M (absval Asigma))
				    (absval lsigma))
			      (plus (times M (div epsilon (times 2 M)))
				    (absval lsigma)))                     ("<add-r" () (L6)))
		 (L8 () (less (absval B)
			      (plus (times M (div epsilon (times 2 M)))
				    (absval lsigma)))                     ("<=trans<" () (L5 L7)))
		 (L9 () (less (plus (times M (div epsilon (times 2 M)))
				    (absval lsigma))
			      (plus (times M (div epsilon (times 2 M)))
				    (div epsilon 2)))                   ("<add-l" () (L03)))
		 (L10 () (less (absval B)
			       (plus (times M (div epsilon (times 2 M)))
				     (div epsilon 2)))                  ("<trans" () (L8 L9)))
		 
		 (Lthm () (rel (absval B) epsilon)                       ("solved" ()
									   (L10)));;statt "fixer-arith"
		 )
		;;===========================
		;; NOT CORRECT!!!!!!!!!!!!!!
		;;===========================
		(proc-content (lineq~EXTRACT L00 Lthm))
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark 
("<U>ComplexEstimate<:</U><BR>
We need to estimate the magnitude of
<BR><TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>.<BR>
To do this, we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>This goal can be shown in three steps:
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists an <TERM>M</TERM> such that <TERM> (less (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L02\"> 2. </A> <TERM> (less (absval Asigma) (div epsilon (times 2 M))) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (less (absval lsigma) (div epsilon 2))</TERM>,
</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR><A HREF=\"ltp:get-detailed-verbalization Lthm\">-> More details</A>"

"<U>ComplexEstimate<(detailed):</U><BR>
In order to estimate the magnitude of the complicated term
<BR><TERM>(less (absval B) epsilon)</TERM>, <BR>
we represent it as the linear combination
<BR><TERM>(absval (plus (times k Asigma) l))</TERM>.
<BR><BR>Now we use the Triangle Inequality and obtain<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>The magnitude of <BR><TERM>(plus (absval (times k Asigma)) (absval l))</TERM><BR>
can be estimated easily by the following three steps:<BR>
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists an <TERM>M</TERM> such that <TERM> (less (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L02\"> 2. </A> <TERM> (less (absval Asigma) (div epsilon (times 2 M))) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (less (absval lsigma) (div epsilon 2))</TERM>,
</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>=</TERM>   
<TERM>(plus (absval (times k Asigma)) l))</TERM>
<BR>
<TAB><TERM>leq</TERM>   
<TERM>(plus (absval (times k Asigma)) (absval l)))</TERM>
<BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>,
<BR>and therefore
<TERM>(less (absval B) epsilon)</TERM>,<BR>
which was to be shown.
")
("
<LISP>(get-motivation)</LISP><LISP>(get-estimate-variants)</LISP>the magnitude of
<BR><TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>,<BR>
we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR><LISP>(get-complex-estimate-variants)<BR>
<BLOCKQUOTE><LISP>(item)</LISP> There exists an <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.<BR>
<LISP>(verbalize-text-subproof l01)</LISP></BLOCKQUOTE>
<BLOCKQUOTE><LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon (times 2 M)))</TERM>:<BR>
<LISP>(verbalize-text-subproof l02)</LISP></BLOCKQUOTE>
<BLOCKQUOTE><LISP>(item)</LISP> <TERM>(less (absval l) (div epsilon 2))</TERM>:<BR>
<LISP>(verbalize-text-subproof l03)</LISP></BLOCKQUOTE>
<BR>Now<LISP>(get-conclusion)</LISP> <BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR>"
 "
<LISP>(get-motivation)</LISP><LISP>(get-estimate-variants)</LISP>the magnitude of
<BR><TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>,<BR>
we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>For proving <TERM>(less (plus (absval (times k Asigma)) (absval l)) epsilon)</TERM>,
<BR>it suffices to show that:<BR>
<BLOCKQUOTE><LISP>(item)</LISP> There exists an <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.<BR>
<LISP>(verbalize-cons-subproof l01)</LISP></BLOCKQUOTE>
<BLOCKQUOTE><LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon (times 2 M)))</TERM>:<BR>
<LISP>(verbalize-cons-subproof l02)</LISP></BLOCKQUOTE>
<BLOCKQUOTE><LISP>(item)</LISP> <TERM>(less (absval l) (div epsilon 2))</TERM>:<BR>
<LISP>(verbalize-cons-subproof l03)</LISP></BLOCKQUOTE>
<BR>From the collected restrictions follows<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR>")
)
)

(infer~defmethod Simplify-div-m
		 (outline-mappings (((existent nonexistent) Simplify-div-m-b)
				    ))
		 (help "Tests consistency of (in)equality with constraint store."))

(meth~defmethod Simplify-div-m-b Simplify-div-m 
		(in limit)
		(rating 40)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;;(meta-var type sort)
		  (B num term)
		  (C num term)
		  (e num term)
		  )
		 )
		
		(premises (+ L1))
		(application-condition
		 (mand (subterm (:term div) B)
		       (cas-simplify B C)
		       )
		 )

		(conclusions (- l2))
		(decl-content
		 (L1 () (less C e)                       ("OPEN" () ()))
		 (l2 () (less B e)                       ("solved" () ()))
		 )
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation ""))
		
		(remark "")
		)


(infer~defmethod Subst=-m
		 (outline-mappings (((existent existent nonexistent) Subst=-m-b)))
		 (help "The application of an equation."))

(meth~defmethod Subst=-m-b Subst=-m
		(in limit)
		(rating 45)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables

		  (F o term)
		  (G o term)
		  (Delta1 o prlnlist)              
		  (H o prlnlist)              
		  (position o pos)
		  (occsa o poslist)
		  (occsb o poslist)
		  (result o var)     
		  (a num term) (b num term)
		  )
		 )
		(premises L1 (+ L2))
		(application-condition
		 (termoccs a G occsa)
					;(termoccs b G occsb))
		 )
		(outline-computations
		 (F (termrplatpos b (mfirst occsa) G))
		 )
		(conclusions (- L3))
		(decl-content
		 (L1 () (= a b))
		 (L2 () F             ("OPEN" () ()))
		 (L3 () G             ("=subst" (position) (L1 L2)))
		 )
		(manual (author "Juergen Zimmer")
			(examples "all kind of problems")
			(documentation "The method Subst=-m-b is a partial specification of the
<A HREF=\"../commands/=subst.lml\">=subst</A> tactic applied backwards."))
		(remark "")
		) ;A method for the =subst tactic.


(infer~defmethod Skolemize-m
		 (outline-mappings (((existent nonexistent) Skolemize-m-b)
				    ))
		 (parameter-types term-list)
		 )

(meth~defmethod Skolemize-m-b Skolemize-m
		(in limit)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (result o termlist)
		  (psi o term)
		  (phi o term)
		  (consts o termlist)
		  (metas o termlist)
		  (params o termlist)
		  )
		 )
		
		(parameters params) 
		(premises (+ L1))
		(conclusions (- L2))
		
		(application-condition
		 (mand (mor (universal-p phi)
			    (existential-p phi))
		       (meval result)
		       ))
		(outline-computations
		 (psi (mfirst result))
		 (params (msecond result))
		 )
		
		(decl-content
		 (L1 () psi 	 ("OPEN" () ()))
		 
		 (L2 () phi       ("skolemize" params (L1)))
		 )
		(proc-content (limtac~skolemize phi T))
		(manual (author "Juergen Zimmer")
			(examples "Limit theorems")
			(documentation "This methods eliminates all top level quanitifiers in the
conclusion <FONT COLOR=\"#0000ff\">l2</FONT>, introducing new constants for universally quantified
variables and meta-variables for existentially quantified variables. <BR>
<FONT COLOR=\"#aa0000\">NOTE:</FONT> This method does not introduce Skolem terms in the sense of
Skolemization! <BR>
"))
		(remark "");Skolemizing phi introducing the new symbols params .)
		)


|#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; THE FOLLOWING METHODS ARE INSERTED FOR USING MULTI TO SOLVE
;; LIMES PROBLEMS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MSet-focus-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod MSet-focus-m
		 (outline-mappings (((existent existent nonexistent) mset-focus-m-b)))
		 (parameter-types position) 
		 (help "Set the Focus"))

(meth~defmethod mset-focus-m-b MSet-focus-m
		(in real)
		(rating 43)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (thm o term)
		  (new-thm o term)
		  (ass o term)
		  (posit o pos)
		  )
		 )
		(parameters posit)
		(premises l1 (+ new-thm))
		(conclusions (- l2))
		(application-condition
		 (mand (bound-p posit)
		       (mnot-empty-pos posit)))  ;; position darf nicht die leere position sein
		(outline-computations
		 (new-thm (mfocussednewprln (prlnhyps l2)
					     thm
					     l1
					     posit)))
		(decl-content
		 (l1 () ass)
		 (l2 () thm ("Weaken" () (new-thm))) 
		 )
		(proc-content schema-interpreter)
		(remark "This method sets a focus on a subformula in an assumption respective to a goal")
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mflipabsval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Since all ComplexEstimate Methods work only with absolute Values on the left hand side of the inequations
;; all inequations with absolute values on the right hand side should be flipped

(infer~defmethod mflip-absval-inequality-m
		 (outline-mappings (((existent nonexistent) mflip-absval-inequality-m-b)
				    ((nonexistent closed) mflip-absval-inequality-m-f)))
		 (help "Flips inequalities if absval is on the right side ..."))

(meth~defmethod mflip-absval-inequality-m-b mflip-absval-inequality-m
		(in real)
		(rating 10)
		(reasoning :restricting)

		(declarations
		 (sorted-meta-variables
		  (a num term)
		  (b num term)
		  (rel1 (o num num))
		  (rel2 (o num num))
		  ))

		(premises (+ l1))
		(conclusions (- l2))

		(application-condition
		 (mand (mor (mand (mequal (:term less) rel1)
				  (mbind rel2 (:term greater)))
			    (mand (mequal (:term leq) rel1)
				  (mbind rel2 (:term geq)))
			    (mand (mequal (:term geq) rel1)
				  (mbind rel2 (:term leq)))
			    (mand (mequal (:term greater) rel1)
				  (mbind rel2 (:term lesser))))))
		
		(decl-content
		 (l1 () (rel2 (absval a) b) ("Open" () ()))
		 (l2 () (rel1 b (absval a)) ("Flip" () (l1)))
		 )
		(proc-content schema-interpreter)
		(remark "This method turns the absval in an inequality to the left hand side."))

(meth~defmethod mflip-absval-inequality-m-f mflip-absval-inequality-m
		(in real)
		(rating 10)
		(reasoning :normalizing)
		(declarations
		 (sorted-meta-variables
		  (a num term)
		  (b num term)
		  (rel1 (o num num))
		  (rel2 (o num num))
		  ))
		(premises (- l1))
		(conclusions (+ l2))

		(application-condition
		 (mand (mor (mand (mequal (:term less) rel1)
				  (mbind rel2 (:term greater)))
			    (mand (mequal (:term leq) rel1)
				  (mbind rel2 (:term geq)))
			    (mand (mequal (:term geq) rel1)
				  (mbind rel2 (:term leq)))
			    (mand (mequal (:term greater) rel1)
				  (mbind rel2 (:term lesser))))))
				
		(decl-content
		 (l1 () (rel1 a (absval b)))
		 (l2 () (rel2 (absval b) a) ("Flip" () (l1)))
		 )
		(proc-content schema-interpreter)
		(remark "This method turns the absval in an inequality to the left hand side."))


#| ------------------------------------------------MCOMPLEX-ESTIMATE METHODS -------------------------------------------- |#

;;
;; These are the COmplexEstimate Methods slightly changed for the use in MULTI
;; In particular, in all the complexestimate Methods the applications of the corresponding Solve*<,<= etc. Methods
;; are directly integrated
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Complex Estimate with CAS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod MComplexEstimate<-m
		 (outline-mappings (((existent
				      existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     MComplexEstimate<-m-b))))

(meth~defmethod MComplexEstimate<-m-b MComplexEstimate<-m
		;;NOTE: *******************************
		;;  the application of solve*< is integrated into this version of CE<=
		;;**************************************
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;; parameter der input-gleichungen (ass + thm)
		  (a num term) (b num term) (epsilon num term) (e1 num term)
		  ;; aus casextract
		  (l num term) (k num term)
		  ;; substituierte
		  (asigma num term) (bsigma num term) (epsilonsigma num term) (e1sigma num term) (ksigma num term) (lsigma num term) 
		  ;; neue Metavariable
		  (m num metavar)
		  ;; substitution
		  (sigma o sub)
		  ;; position
		  (pos o pos)
		  (pos2 o pos)
		  ;; the-cas
		  (the-cas o symbol)
		  ;; Neue Gleichungsziele aus unification 
		  (eqprems o prlnlist)
		  ;; Eine neue Zeile mit einem Conjunct von Gleichnungen
		  (=conjunct o term)
		  ))
		
		(premises LS1 (+ L01) (+ LS2) (+ L03) (+ L04) (+ LS0))
		(conclusions (- Lthm))
		
		(application-condition
		 (mand (getcassubst A B sigma)               ;; get the mystic substitution
		       (mbind Asigma (subst-apply sigma A))  ;; create ASIGMA
		       ;; (alphaunify A Asigma mgu)             ;; braucht man wohl nicht AMEIER! try to unify A and Asigma via 'mgu'
		       (casextract Asigma B k l sigma)       ;; compute linear combination
		       (mnot (mequal k (:term 0)))           ;; do not accept the trivial cases
		       (mnot (mand (mequal k (:term 1))      ;; where A = B...
				   (mequal l (:term 0))))
		       (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		       (no-Eigenvars-in (:symbol COSIE) M) ;; the new variable M should not contain any Eigenvars
		       
		       (mbind Bsigma (subst-apply sigma B))
		       (mbind e1sigma (subst-apply sigma e1))
		       (mbind epsilonsigma (subst-apply sigma epsilon))
		       (mbind ksigma (subst-apply sigma k))
		       (mbind lsigma (subst-apply sigma l))
		       )
		 )
		
		(outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 )

		(expansion-computations
		 (pos1 (:position (2 1)))
		 (pos (:position ()))
		 (pos2 (:position (2)))
		 (the-cas (:symbol mass)))
		
		(decl-content
		 (LS0 () =conjunct                                              ("OPEN" () ()))
		 (LS1 () (less (absval A) e1))
		 (LS2 () (leq e1sigma (div epsilonsigma (times 2 M)))           ("OPEN" () ()))
		 
		 (L01 () (leq (absval ksigma) M)                                ("OPEN" () ()))
		 (L02 () (less (absval Asigma) (div epsilonsigma (times 2 M)))  ("<trans<=" () (LS1 LS2)))	
		 (L03 () (less (absval lsigma) (div epsilonsigma 2))            ("OPEN" () ()))
		 (L04 () (less 0 M)                                             ("OPEN" () ()))

		 (L0 () (= Bsigma Bsigma)                                       ("=ref" (B) ()))
		 (L1 () (= Bsigma (plus (times ksigma Asigma) lsigma))          ("cas" (pos the-cas) (L0)))
		 (L2 () (leq (absval Bsigma) (plus (absval (times ksigma Asigma))
						   (absval lsigma)))            ("triangle" () (L1)))
		 (L3 () (leq (absval Bsigma) (plus (times (absval ksigma)
							  (absval Asigma))
						   (absval lsigma)))            ("abs-mult" (pos1) (L2)))
		 (L4b () (leq 0 (absval Asigma))                                ("0<=abs" () ()))
		 (L4a () (leq (times (absval ksigma)
				     (absval Asigma))
			      (times M
				     (absval Asigma)))                          ("<=mult-r" () (L01 L4b)))
		 (L4 () (leq (plus (times (absval ksigma) (absval Asigma))
				   (absval lsigma))
			     (plus (times M (absval Asigma))
				   (absval lsigma)))                            ("<=add-r" () (L4a)))
		 (L5 () (leq (absval Bsigma)
			     (plus (times M (absval Asigma)) (absval lsigma)))  ("<=trans" () (L3 L4)))
		 (L6 () (less (times M (absval Asigma))
			      (times M (div epsilonsigma (times 2 M))))         ("<mult-l" () (L02 L04)))
		 (L7 () (less (plus (times M (absval Asigma))
				    (absval lsigma))
			      (plus (times M (div epsilonsigma (times 2 M)))
				    (absval lsigma)))                           ("<add-r" () (L6)))
		 (L8 () (less (absval Bsigma)
			      (plus (times M (div epsilonsigma (times 2 M)))
				    (absval lsigma)))                           ("<=trans<" () (L5 L7)))
		 (L9 () (less (plus (times M (div epsilonsigma (times 2 M)))
				    (absval lsigma))
			      (plus (times M (div epsilonsigma (times 2 M)))
				    (div epsilonsigma 2)))                      ("<add-l" () (L03)))
		 (L10 () (less (absval Bsigma)
			       (plus (times M (div epsilonsigma (times 2 M)))
				     (div epsilonsigma 2)))                     ("<trans" () (L8 L9)))
		 (Lthm () (less (absval B) epsilon)                             ("solved" () (l10)))
		 )
		(remark 
("<U>ComplexEstimate<:</U><BR>
We need to estimate the magnitude of
<BR><TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>.<BR>
To do this, we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>This goal can be shown in three steps:
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists a real number <TERM>M</TERM> such that <TERM> (less (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization LS2\"> 2. </A> <TERM> (less (absval Asigma) (div epsilon (times 2 M))) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (less (absval l) (div epsilon 2))</TERM>,
<BR> Since there is the assumption <A HREF=\"ltp:get-local-verbalization LS1\"> </A> <TERM> (less (absval Asigma) e1sigma)</TERM>,
<BR> for 2 it is enough to show <A HREF=\"ltp:get-local-verbalization LS1\"> </A> <TERM> (leq e1sigma (div epsilonsigma (times 2 M)))</TERM>
</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR><A HREF=\"ltp:get-detailed-verbalization Lthm\">-> More details</A><BR>"

"<U>ComplexEstimate<(detailed):</U><BR>
In order to estimate the magnitude of the complicated term
<BR><TERM>(less (absval B) epsilon)</TERM>, <BR>
we represent it as the linear combination
<BR><TERM>(absval (plus (times k Asigma) l))</TERM>.
<BR><BR>Now we use the Triangle Inequality and obtain<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>The magnitude of <BR><TERM>(plus (absval (times k Asigma)) (absval l))</TERM><BR>
can be estimated easily by the following three steps:<BR>
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists a real number <TERM>M</TERM> such that <TERM> (less (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization LS2\"> 2. </A> <TERM> (less (absval Asigma) (div epsilon (times 2 M))) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (less (absval l) (div epsilon 2))</TERM>,
<BR> Since there is the assumption <A HREF=\"ltp:get-local-verbalization LS1\"> </A> <TERM> (less (absval Asigma) e1sigma)</TERM>,
<BR> for 2 it is enough to show <A HREF=\"ltp:get-local-verbalization LS1\"> </A> <TERM> (leq e1sigma (div epsilonsigma (times 2 M)))</TERM>

</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>=</TERM>   
<TERM>(plus (absval (times k Asigma)) l))</TERM>
<BR>
<TAB><TERM>leq</TERM>   
<TERM>(plus (absval (times k Asigma)) (absval l)))</TERM>
<BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>,
<BR>and therefore
<TERM>(less (absval B) epsilon)</TERM>,<BR>
which was to be shown.<BR>
")
("
<LISP>(header)</LISP>
<TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>,<BR>
we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR><LISP>(get-complex-estimate-variants)</LISP>
<LISP>(increase-depth)</LISP><BR><BLOCKQUOTE>
<LISP>(item)</LISP> There exists a real number <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l01)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon (times 2 M)))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof lS2)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval l) (div epsilon 2))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l03)</LISP></BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
</BLOCKQUOTE>
<LISP>(get-reasoning-variants)</LISP><LISP>(get-conclusion)</LISP> <BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR>"
 "
<LISP>(header)</LISP>
<TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>,<BR>
we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR><LISP>(get-motivation)</LISP><LISP>(get-action-variants)</LISP>
<TERM>(less (plus (absval (times k Asigma)) (absval l)) epsilon)</TERM>,
<BR><LISP>(get-motivation2)</LISP>show that:<LISP>(increase-depth)</LISP><BR>
<BLOCKQUOTE>
<LISP>(item)</LISP> There exists a real number <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l01)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon (times 2 M)))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof lS2)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval l) (div epsilon 2))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l03)</LISP></BLOCKQUOTE>
</BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
<LISP>(get-restriction-variants)</LISP><BR>
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR>")
))


(infer~defmethod MComplexEstimate<=-m
		 (outline-mappings (((existent
				      existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     MComplexEstimate<=-m-b))))

(meth~defmethod MComplexEstimate<=-m-b MComplexEstimate<=-m
		;;NOTE: *******************************
		;;  the application of solve*< is integrated into this version of CE<=
		;;**************************************
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;; parameter der input-gleichungen (ass + thm)
		  (a num term) (b num term) (epsilon num term) (e1 num term)
		  ;; aus casextract
		  (l num term) (k num term)
		  ;; substituierte
		  (asigma num term) (bsigma num term) (epsilonsigma num term) (e1sigma num term) (ksigma num term) (lsigma num term) 
		  ;; neue Metavariable
		  (m num metavar)
		  ;; substitution
		  (sigma o sub)
		  ;; position
		  (pos o pos)
		  ;; the-cas
		  (the-cas o symbol)
		  ;; Neue Gleichungsziele aus unification 
		  (eqprems o prlnlist)
		  ;; Eine neue Zeile mit einem Conjunct von Gleichnungen
		  (=conjunct o term)
		  ))
		
		(premises LS1 (+ L01) (+ LS2) (+ L03) (+ L04) (+ LS0))
		(conclusions (- Lthm))
		
		(application-condition
		 (mand (getcassubst A B sigma)               ;; get the mystic substitution
		       (mbind Asigma (subst-apply sigma A))  ;; create ASIGMA
		       ;; (alphaunify A Asigma mgu)             ;; braucht man wohl nicht AMEIER! try to unify A and Asigma via 'mgu'
		       (casextract Asigma B k l sigma)       ;; compute linear combination
		       (mnot (mequal k (:term 0)))           ;; do not accept the trivial cases
		       (mnot (mand (mequal k (:term 1))      ;; where A = B...
				   (mequal l (:term 0))))
		       (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		       (no-Eigenvars-in (:symbol COSIE) M) ;; the new variable M should not contain any Eigenvars
		       
		       (mbind Bsigma (subst-apply sigma B))
		       (mbind e1sigma (subst-apply sigma e1))
		       (mbind epsilonsigma (subst-apply sigma epsilon))
		       (mbind ksigma (subst-apply sigma k))
		       (mbind lsigma (subst-apply sigma l))
		       )
		 )
		
		(outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 )

		(expansion-computations
		 (pos1 (:position (2 1)))
		 (pos (:position ()))
		 (the-cas (:symbol mass)))
		
		(decl-content
		 (LS0 () =conjunct                                              ("OPEN" () ()))
		 (LS1 () (less (absval A) e1))
		 (LS2 () (leq e1sigma (div epsilonsigma (times 2 M)))           ("OPEN" () ()))
		 
		 (L01 () (leq (absval ksigma) M)                                ("OPEN" () ()))
		 (L02 () (less (absval Asigma) (div epsilonsigma (times 2 M)))  ("<trans<=" () (LS1 LS2)))	
		 (L03 () (leq (absval lsigma) (div epsilonsigma 2))             ("OPEN" () ()))
		 (L04 () (less 0 M)                                             ("OPEN" () ()))

		 (L0 () (= Bsigma Bsigma)                                       ("=ref" (B) ()))
		 (L1 () (= Bsigma (plus (times ksigma Asigma) lsigma))          ("cas" (pos the-cas) (L0)))
		 (L2 () (leq (absval Bsigma) (plus (absval (times ksigma Asigma))
						   (absval lsigma)))            ("triangle" () (L1)))
		 (L3 () (leq (absval Bsigma) (plus (times (absval ksigma)
							  (absval Asigma))
						   (absval lsigma)))            ("abs-mult" (pos1) (L2)))
		 (L4b () (leq 0 (absval Asigma))                                ("0<=abs" () ()))
		 (L4a () (leq (times (absval ksigma)
				     (absval Asigma))
			      (times M
				     (absval Asigma)))                          ("<=mult-r" () (L01 L4b)))
		 (L4 () (leq (plus (times (absval ksigma) (absval Asigma))
				   (absval lsigma))
			     (plus (times M (absval Asigma))
				   (absval lsigma)))                            ("<=add-r" () (L4a)))
		 (L5 () (leq (absval Bsigma)
			     (plus (times M (absval Asigma)) (absval lsigma)))  ("<=trans" () (L3 L4)))
		 (L6 () (less (times M (absval Asigma))
			      (times M (div epsilonsigma (times 2 M))))         ("<mult-l" () (L02 L04)))
		 (L7 () (less (plus (times M (absval Asigma))
				    (absval lsigma))
			      (plus (times M (div epsilonsigma (times 2 M)))
				    (absval lsigma)))                           ("<add-r" () (L6)))
		 (L8 () (less (absval Bsigma)
			      (plus (times M (div epsilonsigma (times 2 M)))
				    (absval lsigma)))                           ("<=trans<" () (L5 L7)))
		 (L9 () (leq (plus (times M (div epsilonsigma (times 2 M)))
				   (absval lsigma))
			     (plus (times M (div epsilonsigma (times 2 M)))
				   (div epsilonsigma 2)))                       ("<=add-l" () (L03)))
		 (L10 () (less (absval Bsigma)
			       (plus (times M (div epsilonsigma (times 2 M)))
				     (div epsilonsigma 2)))                      ("<trans<=" () (L8 L9)))
		 (Lthm () (leq (absval B) epsilon)                              ("solved" () (L10)))
		 )
		(remark 
("<U>ComplexEstimate<:</U><BR>
We need to estimate the magnitude of
<BR><TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>.<BR>
To do this, we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>This goal can be shown in three steps:
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists a real number <TERM>M</TERM> such that <TERM> (leq (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization LS2\"> 2. </A> <TERM> (leq (absval Asigma) (div epsilon (times 2 M))) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (leq (absval l) (div epsilon 2))</TERM>,
<BR> Since there is the assumption <A HREF=\"ltp:get-local-verbalization LS1\"> </A> <TERM> (less (absval Asigma) e1sigma)</TERM>,
<BR> for 2 it is enough to show <A HREF=\"ltp:get-local-verbalization LS1\"> </A> <TERM> (leq e1sigma (div epsilonsigma (times 2 M)))</TERM>
</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>leq</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR><A HREF=\"ltp:get-detailed-verbalization Lthm\">-> More details</A><BR>"

"<U>ComplexEstimate<(detailed):</U><BR>
In order to estimate the magnitude of the complicated term
<BR><TERM>(less (absval B) epsilon)</TERM>, <BR>
we represent it as the linear combination
<BR><TERM>(absval (plus (times k Asigma) l))</TERM>.
<BR><BR>Now we use the Triangle Inequality and obtain<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR>The magnitude of <BR><TERM>(plus (absval (times k Asigma)) (absval l))</TERM><BR>
can be estimated easily by the following three steps:<BR>
<BLOCKQUOTE>
<A HREF=\"ltp:get-local-verbalization L01\"> 1. </A> There exists a real number <TERM>M</TERM> such that <TERM> (leq (absval k) M)</TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization LS2\"> 2. </A> <TERM> (leq (absval Asigma) (div epsilon (times 2 M))) </TERM>
, and<BR>
<A HREF=\"ltp:get-local-verbalization L03\"> 3. </A> <TERM> (leq (absval l) (div epsilon 2))</TERM>,
<BR> Since there is the assumption <A HREF=\"ltp:get-local-verbalization LS1\"> </A> <TERM> (less (absval Asigma) e1sigma)</TERM>,
<BR> for 2 it is enough to show <A HREF=\"ltp:get-local-verbalization LS1\"> </A> <TERM> (leq e1sigma (div epsilonsigma (times 2 M)))</TERM>

</BLOCKQUOTE>
Then<BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>=</TERM>   
<TERM>(plus (absval (times k Asigma)) l))</TERM>
<BR>
<TAB><TERM>leq</TERM>   
<TERM>(plus (absval (times k Asigma)) (absval l)))</TERM>
<BR>
<TAB><TERM>leq</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>,
<BR>and therefore
<TERM>(leq (absval B) epsilon)</TERM>,<BR>
which was to be shown.<BR>
")
("
<LISP>(header)</LISP>
<TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>,<BR>
we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR><LISP>(get-complex-estimate-variants)</LISP>
<LISP>(increase-depth)</LISP><BR><BLOCKQUOTE>
<LISP>(item)</LISP> There exists a real number <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l01)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon (times 2 M)))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof lS2)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval l) (div epsilon 2))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-text-subproof l03)</LISP></BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
</BLOCKQUOTE>
<LISP>(get-reasoning-variants)</LISP><LISP>(get-conclusion)</LISP> <BR>  
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR>"
 "
<LISP>(header)</LISP>
<TERM>(= (absval B) (absval (plus (times k Asigma)(absval l))))</TERM>,<BR>
we use the Triangle Inequality and obtain:<BR>
<TERM>(leq (absval B) (plus (absval (times k Asigma)) (absval l)))</TERM>.
<BR><BR><LISP>(get-motivation)</LISP><LISP>(get-action-variants)</LISP>
<TERM>(less (plus (absval (times k Asigma)) (absval l)) epsilon)</TERM>,
<BR><LISP>(get-motivation2)</LISP>show that:<LISP>(increase-depth)</LISP><BR>
<BLOCKQUOTE>
<LISP>(item)</LISP> There exists a real number <TERM>M</TERM> such that <TERM>(less (absval k) M)</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l01)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval Asigma) (div epsilon (times 2 M)))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof lS2)</LISP></BLOCKQUOTE><BR>
<LISP>(item)</LISP> <TERM>(less (absval l) (div epsilon 2))</TERM>.
<BLOCKQUOTE>Proof: <LISP>(verbalize-cons-subproof l03)</LISP></BLOCKQUOTE>
</BLOCKQUOTE>
<LISP>(decrease-depth)</LISP>
<LISP>(get-restriction-variants)</LISP><BR>
  <TAB>  <TERM>(absval B)</TERM><BR>
<TAB><TERM>less</TERM>     
    <TERM>(plus(times M (div epsilon (times 2 M)))(div epsilon 2))</TERM>
<BR>
<TAB><TERM>=</TERM>    
    <TERM>epsilon</TERM>.
<BR>")
))



(infer~defmethod MComplexEstimate>-m
		 (outline-mappings (((existent
				      existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     MComplexEstimate>-m-b))))

(meth~defmethod MComplexEstimate>-m-b MComplexEstimate>-m
		;;NOTE: *******************************
		;;  the application of solve*< is integrated into this version of CE>=
		;;**************************************
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;; parameter der input-gleichungen (ass + thm)
		  (a num term) (b num term) (epsilon num term) (e1 num term)
		  ;; aus casextract
		  (l num term) (k num term)
		  ;; substituierte
		  (asigma num term) (bsigma num term) (epsilonsigma num term) (e1sigma num term) (ksigma num term) (lsigma num term) 
		  ;; neue Metavariable
		  (m num metavar)
		  ;; substitution
		  (sigma o sub)
		  ;; position
		  (pos o pos)
		  ;; the-cas
		  (the-cas o symbol)
		  ;; Neue Gleichungsziele aus unification 
		  (eqprems o prlnlist)
		  ;; Eine neue Zeile mit einem Conjunct von Gleichnungen
		  (=conjunct o term)
		  ))
		
		(premises L00 (+ L01) (+ L02) (+ L03) (+ L04) (+ LS0))
		(conclusions (- Lthm))
		
		(application-condition
		 (mand (getcassubst A B sigma)               ;; get the mystic substitution
		       (mbind Asigma (subst-apply sigma A))  ;; create ASIGMA
		       ;; (alphaunify A Asigma mgu)             ;; braucht man wohl nicht AMEIER! try to unify A and Asigma via 'mgu'
		       (casextract Asigma B k l sigma)       ;; compute linear combination
		       (mnot (mequal k (:term 0)))           ;; do not accept the trivial cases
		       (mnot (mand (mequal k (:term 1))      ;; where A = B...
				   (mequal l (:term 0))))
		       (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		       (no-Eigenvars-in (:symbol COSIE) M) ;; the new variable M should not contain any Eigenvars

		       (mbind Bsigma (subst-apply sigma B))
		       (mbind e1sigma (subst-apply sigma e1))
		       (mbind epsilonsigma (subst-apply sigma epsilon))
		       (mbind ksigma (subst-apply sigma k))
		       (mbind lsigma (subst-apply sigma l))
		       )
		 )
		
		(outline-computations
		 ;;(eqprems (mcompute-eq-lines sigma lthm))
		 (=conjunct (subst-to-conjunct sigma))
		 )
				
		(decl-content
		 (LS0 () =conjunct                                                    ("OPEN" () ()))
		 (L00 () (less (absval A) e1))

		 (L01 () (less 0 M)                                                   ("OPEN"          () ()))
		 (L02 () (less (absval ksigma) M)                                     ("OPEN"          () ()))
		 (L03 () (geq (absval lsigma) (times epsilonsigma 2))             ("OPEN"          () ()))
		 (L04 () (less e1sigma (div epsilonsigma M))                          ("OPEN"          () ()))
		 
		 (Lthm () (greater (absval B) epsilon)                                ("solved"       () (L00 L01 L02 L03 L04)))
		 )
		)

(infer~defmethod MComplexEstimate>=-m
		 (outline-mappings (((existent
				      existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     MComplexEstimate>=-m-b))))

(meth~defmethod MComplexEstimate>=-m-b MComplexEstimate>=-m
		;;NOTE: *******************************
		;;  the application of solve*< is integrated into this version of CE>=
		;;**************************************
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;; parameter der input-gleichungen (ass + thm)
		  (a num term) (b num term) (epsilon num term) (e1 num term)
		  ;; aus casextract
		  (l num term) (k num term)
		  ;; substituierte
		  (asigma num term) (bsigma num term) (epsilonsigma num term) (e1sigma num term) (ksigma num term) (lsigma num term) 
		  ;; neue Metavariable
		  (m num metavar)
		  ;; substitution
		  (sigma o sub)
		  ;; position
		  (pos o pos)
		  ;; the-cas
		  (the-cas o symbol)
		  ;; Neue Gleichungsziele aus unification 
		  (eqprems o prlnlist)
		  ;; Eine neue Zeile mit einem Conjunct von Gleichnungen
		  (=conjunct o term)
		  ))
		
		(premises L00 (+ L01) (+ L02) (+ L03) (+ L04) (+ LS0))
		(conclusions (- Lthm))
		
		(application-condition
		 (mand (getcassubst A B sigma)               ;; get the mystic substitution
		       (mbind Asigma (subst-apply sigma A))  ;; create ASIGMA
		       ;; (alphaunify A Asigma mgu)             ;; braucht man wohl nicht AMEIER! try to unify A and Asigma via 'mgu'
		       (casextract Asigma B k l sigma)       ;; compute linear combination
		       (mnot (mequal k (:term 0)))           ;; do not accept the trivial cases
		       (mnot (mand (mequal k (:term 1))      ;; where A = B...
				   (mequal l (:term 0))))
		       (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		       (no-Eigenvars-in (:symbol COSIE) M) ;; the new variable M should not contain any Eigenvars

		       (mbind Bsigma (subst-apply sigma B))
		       (mbind e1sigma (subst-apply sigma e1))
		       (mbind epsilonsigma (subst-apply sigma epsilon))
		       (mbind ksigma (subst-apply sigma k))
		       (mbind lsigma (subst-apply sigma l))
		       )
		 )
		
		(outline-computations
		 ;;(eqprems (mcompute-eq-lines sigma lthm))
		 (=conjunct (subst-to-conjunct sigma))
		 )
				
		(decl-content
		 (LS0 () =conjunct                                                    ("OPEN" () ()))
		 (L00 () (less (absval A) e1))

		 (L01 () (less 0 M)                                                   ("OPEN"          () ()))
		 (L02 () (less (absval ksigma) M)                                     ("OPEN"          () ()))
		 (L03 () (greater (absval lsigma) (times epsilonsigma 2))             ("OPEN"          () ()))
		 (L04 () (less e1sigma (div epsilonsigma M))                          ("OPEN"          () ()))
		 
		 (Lthm () (geq (absval B) epsilon)                                    ("solved"       () (L00 L01 L02 L03 L04)))
		 )
		)

(infer~defmethod MComplexEstimate>>-m
		 (outline-mappings (((existent
				      existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     MComplexEstimate>>-m-b))))

;; BRaucht man fuer 4.1.12
(meth~defmethod MComplexEstimate>>-m-b MComplexEstimate>>-m
		;;NOTE: *******************************
		;;  the application of solve*< is integrated into this version of CE>=
		;;**************************************
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  ;; parameter der input-gleichungen (ass + thm)
		  (a num term) (b num term) (epsilon num term) (e1 num term)
		  ;; aus casextract
		  (l num term) (k num term)
		  ;; substituierte
		  (asigma num term) (bsigma num term) (epsilonsigma num term) (e1sigma num term) (ksigma num term) (lsigma num term) 
		  ;; neue Metavariable
		  (m num metavar)
		  ;; substitution
		  (sigma o sub)
		  ;; position
		  (pos o pos)
		  ;; the-cas
		  (the-cas o symbol)
		  ;; Neue Gleichungsziele aus unification 
		  (eqprems o prlnlist)
		  ;; Eine neue Zeile mit einem Conjunct von Gleichnungen
		  (=conjunct o term)
		  ))
		
		(premises L00 (+ L01) (+ L02) (+ L03) (+ L04) (+ LS0))
		(conclusions (- Lthm))
		
		(application-condition
		 (mand (getcassubst A B sigma)               ;; get the mystic substitution
		       (mbind Asigma (subst-apply sigma A))  ;; create ASIGMA
		       ;; (alphaunify A Asigma mgu)             ;; braucht man wohl nicht AMEIER! try to unify A and Asigma via 'mgu'
		       (casextract Asigma B k l sigma)       ;; compute linear combination
		       (mnot (mequal k (:term 0)))           ;; do not accept the trivial cases
		       (mnot (mand (mequal k (:term 1))      ;; where A = B...
				   (mequal l (:term 0))))
		       (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		       (no-Eigenvars-in (:symbol COSIE) M) ;; the new variable M should not contain any Eigenvars
		       
		       (mbind Bsigma (subst-apply sigma B))
		       (mbind e1sigma (subst-apply sigma e1))
		       (mbind epsilonsigma (subst-apply sigma epsilon))
		       (mbind ksigma (subst-apply sigma k))
		       (mbind lsigma (subst-apply sigma l))
		       )
		 )
		
		(outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 ;;(eqprems (mcompute-eq-lines sigma lthm))
		 )
		
		(decl-content
		 (LS0 () =conjunct                                                 ("OPEN" () ()))
		 (L00 () (greater (absval A) e1))
		 		 
		 (L01 () (less 0 M)                                                ("OPEN"          () ()))
		 (L02 () (geq (absval ksigma) M)                                   ("OPEN"          () ()))
		 (L03 () (leq (absval lsigma) epsilonsigma)                        ("OPEN"          () ()))
		 (L04 () (geq e1sigma (times 2 (div epsilonsigma M)))              ("OPEN"          () ()))
		 
		 (Lthm () (greater (absval B) epsilon)                             ("solved"       () (L00 L01 L02 L03 L04)))
		 ))

;; Idee: Wenn |k|=>M und |A|>e1 und e1=>2*(epsilon/M), dann ist |K*A|>M*e1=>M*2*(epsilon/M)=2*epsilon
;;       Wenn nun noch |L|<=epsilon, dann ist |K*A + L| > epsilon (da im schlimmestnen Falle K*A -L > epsilon)




(infer~defmethod MSumEstimateI<-m
		 (outline-mappings (((existent
				      existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     MSumEstimateI<-m-b))))

(meth~defmethod MSumEstimateI<-m-b MSumEstimateI<-m
		;;NOTE: *******************************
		;;  the application of solve*< is integrated into this version of CE<=
		;;**************************************
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (a num term) (b num term) (epsilon num term) (e1 num term)
		  (l num term) (k num term)
		  (asigma num term) (bsigma num term) (epsilonsigma num term) (e1sigma num term) (ksigma num term) (lsigma num term) 
		  (m num metavar)
		  (sigma o sub)
		  (pos o pos)
		  (pos2 o pos)
		  (the-cas o symbol)
		  (eqprems o prlnlist)
		  (=conjunct o term)
		  (rel1 (o num num) term)
		  (rel2 (o num num) term)
		  (rel3 (o num num) term)
		  (rel4 (o num num) term)
		  (rel5 (o num num) term)
		  ))
		
		(premises LS1 (+ L01) (+ LS2) (+ L03) (+ L04) (+ L05) (+ LS0))
		(conclusions (- Lthm))
		
		(application-condition
		 (mand
		  
		  ;; a, b are no absval terms
		  (mnot (mand (appl-p b)
			       (mequal (applfunc b) (:term absval))
			       ))
		  (mnot (mand (appl-p a)
			       (mequal (applfunc a) (:term absval))
			       ))

		  ;; rel1, rel2, rel3, rel4 correspond to each other
		  (mor (mand (mequal rel1 (:term less))                       ;; if rel1 is < 
			     (mor (mand (mequal rel2 (:term leq))             ;; and rel2 is <= then rel3 is < 
					(mbind rel3 (:term less)))
				  (mand (mequal rel2 (:term geq))             ;; and rel2 is >= then rel3 is >
					(mbind rel3 (:term greater)))
				  (mand (mequal rel2 (:term less))            ;; and rel2 is < then rel3 is <=
					(mbind rel3 (:term leq)))
				  (mand (mequal rel2 (:term greater))         ;; and rel2 is > then rel3 is >=
					(mbind rel3 (:term geq)))))
		       (mand (mequal rel1 (:term leq))                        ;; if rel1 is <=
			     (mor (mand (mor (mequal rel2 (:term leq))        ;; and rel2 is <= or < then rel3 is <=
					     (mequal rel2 (:term less)))
					(mbind rel3 (:term leq)))
				  (mand (mor (mequal rel2 (:term geq))        ;; and rel2 is >= or > then rel3 is >= 
					     (mequal rel2 (:term greater)))     
					(mbind rel3 (:term geq))))))
		  
		  (mor (mand (mor (mequal rel2 (:term greater))             ;; if rel2 is > or >= then rel4 is >=
				  (mequal rel2 (:term geq)))
			     (mbind rel4 (:term geq)))
		       (mand (mor (mequal rel2 (:term less))                ;; if rel2 is > or >= then rel4 is <=
				  (mequal rel2 (:term leq)))
			     (mbind rel4 (:term leq))))
		  
		  (mor (mand (mor (mequal rel2 (:term greater))             ;; if rel2 is > or >= then rel5 is >
				  (mequal rel2 (:term geq)))
			     (mbind rel5 (:term greater)))
		       (mand (mor (mequal rel2 (:term less))                ;; if rel2 is > or >= then rel5 is <
				  (mequal rel2 (:term leq)))
			     (mbind rel5 (:term less))))
		  
		  (getcassubst A B sigma)               ;; get the mystic substitution
		  (mbind Asigma (subst-apply sigma A))  ;; create ASIGMA
		  (casextract Asigma B k l sigma)       ;; compute linear combination
		  
		  (mnot (mequal k (:term 0)))           ;; do not accept the trivial cases
		  (mnot (mand (mequal k (:term 1))      ;; where A = B...
			      (mequal l (:term 0))))

		  ;; if rel2 is less or leq, then k > 0 has to hold
		  ;; if rel2 is greater or geq, then k < 0 has to hold
		  (mor (mand (mor (mequal rel2 (:term less))
				  (mequal rel2 (:term leq)))
			     (mgreater k 0))
		       (mand (mor (mequal rel2 (:term greater))
				  (mequal rel2 (:term geq)))
			     (mnot (mgreater-equal k 0))))
		       
		  
		  (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		  (no-Eigenvars-in (:symbol COSIE) M) ;; the new variable M should not contain any Eigenvars
		  
		  (mbind Bsigma (subst-apply sigma B))
		  (mbind e1sigma (subst-apply sigma e1))
		  (mbind epsilonsigma (subst-apply sigma epsilon))
		  (mbind ksigma (subst-apply sigma k))
		  (mbind lsigma (subst-apply sigma l))
		  )
		 )
		
		(outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 )
				
		(decl-content
		 (LS0 () =conjunct                                              ("OPEN" () ()))

		 (LS1 () (rel2 A e1))
		 
		 (LS2 () (rel4 e1sigma (div epsilonsigma (times 2 M)))          ("OPEN" () ()))
		 (L01 () (rel3 ksigma M)                                        ("OPEN" () ()))

		 (L02 () (rel4 Asigma (div epsilonsigma (times 2 M)))           ("trans<=" () (LS1 LS2)))	
		 (L03 () (leq lsigma (div epsilonsigma 2))                      ("OPEN" () ()))

		 (L04 () (rel5 0 M)                                             ("OPEN" () ()))
		 (L05 () (leq 0 epsilon)                                        ("OPEN" () ()))
		 
		 (L0 () (= Bsigma Bsigma)                                       ("=ref" (B) ()))
		 (L1 () (= Bsigma (plus (times ksigma Asigma) lsigma))          ("cas" (pos the-cas) (L0)))
		 
		 (Lthm () (rel1 B epsilon)                                      ("solved" () (l1)))
		 )
		)

#|
Idea of this method:
We want to show that B < epsilon
We have that A < e1 or A > e1
If B = k*A+l
Then we allow two cases:
Case1: The premise is A < e1 and k > 0 (checked in Method) and 0 < epsilon (goal)

       We show that the following subgoals hold:e1 < epsilon/2*M
                                                 k < M
                                                 0 < M
                                                 l < epsilon/2

                      A < e1 < epsilon/2*M
        =>            A < epsilon/(2*M)                       (by transitivity of <)   
        =>          A*k < epsilon/(2*M) * k                   (holds since k > 0)

                          k < M  
        =>  epsilon/(2*M)*k < epsilon/(2*M)*M                 since M>0 and epsilon>0 and hence epsilon/(2*M)>0

        =>          A*k < epsilon/(2*M)*M=epsilon/2           (by transitivity of <)
        =>        A*k+l < epsilon/2+l                         (adding l two both sides)

                      l < epsilon/2
        =>  epsilon/2+l < epsilon/2+epsilon/2                 (adding l epsilon/2 to both sides)

        =>        A*k+l < epsilon/2+epsilon/2                 (by transitivity of <)
        =>          B   < epsilon



Case2: The premise is A > e1 and k < 0 (checked in Method) and 0 < epsilon (goal)

       We show that the following subgoals hold:e1 > epsilon/2*M
                                                 k > M
                                                 0 > M
                                                 l < epsilon/2
                      A > e1 > epsilon/2*M
        =>            A > epsilon/(2*M)                       (by transitivity of >)   
        =>          A*k < epsilon/(2*M) * k                   (holds since k < 0)

                          k > M  
        =>  epsilon/(2*M)*k < epsilon/(2*M)*M                 since M<0 and epsilon>0 and hence epsilon/(2*M)<0

        =>          A*k < epsilon/(2*M)*M=epsilon/2           (by transitivity of <)
        =>        A*k+l < epsilon/2+l                         (adding l two both sides)

                      l < epsilon/2
        =>  epsilon/2+l < epsilon/2+epsilon/2                 (adding l epsilon/2 to both sides)

        =>        A*k+l < epsilon/2+epsilon/2                 (by transitivity of <)
        =>          B   < epsilon
|#





(infer~defmethod MSumEstimateII<-m
		 (outline-mappings (((existent
				      existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     MSumEstimateII<-m-b))))

(meth~defmethod MSumEstimateII<-m-b MSumEstimateII<-m
		;;NOTE: *******************************
		;;  the application of solve*< is integrated into this version of CE<=
		;;**************************************
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (a num term) (b num term) (epsilon num term) (e1 num term)
		  (l num term) (k num term)
		  (asigma num term) (bsigma num term) (epsilonsigma num term) (e1sigma num term) (ksigma num term) (lsigma num term) 
		  (m num metavar)
		  (sigma o sub)
		  (pos o pos)
		  (pos2 o pos)
		  (the-cas o symbol)
		  (eqprems o prlnlist)
		  (=conjunct o term)
		  (rel1 (o num num) term)
		  (rel2 (o num num) term)
		  (rel3 (o num num) term)
		  (rel4 (o num num) term)
		  (rel5 (o num num) term)
		  ))
		
		(premises LS1 (+ L01) (+ LS2) (+ L03) (+ L04) (+ L05) (+ LS0))
		(conclusions (- Lthm))
		
		(application-condition
		 (mand
		  
		  ;; a, b are no absval terms
		  (mnot (mand (appl-p b)
			       (mequal (applfunc b) (:term absval))
			       ))
		  (mnot (mand (appl-p a)
			       (mequal (applfunc a) (:term absval))
			       ))

		  ;; rel1, rel2, rel3, rel4 correspond to each other
		  (mor (mand (mequal rel1 (:term less))                       ;; if rel1 is < 
			     (mor (mand (mequal rel2 (:term leq))             ;; and rel2 is <= then rel3 is > 
					(mbind rel3 (:term greater)))
				  (mand (mequal rel2 (:term geq))             ;; and rel2 is >= then rel3 is <
					(mbind rel3 (:term less)))
				  (mand (mequal rel2 (:term less))            ;; and rel2 is < then rel3 is >=
					(mbind rel3 (:term geq)))
				  (mand (mequal rel2 (:term greater))         ;; and rel2 is > then rel3 is <=
					(mbind rel3 (:term leq)))))
		       (mand (mequal rel1 (:term leq))                        ;; if rel1 is <=
			     (mor (mand (mor (mequal rel2 (:term leq))        ;; and rel2 is <= or < then rel3 is >=
					     (mequal rel2 (:term less)))
					(mbind rel3 (:term geq)))
				  (mand (mor (mequal rel2 (:term geq))        ;; and rel2 is >= or > then rel3 is <= 
					     (mequal rel2 (:term greater)))     
					(mbind rel3 (:term leq))))))
		  
		  (mor (mand (mor (mequal rel2 (:term greater))             ;; if rel2 is > or >= then rel4 is >=
				  (mequal rel2 (:term geq)))
			     (mbind rel4 (:term geq)))
		       (mand (mor (mequal rel2 (:term less))                ;; if rel2 is > or >= then rel4 is <=
				  (mequal rel2 (:term leq)))
			     (mbind rel4 (:term leq))))
		  
		  (mor (mand (mor (mequal rel2 (:term greater))             ;; if rel2 is > or >= then rel5 is >
				  (mequal rel2 (:term geq)))
			     (mbind rel5 (:term greater)))
		       (mand (mor (mequal rel2 (:term less))                ;; if rel2 is > or >= then rel5 is <
				  (mequal rel2 (:term leq)))
			     (mbind rel5 (:term less))))
		  
		  (getcassubst A B sigma)               ;; get the mystic substitution
		  (mbind Asigma (subst-apply sigma A))  ;; create ASIGMA
		  (casextract Asigma B k l sigma)       ;; compute linear combination
		  
		  (mnot (mequal k (:term 0)))           ;; do not accept the trivial cases
		  (mnot (mand (mequal k (:term 1))      ;; where A = B...
			      (mequal l (:term 0))))

		  ;; if rel2 is less or leq, then k > 0 has to hold
		  ;; if rel2 is greater or geq, then k < 0 has to hold
		  (mor (mand (mor (mequal rel2 (:term less))
				  (mequal rel2 (:term leq)))
			     (mgreater k 0))
		       (mand (mor (mequal rel2 (:term greater))
				  (mequal rel2 (:term geq)))
			     (mnot (mgreater-equal k 0))))
		       
		  
		  (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		  (no-Eigenvars-in (:symbol COSIE) M) ;; the new variable M should not contain any Eigenvars
		  
		  (mbind Bsigma (subst-apply sigma B))
		  (mbind e1sigma (subst-apply sigma e1))
		  (mbind epsilonsigma (subst-apply sigma epsilon))
		  (mbind ksigma (subst-apply sigma k))
		  (mbind lsigma (subst-apply sigma l))
		  )
		 )
		
		(outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 )
				
		(decl-content
		 (LS0 () =conjunct                                              ("OPEN" () ()))
		 (LS1 () (rel2 A e1))
		 (LS2 () (rel4 e1sigma (div epsilonsigma (times 2 M)))          ("OPEN" () ()))
		 (L01 () (rel3 ksigma M)                                        ("OPEN" () ()))

		 (L02 () (rel4 Asigma (div epsilonsigma (times 2 M)))           ("trans<=" () (LS1 LS2)))	

		 (L03 () (leq lsigma (div epsilonsigma 2))                      ("OPEN" () ()))
		 (L04 () (rel5 0 M)                                             ("OPEN" () ()))
		 (L05 () (geq 0 epsilon)                                        ("OPEN" () ()))

		 (L0 () (= Bsigma Bsigma)                                       ("=ref" (B) ()))
		 (L1 () (= Bsigma (plus (times ksigma Asigma) lsigma))          ("cas" (pos the-cas) (L0)))

		 (Lthm () (rel1 B epsilon)                                      ("solved" () (l1)))
		 )
		)


#|
Idea of this method:
We want to show that B < epsilon
We have that A < e1 or A > e1
If B = k*A+l
Then we allow two cases:

Case1: The premise is A < e1 and k > 0 (checked in Method) and epsilon < 0 (goal)

       We show that the following subgoals hold: e1 < epsilon/2*M
                                                  k > M
                                                  0 < M
                                                  l < epsilon/2

                      A < e1 < epsilon/2*M
        =>            A < epsilon/(2*M)                       (by transitivity of <)   
        =>          A*k < epsilon/(2*M) * k                   (holds since k > 0)

                          k > M  
        =>  epsilon/(2*M)*k < epsilon/(2*M)*M                 since M>0 and epsilon<0 and hence epsilon/(2*M)<0

        =>          A*k < epsilon/(2*M)*M=epsilon/2           (by transitivity of <)
        =>        A*k+l < epsilon/2+l                         (adding l two both sides)

                      l < epsilon/2
        =>  epsilon/2+l < epsilon/2+epsilon/2                 (adding l epsilon/2 to both sides)

        =>        A*k+l < epsilon/2+epsilon/2                 (by transitivity of <)
        =>          B   < epsilon



Case2: The premise is A > e1 and k < 0 (checked in Method) and epsilon < 0 (goal)

       We show that the following subgoals hold: e1 > epsilon/2*M
                                                  k < M
                                                  0 > M
                                                  l < epsilon/2
                      A > e1 > epsilon/2*M
        =>            A > epsilon/(2*M)                       (by transitivity of >)   
        =>          A*k < epsilon/(2*M) * k                   (holds since k < 0)

                          k < M  
        =>  epsilon/(2*M)*k < epsilon/(2*M)*M                 since M<0 and epsilon<0 and hence epsilon/(2*M)>0

        =>          A*k < epsilon/(2*M)*M=epsilon/2           (by transitivity of <)
        =>        A*k+l < epsilon/2+l                         (adding l two both sides)

                      l < epsilon/2
        =>  epsilon/2+l < epsilon/2+epsilon/2                 (adding l epsilon/2 to both sides)

        =>        A*k+l < epsilon/2+epsilon/2                 (by transitivity of <)
        =>          B   < epsilon
|#









(infer~defmethod MSumEstimateI>-m
		 (outline-mappings (((existent
				      existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     MSumEstimateI>-m-b))))

(meth~defmethod MSumEstimateI>-m-b MSumEstimateI>-m
		;;NOTE: *******************************
		;;  the application of solve*< is integrated into this version of CE<=
		;;**************************************
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (a num term) (b num term) (epsilon num term) (e1 num term)
		  (l num term) (k num term)
		  (asigma num term) (bsigma num term) (epsilonsigma num term) (e1sigma num term) (ksigma num term) (lsigma num term) 
		  (m num metavar)
		  (sigma o sub)
		  (pos o pos)
		  (pos2 o pos)
		  (the-cas o symbol)
		  (eqprems o prlnlist)
		  (=conjunct o term)
		  (rel1 (o num num) term)
		  (rel2 (o num num) term)
		  (rel3 (o num num) term)
		  (rel4 (o num num) term)
		  (rel5 (o num num) term)
		  ))
		
		(premises LS1 (+ L01) (+ LS2) (+ L03) (+ L04) (+ L05) (+ LS0))
		(conclusions (- Lthm))
		
		(application-condition
		 (mand
		  
		  ;; a, b are no absval terms
		  (mnot (mand (appl-p b)
			       (mequal (applfunc b) (:term absval))
			       ))
		  (mnot (mand (appl-p a)
			       (mequal (applfunc a) (:term absval))
			       ))

		  ;; rel1, rel2, rel3, rel4 correspond to each other
		  (mor (mand (mequal rel1 (:term greater))                    ;; if rel1 is >
			     (mor (mand (mequal rel2 (:term leq))             ;; and rel2 is <= then rel3 is < 
					(mbind rel3 (:term less)))
				  (mand (mequal rel2 (:term geq))             ;; and rel2 is >= then rel3 is >
					(mbind rel3 (:term greater)))
				  (mand (mequal rel2 (:term less))            ;; and rel2 is < then rel3 is <=
					(mbind rel3 (:term leq)))
				  (mand (mequal rel2 (:term greater))         ;; and rel2 is > then rel3 is >=
					(mbind rel3 (:term geq)))))
		       (mand (mequal rel1 (:term geq))                        ;; if rel1 is >=
			     (mor (mand (mor (mequal rel2 (:term leq))        ;; and rel2 is <= or < then rel3 is <=
					     (mequal rel2 (:term less)))
					(mbind rel3 (:term leq)))
				  (mand (mor (mequal rel2 (:term geq))        ;; and rel2 is >= or > then rel3 is >= 
					     (mequal rel2 (:term greater)))     
					(mbind rel3 (:term geq))))))
		  
		  (mor (mand (mor (mequal rel2 (:term greater))             ;; if rel2 is > or >= then rel4 is >=
				  (mequal rel2 (:term geq)))
			     (mbind rel4 (:term geq)))
		       (mand (mor (mequal rel2 (:term less))                ;; if rel2 is < or <= then rel4 is <=
				  (mequal rel2 (:term leq)))
			     (mbind rel4 (:term leq))))
		  
		  (mor (mand (mor (mequal rel2 (:term greater))             ;; if rel2 is > or >= then rel5 is <
				  (mequal rel2 (:term geq)))
			     (mbind rel5 (:term less)))
		       (mand (mor (mequal rel2 (:term less))                ;; if rel2 is > or >= then rel5 is >
				  (mequal rel2 (:term leq)))
			     (mbind rel5 (:term greater))))
		  
		  (getcassubst A B sigma)               ;; get the mystic substitution
		  (mbind Asigma (subst-apply sigma A))  ;; create ASIGMA
		  (casextract Asigma B k l sigma)       ;; compute linear combination
		  
		  (mnot (mequal k (:term 0)))           ;; do not accept the trivial cases
		  (mnot (mand (mequal k (:term 1))      ;; where A = B...
			      (mequal l (:term 0))))

		  ;; if rel2 is less or leq, then k < 0 has to hold
		  ;; if rel2 is greater or geq, then k > 0 has to hold
		  (mor (mand (mor (mequal rel2 (:term less))
				  (mequal rel2 (:term leq)))
			     (mnot (mgreater-equal k 0)))
		       (mand (mor (mequal rel2 (:term greater))
				  (mequal rel2 (:term geq)))
			     (mgreater k 0)))		       
		  
		  (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		  (no-Eigenvars-in (:symbol COSIE) M) ;; the new variable M should not contain any Eigenvars
		  
		  (mbind Bsigma (subst-apply sigma B))
		  (mbind e1sigma (subst-apply sigma e1))
		  (mbind epsilonsigma (subst-apply sigma epsilon))
		  (mbind ksigma (subst-apply sigma k))
		  (mbind lsigma (subst-apply sigma l))
		  )
		 )
		
		(outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 )
				
		(decl-content
		 (LS0 () =conjunct                                              ("OPEN" () ()))

		 (LS1 () (rel2 A e1))
		 
		 (LS2 () (rel4 e1sigma (div (times 2 epsilonsigma) M))          ("OPEN" () ()))
		 
		 (L01 () (rel3 ksigma M)                                        ("OPEN" () ()))

		 (L02 () (rel4 Asigma (div (times 2 epsilonsigma) M))           ("trans>=" () (LS1 LS2)))	

		 (L03 () (geq lsigma (minus 0 epsilon))                         ("OPEN" () ()))
		 
		 (L04 () (rel5 0 M)                                             ("OPEN" () ()))

		 (L05 () (leq 0 epsilon)                                        ("OPEN" () ()))
		 
		 (L0 () (= Bsigma Bsigma)                                       ("=ref" (B) ()))
		 (L1 () (= Bsigma (plus (times ksigma Asigma) lsigma))          ("cas" (pos the-cas) (L0)))
		 
		 (Lthm () (rel1 B epsilon)                                      ("solved" () (l1)))
		 )
		)







#|
Idea of this method:
We want to show that B > epsilon
We have that A > e1 or A < e1
If B = k*A+l
Then we allow two cases:
Case1: The premise is A > e1 and k > 0 (checked in Method) and 0 < epsilon (goal)

       We show that the following subgoals hold:e1 > 2*epsilon/M
                                                 k > M
                                                 0 < M
                                                 l > - epsilon

                      A > e1 > 2*epsilon/M
        =>            A > 2*epsilon/M                         (by transitivity of >)   
        =>          A*k > 2*epsilon/M * k                     (holds since k > 0)

                          k > M  
        =>  2*epsilon/M * k > 2*epsilon/M * M                 since M>0 and epsilon>0 and hence 2*epsilon/M>0

        =>          A*k > 2*epsilon/M * M =2*epsilon          (by transitivity of >)
        =>        A*k+l > 2*epsilon+l                         (adding l two both sides)

                      l > -epsilon
        =>  2*epsilon+l > 2*epsilon-epsilon                   (adding 2*epsilon to both sides)

        =>        A*k+l > 2*epsilon-epsilon                   (by transitivity of <)
        =>          B   > epsilon



Case2: The premise is A < e1 and k < 0 (checked in Method) and 0 < epsilon (goal)

       We show that the following subgoals hold: 0 < epsilon
                                                e1 < 2*epsilon/M
                                                 k < M
                                                 0 > M
                                                 l > -epsilon
                      A < e1 < 2*epsilon/M
        =>            A < 2*epsilon/M                         (by transitivity of >)   
        =>          A*k > 2*epsilon/M * k                     (holds since k < 0)


                          k < M  
        =>  2*epsilon/M * k > 2*epsilon/M * M                 since M<0 and epsilon>0 and hence 2*epsilon/M<0

        =>          A*k > 2*epsilon/M * M =2*epsilon          (by transitivity of >)
        =>        A*k+l > 2*epsilon+l                         (adding l two both sides)

                      l > -epsilon
        =>  2*epsilon+l > 2*epsilon-epsilon                   (adding 2*epsilon to both sides)

        =>        A*k+l > 2*epsilon-epsilon                   (by transitivity of <)
        =>          B   > epsilon


|#








(infer~defmethod MSumEstimateII>-m
		 (outline-mappings (((existent
				      existent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent
				      nonexistent)
				     MSumEstimateII>-m-b))))

(meth~defmethod MSumEstimateII>-m-b MSumEstimateII>-m
		;;NOTE: *******************************
		;;  the application of solve*< is integrated into this version of CE<=
		;;**************************************
		(in limit) 
		(rating 50)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (a num term) (b num term) (epsilon num term) (e1 num term)
		  (l num term) (k num term)
		  (asigma num term) (bsigma num term) (epsilonsigma num term) (e1sigma num term) (ksigma num term) (lsigma num term) 
		  (m num metavar)
		  (sigma o sub)
		  (pos o pos)
		  (pos2 o pos)
		  (the-cas o symbol)
		  (eqprems o prlnlist)
		  (=conjunct o term)
		  (rel1 (o num num) term)
		  (rel2 (o num num) term)
		  (rel3 (o num num) term)
		  (rel4 (o num num) term)
		  (rel5 (o num num) term)
		  ))
		
		(premises LS1 (+ L01) (+ LS2) (+ L03) (+ L04) (+ L05) (+ LS0))
		(conclusions (- Lthm))
		
		(application-condition
		 (mand
		  
		  ;; a, b are no absval terms
		  (mnot (mand (appl-p b)
			       (mequal (applfunc b) (:term absval))
			       ))
		  (mnot (mand (appl-p a)
			       (mequal (applfunc a) (:term absval))
			       ))

		  ;; rel1, rel2, rel3, rel4 correspond to each other
		  (mor (mand (mequal rel1 (:term greater))                    ;; if rel1 is >
			     (mor (mand (mequal rel2 (:term leq))             ;; and rel2 is <= then rel3 is <
					(mbind rel3 (:term less)))
				  (mand (mequal rel2 (:term geq))             ;; and rel2 is >= then rel3 is >
					(mbind rel3 (:term greater)))
				  (mand (mequal rel2 (:term less))            ;; and rel2 is < then rel3 is <=
					(mbind rel3 (:term leq)))
				  (mand (mequal rel2 (:term greater))         ;; and rel2 is > then rel3 is >=
					(mbind rel3 (:term geq)))))
		       (mand (mequal rel1 (:term geq))                        ;; if rel1 is >=
			     (mor (mand (mor (mequal rel2 (:term leq))        ;; and rel2 is <= or < then rel3 is <=
					     (mequal rel2 (:term less)))
					(mbind rel3 (:term leq)))
				  (mand (mor (mequal rel2 (:term geq))        ;; and rel2 is >= or > then rel3 is >= 
					     (mequal rel2 (:term greater)))     
					(mbind rel3 (:term geq))))))
		  
		  (mor (mand (mor (mequal rel2 (:term greater))             ;; if rel2 is > or >= then rel4 is >=
				  (mequal rel2 (:term geq)))
			     (mbind rel4 (:term geq)))
		       (mand (mor (mequal rel2 (:term less))                ;; if rel2 is < or <= then rel4 is <=
				  (mequal rel2 (:term leq)))
			     (mbind rel4 (:term leq))))
		  
		  (mor (mand (mor (mequal rel2 (:term greater))             ;; if rel2 is > or >= then rel5 is <
				  (mequal rel2 (:term geq)))
			     (mbind rel5 (:term less)))
		       (mand (mor (mequal rel2 (:term less))                ;; if rel2 is < or <= then rel5 is >
				  (mequal rel2 (:term leq)))
			     (mbind rel5 (:term greater))))
		  
		  (getcassubst A B sigma)               ;; get the mystic substitution
		  (mbind Asigma (subst-apply sigma A))  ;; create ASIGMA
		  (casextract Asigma B k l sigma)       ;; compute linear combination
		  
		  (mnot (mequal k (:term 0)))           ;; do not accept the trivial cases
		  (mnot (mand (mequal k (:term 1))      ;; where A = B...
			      (mequal l (:term 0))))

		  ;; if rel2 is less or leq, then k < 0 has to hold
		  ;; if rel2 is greater or geq, then k > 0 has to hold
		  (mor (mand (mor (mequal rel2 (:term less))
				  (mequal rel2 (:term leq)))
			     (mnot (mgreater-equal k 0)))
		       (mand (mor (mequal rel2 (:term greater))
				  (mequal rel2 (:term geq)))
			     (mgreater k 0)))		       
		  
		  (mbind M (newmetavar (:symbol :M) (termtype epsilon)))
		  (no-Eigenvars-in (:symbol COSIE) M) ;; the new variable M should not contain any Eigenvars
		  
		  (mbind Bsigma (subst-apply sigma B))
		  (mbind e1sigma (subst-apply sigma e1))
		  (mbind epsilonsigma (subst-apply sigma epsilon))
		  (mbind ksigma (subst-apply sigma k))
		  (mbind lsigma (subst-apply sigma l))
		  )
		 )
		
		(outline-computations
		 (=conjunct (subst-to-conjunct sigma))
		 )
				
		(decl-content
		 (LS0 () =conjunct                                              ("OPEN" () ()))

		 (LS1 () (rel2 A e1))
		 
		 (LS2 () (rel4 e1sigma (div (minus 0 epsilonsigma) M))          ("OPEN" () ()))
		 
		 (L01 () (rel3 ksigma M)                                        ("OPEN" () ()))
		 
		 (L02 () (rel4 Asigma (div (minus 0 epsilonsigma) M))           ("trans>=" () (LS1 LS2)))	
		 
		 (L03 () (geq lsigma (times 2 epsilon))                         ("OPEN" () ()))
		 
		 (L04 () (rel5 0 M)                                             ("OPEN" () ()))

		 (L05 () (geq 0 epsilon)                                        ("OPEN" () ()))
		 
		 (L0 () (= Bsigma Bsigma)                                       ("=ref" (B) ()))
		 (L1 () (= Bsigma (plus (times ksigma Asigma) lsigma))          ("cas" (pos the-cas) (L0)))
		 
		 (Lthm () (rel1 B epsilon)                                      ("solved" () (l1)))
		 )
		)


#|
Idea of this method:
We want to show that B > epsilon
We have that A > e1 or A < e1
If B = k*A+l

Case1: The premise is A > e1 and k > 0 (checked in Method) and 0 > epsilon (goal)

       We show that the following subgoals hold:e1 > -epsilon/M
                                                 k > M
                                                 0 < M
                                                 l > 2*epsilon

                      A > e1 > -epsilon/M
        =>            A > -epsilon/M                          (by transitivity of >)   
        =>          A*k > -epsilon/M * k                      (holds since k > 0)

                          k > M  
        =>   -epsilon/M * k > -epsilon/M * M                  since M>0 and epsilon<0 and hence -epsilon/M>0

        =>          A*k > -epsilon/M * M = -epsilon           (by transitivity of >)
        =>        A*k+l > -epsilon+l                          (adding l two both sides)

                      l > 2*epsilon
        => -epsilon + l > -epsilon + 2*epsilon                (adding -epsilon to both sides)

        =>        A*k+l > -epsilon + 2*epsilon                (by transitivity of >)
        =>          B   > epsilon


Case1: The premise is A < e1 and k < 0 (checked in Method) and 0 > epsilon (goal)

       We show that the following subgoals hold:e1 < -epsilon/M
                                                 k < M
                                                 0 > M
                                                 l > 2*epsilon

                      A < e1 < -epsilon/M
        =>            A < -epsilon/M                          (by transitivity of >)   
        =>          A*k > -epsilon/M * k                      (holds since k > 0)

                          k < M  
        =>   -epsilon/M * k > -epsilon/M * M                  since M<0 and epsilon<0 and hence -epsilon/M<0

        =>          A*k > -epsilon/M * M = -epsilon           (by transitivity of >)
        =>        A*k+l > -epsilon+l                          (adding l two both sides)

                      l > 2*epsilon
        => -epsilon + l > -epsilon + 2*epsilon                (adding -epsilon to both sides)

        =>        A*k+l > -epsilon + 2*epsilon                (by transitivity of >)
        =>          B   > epsilon
|#



#|

;; isolates parts of inequalities by calling MAPLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ISOLATE METHOD


(meth~defcond isolate-position (args cmapp)
	      (declare (edited  "18-FEB-2002" )
		       (authors Ameier)
		       (input   "A list of arguments and a cmapping. The arguments are a proof line whose formula is an"
				"inequality, a position in the proof line, and a meta-variable.") 
		       (effect  "None.")
		       (value   "<T, cmapp'> iff the term at the position in the proof line can be isolated via calling MAPLE."
				"Then, cmapp' results from cmapp by adding a binding of the meta-variable to the resulting"
				"inequality."
				"<NIL, cmapp>, otherwise."))

	      (let* ((line (first args))
		     (position (second args))
		     (meta-var (third args))
		     (new-inequality (methhelp=isolate-with-maple (node~formula line) position)))

		(if new-inequality
		    (meth~mapp-extend-mapp cmapp meta-var new-inequality)
		  (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'isolate-position)

(defun methhelp=isolate-with-maple (formula position)
  (let* ((term-at-position (data~struct-at-position formula position))
	 (new-term-const (term~constant-create 'isoconst (term~type term-at-position)))
	 (new-formula (data~replace-at-position formula position new-term-const))
	 (formula-string (string-downcase (post~string new-formula)))
	 (maple-result (maple~call-maple (list "solve" formula-string "{isoconst}") :syntax 'post2post)))

    (if (string-equal maple-result 'error)
	nil
      (progn
	(env~enter 'isoconst new-term-const (pds~environment omega*current-proof-plan))

	(let* ((streami (make-string-input-stream maple-result))
	       (output (read streami))
	       (maple-formula (ignore-errors (post~read-object (first output) (pds~environment omega*current-proof-plan) :existing-term))))
	  (if maple-formula
	      (let* ((switched-formula (methhelp=switch-isolate-left maple-formula new-term-const)))

		(env~remove 'isoconst (pds~environment omega*current-proof-plan))
		(data~replace-struct switched-formula new-term-const term-at-position))
	    (progn
	      (env~remove 'isoconst (pds~environment omega*current-proof-plan))
	      nil)))))))

	
(defun methhelp=switch-isolate-left (formula isoconst)
  (let* ((func (data~appl-function formula))
	 (args (data~appl-arguments formula))
	 (env (pds~environment omega*current-proof-plan)))
    (if (keim~equal isoconst (first args))
	formula
      (term~appl-create (cond ((keim~equal func (env~lookup-object 'less env))
			       (env~lookup-object 'greater env))
			      ((keim~equal func (env~lookup-object 'leq env))
			       (env~lookup-object 'geq env))
			      ((keim~equal func (env~lookup-object 'greater env))
			       (env~lookup-object 'less env))
			      ((keim~equal func (env~lookup-object 'geq env))
			       (env~lookup-object 'leq env)))
			(reverse args)))))



(infer~defmethod isolate-m
		 (outline-mappings (((existent nonexistent) isolate-m-b))
				   (((nonexistent existent) isolate-m-f)))
		 (parameter-types position)
		 (help "Isolates subterms in inequalities."))

(meth~defmethod isolate-m-b isolate-m
		(in limit)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (rel (o num num) term)
		  (a num term) (b num term)
		  (phi o term)
		  (posisolate o pos)
		  )
		 )
		
		(parameters posisolate)
		
		(application-condition
		 (mand (bound-p posisolate)
		       (mor (mequal rel (:term leq))
			    (mequal rel (:term less))
			    (mequal rel (:term greater))
			    (mequal rel (:term geq)))
		       (isolate-position l1 posisolate phi)
		       ))
		
		(premises (+ l2))
		(conclusions (- l1))
		
		(decl-content
		 (l2 () phi         ("Open" () ()))
		 (l1 () (rel a b)   ("isolate" () (l2)))   
		 )
		)


(meth~defmethod isolate-m-f isolate-m
		(in limit)
		(rating 60)
		(reasoning :planning :middle-out)
		(declarations
		 (sorted-meta-variables
		  (rel (o num num) term)
		  (a num term) (b num term)
		  (phi o term)
		  (posisolate o pos)
		  )
		 )
		
		(parameters posisolate)
		
		(application-condition
		 (mand (bound-p posisolate)
		       (mor (mequal rel (:term leq))
			    (mequal rel (:term less))
			    (mequal rel (:term greater))
			    (mequal rel (:term geq)))
		       (isolate-position l1 posisolate phi)
		       ))
				 
		(premises (- l1))
		(conclusions (+ l2))
		
		(decl-content
		 (l1 () (rel a b)   )
		 (l2 () phi         ("isolate" () (l1)))  
		 )
		)

|#
