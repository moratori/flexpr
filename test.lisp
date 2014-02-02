

(ql:quickload :flexpr)





(print 
  (dump:lexpr->string  
	(parser:string->lexpr "AyEx.(=(y,f(x)))")))

(print 
  (dump:lexpr->string
		 (parser:string->lexpr
		 "Ax1Ax2.(~=(x1,x2) > ~=(f(x1),f(x2)))")))

(print 
  (dump:lexpr->string 
	(parser:string->lexpr
	  "~=(succ(x),0)")))


(print 
  (dump:lexpr->string 
	(print 
	  (parser:string->lexpr
	  "p(f(x))"))))



(print 
  (unifier:mgu  (struct:vterm 'C1 t) (struct:vterm 'C2 t)))


(print 
  (util:term= (struct:vterm 'x nil) (struct:vterm 'x nil)))


(print 
  (util:term= (struct:vterm 'x nil)
			 (struct:fterm 'f (struct:vterm 'x nil))))
