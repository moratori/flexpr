

(ql:quickload :flexpr)





(print 
  (dump::lexpr->string  
	(parser::string->lexpr "AyEx.(=(y,f(x)))")))

(print 
  (dump::lexpr->string
		 (parser::string->lexpr
		 "Ax1Ax2.(~=(x1,x2) > ~=(f(x1),f(x2)))")))

(print 
  (dump::lexpr->string 
	(parser::string->lexpr
	  "~=(succ(x),0)")))



