

(ql:quickload :flexpr)



(defun ft (&rest arg)
  (apply #'flexpr.struct:fterm arg))

(defun vt (&rest arg)
  (apply #'flexpr.struct:vterm arg))

(defun al (&rest arg)
  (apply #'flexpr.struct:atomic-lexpr arg))





(print 
  (flexpr.dump:lexpr->string 
	(flexpr.formalize::remove-disuse-quant
	  (flexpr.parser:string->lexpr 
		"AxAyAz.P(w)"))))

(print (flexpr.dump:lexpr->string 
		   (flexpr.formalize::remove-operator
		   (print (flexpr.parser:string->lexpr "P(x) V (Q(x) - R(x))")))))



(print (flexpr.parser:string->lexpr "P(x) V Q(x) V R(x) V S(x)"))



(print 
  (flexpr.dump:lexpr->string 
	(flexpr.formalize::remove-quant-negation 
	  (flexpr.parser:string->lexpr "Ax~~Ey~Aw~Ez.P(x)"))))


