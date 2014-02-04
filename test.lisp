

(ql:quickload :flexpr)



(defun ft (&rest arg)
  (apply #'struct:fterm arg))

(defun vt (&rest arg)
  (apply #'struct:vterm arg))

(defun al (&rest arg)
  (apply #'struct:atomic-lexpr arg))


(print 
  (dump:lexpr->string
	(util::remove-disuse-quant 
	  (print 
		(parser:string->lexpr
		"Ax.(Ay.(P(y) > Q(w) & R(z)))")))))
