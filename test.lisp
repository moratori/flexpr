

(ql:quickload :flexpr)



(defun ft (&rest arg)
  (apply #'flexpr.struct:fterm arg))

(defun vt (&rest arg)
  (apply #'flexpr.struct:vterm arg))

(defun al (&rest arg)
  (apply #'flexpr.struct:atomic-lexpr arg))


;; AxEy.(Q(x,y) & P(x)) = Ax.(P(x) & Ey.Q(x,y))
;; Ax.~P(x) > (AxEy.(P(x,y) > Ey.Q(y))) = EaAbEcEd.(P(a) V ~P(b,c) V Q(d))
(print (flexpr.dump::lexpr->string 
		 (flexpr.formalize:formalize 
		 (flexpr.parser::string->lexpr 
		   "Ax.~P(x) > (AxEy.(P(x,y) > Ey.Q(y)))"))))




(print (flexpr.parser:string->lexpr "Ax.P(x)"))
