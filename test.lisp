

(ql:quickload :flexpr)


(defun parse (str)
  (flexpr.parser::string->lexpr str))

(defun dump (lexpr)
  (flexpr.dump::lexpr->string lexpr))

(defun formal (lexpr)
  (flexpr.formalize::formalize lexpr))


(defun test (str)
  (dump (formal (parse str))))

;; AxEy.(Q(x,y) & P(x)) = Ax.(P(x) & Ey.Q(x,y))
;; Ax.~P(x) > (AxEy.(P(x,y) > Ey.Q(y))) = EaAbEcEd.(P(a) V ~P(b,c) V Q(d))



(print (test "Ax.~P(x) > (AxEy.(P(x,y) > Ey.Q(y)))"))
(print (test "Ax.(P(x) & Ey.Q(x,y))"))
(print (test "Ax.(man(x) & handsome(x) > Ey.(girl(y) & cute(y) & couple(x,y)))"))


