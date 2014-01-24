

(load "./defpackages.lisp")

(ql:quickload :lisp-unit)

(defpackage test 
  (:use :cl :lisp-unit
		:constant
		:methods))
(in-package test)




(print (tokenize "P(x) V Q(x) > R(x) & AxAy.(P(x,y) > R(y,x))"))
