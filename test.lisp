

(load "./defpackages.lisp")

(ql:quickload :lisp-unit)

(defpackage test 
  (:use :cl :lisp-unit
		:constant
		:methods))
(in-package test)



(print 
  
(lexpr->string 
  (lexpr 
	(quantsp 
	  (quant +FORALL+ (vterm 'x))
	  (quant +EXISTS+ (vterm 'y)))
	(normal-lexpr
	  (operator +IMPL+)
	  (normal-lexpr (operator +NEG+)
		(atomic-lexpr 'P (vterm 'x) (vterm 'y))
		nil)
	  (lexpr 
		(quantsp 
		  (quant +FORALL+ (vterm 'z)))
		(atomic-lexpr 'R (vterm 'z)))))))
