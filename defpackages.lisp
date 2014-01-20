

(in-package :cl-user)
(defpackage constant
  (:use :cl)
  (:export 
	:+IMPL+
	:+NEG+
	:+AND+
	:+OR+
	:+EQ+
	:+FORALL+
	:+EXISTS+
	:+OPERATOR+
	:+QUANTS+
	))
(in-package :constant)
(load "./constant.lisp")





(in-package :cl-user)

(ql:quickload :optima)

(defpackage methods
  (:use :cl
		:optima
		:constant)
  (:export
	:vterm
	:fterm
	:atomic-lexpr
	:operator
	:normal-lexpr
	:quant
	:quantsp
	:lexpr

	:term->string
	:lexpr->string
	:opr->string
	:opr->strength
	:opr-strong?
	:opr-equal?
	:quant->string
	:quantsp->string
	))
(in-package :methods)
(load "./methods.lisp")


