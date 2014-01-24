

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
	:+NEG-STR+
	:+FORALL-STR+
	:+EXISTS-STR+
	:+OPERATOR+
	:+QUANTS+
	:+DELIMITER+
	:+PAREN-START+
	:+PAREN-END+
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

	:tokenize
	:term->string
	:lexpr->string
	:opr->string
	:opr->strength
	:opr-strong?
	:opr-equal?
	:quant->string
	:quantsp->string
	:string->lexpr
	:string->quantsp
	:string->atomic
	))
(in-package :methods)
(load "./methods.lisp")


