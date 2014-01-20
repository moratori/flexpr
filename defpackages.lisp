

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
(defpackage types 
  (:use :cl :constant)
  (:export 
	:vterm
	:fterm
	:atomic-lexpr
	:operator
	:normal-lexpr
	:quant
	:quantsp
	:lexpr
	))
(in-package :types)
(load "./types.lisp")



(in-package :cl-user)

(ql:quickload :optima)

(defpackage methods
  (:use :cl
		:optima
		:constant
		:types)
  (:export
	)
  )
(in-package :methods)

