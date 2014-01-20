

(load "./defpackages.lisp")

(ql:quickload :lisp-unit)

(defpackage test 
  (:use :cl :lisp-unit
		:constant
		:methods
		:types))
(in-package test)


