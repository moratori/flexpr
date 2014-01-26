

(defpackage util
  (:use :cl 
		:constant
		:struct)
  (:export 
		:opr->strength
		:opr-strong?
		:opr-equal?))
(in-package util)


(defmethod opr->strength ((opr operator))
  (third  (assoc (operator-opr opr) +OPERATOR+)))


(defmethod opr-strong? ((opr1 operator) (opr2 operator))
  (< (opr->strength opr1)
	 (opr->strength opr2)))


(defmethod opr-equal? ((opr1 operator) (opr2 operator))
  (eq (operator-opr opr1)
	  (operator-opr opr2)))



