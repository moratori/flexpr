

(ql:quickload :flexpr)
(ql:quickload :lisp-unit)
(use-package :lisp-unit)

(defvar *and* (flexpr.struct::operator flexpr.constant::+AND+))
(defvar *or* (flexpr.struct::operator flexpr.constant::+OR+))

(defun parse (str)
  (flexpr.parser::string->lexpr str))

(defun dump (lexpr)
  (flexpr.dump::lexpr->string lexpr))

(defun dump2 (literal &optional (op *and*))
  (flexpr.dump:clause-form->string literal op))

(defun formal (lexpr op)
  (flexpr.formalize:formalize lexpr op))


(defun test1 (str)
  (print (dump (parse str))))

(defun test2 (str &optional (op *and*))
  (print (dump (formal (parse str) op))))

(defun test3 (str &optional (op *and*))
  (flexpr.formalize:convert (parse str) op))

(test2 "(((P(x) & Q(x) > R(x)) V (P(y) > Q(y))) & P(z)) & ~(Q(x) & R(x))")
(test2 "(P & Q) V (R & S) V (T & U)")
(test2 "(((P & Q) V (R & S)) V T) & (((P & Q) V (R & S)) V U)")
(test2 "(P & Q) V (R & S) V (T & U) V (W & X)")
(test2 "(((P & ~Q) V Q) V ~P) & (((~Q & P) V ~P) V Q)")
(test2 "((P > Q) - (~ Q > ~P)) & ((~ P V ~Q) - (~(P & Q)))")
(test2 "P & Q V P  & Q")
(test2 "AxAy.(P(x,y) > (Q(x) - Ez.R(x,y,z)))")
(test2 "Ax.(P(x) & Q(C)) & Ey.(R(y) > S(y)) > AxAy.(P(x) & Q(K))")
(test2 "ExAyEz.(P(x,z) > Q(y,x)) > P(C) & Q(x)")
(test2 "AxAyAzEw.(Ev.(Ax.P(C,V,K)))")

(test2 "~(P V ~P) > ~(Q V P)")
(test2 "(P V Q) - (Q & R)")
(test2 "((P V ~Q) - (Q & ~P))")
(test2 "(P1 & Q1 & R1) V (P2 & Q2 & R2)")
(test2 "(P V B V C) & (P V ~B V C) & (P V B V ~C) & (~P V ~B V C)" *or*)
(test2 "~P > ~Q")
(test2 "AxEy.(P(x) > Q(x,y)) & ~(AxEy.(P(x) > ~Q(x,y)) & Az.R(z))")


(print (dump2 (flexpr.formalize:convert (parse "(P & Q)") *and*)))


