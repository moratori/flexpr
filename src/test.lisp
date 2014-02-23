

(ql:quickload :flexpr)
(ql:quickload :lisp-unit)
(use-package :lisp-unit)


(defun parse (str)
  (flexpr.parser::string->lexpr str))

(defun dump (lexpr)
  (flexpr.dump::lexpr->string lexpr))

(defun formal (lexpr op)
  (flexpr.formalize:formalize lexpr op))


(defun test1 (str)
  (print (dump (parse str))))

(defun test2 (str &optional (op (flexpr.struct::operator flexpr.constant::+AND+)))
  (print (dump (formal (parse str) op))))


(define-test show
	"test the dump simply"		 
	(let ((data 
			'(("~~Ax.(P(x) > ~Q(x))" . "~~Ax.(P(x) > ~Q(x))")
			  ("~Ax.(P(x) & Q(x)) & ~~Ey.Q(y)" . "~Ax.(P(x) & Q(x)) & ~~Ey.Q(y)")
			  )
			))
	  (dolist (each data)
		(assert-equal (cdr each) (test1 (car each))))))


(define-test formalize
	"most functions (except function which related quantifier process) 
	 can be tested in this test"
	(let ((data 
			'(("~P - Q" . "(P V Q) & (~Q V ~P)")
			  ("P V Q & R" . "(P V Q) & R")
			  ("~(P & Q > R) V (P > Q)" . "(~P V Q V P) & (~P V Q V Q) & (~P V Q V ~R)")
			  ("P & Q > S V R" . "~P V ~Q V S V R")
			  ("((P V Q) & (R V S)) & (T V U)" . "(P V Q) & (R V S) & (T V U)")
			  ("P & (P > Q) > Q" . "(~P V Q V P) & (~P V Q V ~Q)")
			  ("((P > Q) > P) > P" . "(P V ~P V Q) & (P V ~P)")
			  ("~(P & ~P)" . "~P V P")
			  ("(P > Q) - (~Q > ~P)" . "(Q V ~P V P) & (Q V ~P V ~Q) & (~P V Q V ~Q) & (~P V Q V P)")
			  ("P & Q V P & Q" . "(P V P) & (P V Q) & Q")
			  ("((P & Q) V P) & Q" . "(P V P) & (P V Q) & Q")

			  )))
	  (dolist (each data)
		(assert-equal (cdr each) (test2 (car each))))))


(print-failures (run-tests '(show formalize)))


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
(test2 "(P V B V C) & (P V ~B V C) & (P V B V ~C) & (~P V ~B V C)" (flexpr.struct::operator flexpr.constant::+OR+))
(test2 "~P > ~Q")
