

(ql:quickload :flexpr)
(ql:quickload :lisp-unit)
(use-package :lisp-unit)

(import '(flexpr.constant:+FORALL+
		  flexpr.constant:+EXISTS+))

(defvar *and* (flexpr.struct:operator flexpr.constant:+AND+))
(defvar *or* (flexpr.struct:operator flexpr.constant:+OR+))


;; テストデータ作るのが死ぬほど大変なので
;; 意味論的に同値であるかを判定する機能つけなきゃやってられない
;; => けど述語論理に対応できなくなる
;; => けど今でもPLのテストはしてない
(defvar *formalize-test-data*
  '(("P - Q > P" nil "(P & ~Q) V (Q & ~P) V P" 
	 			 nil "(P & ~Q) V (Q & ~P) V (P)")
	("P V ~Q > R" "(R V ~P) & (R V Q)" nil
	 			  "(R V ~P) & (R V Q)" nil)
	("(R > (P V ~Q)) V (Q > R)" "~R V P V ~Q V ~Q V R" "~R V P V ~Q V ~Q V R"
	 							"" "(~R) V (P) V (~Q) V (R)")
	("P V Q V R" "P V Q V R" "P V Q V R"
	 			 "(P V Q V R)" "(P) V (Q) V (R)")
	("P & Q & R" "P & Q & R" "P & Q & R"
	 			 "(P) & (Q) & (R)" "(P & Q & R)")
	("((~P V Q) & R) > (P V ~Q)" "(~R V P V ~Q V P) & (~R V P V ~Q V ~Q)" nil
	 							 "(P V ~R V ~Q)" nil)
	("(P & ~Q) - (P > Q)" "(~P V Q V ~P V Q) & (P V P) & (P V ~Q) & (~Q V P) & (~Q V ~Q)" 
	 nil  "(~P V Q) & (P) & (~Q V P) & (~Q)" nil)

	("(T & T) V R" "(R V T) & (R V T)" "(T & T) V R" 
	 				"(R V T)" "(T) V (R)")

	)
  "(test formal-and-expected  formal-or-expected
		 convert-and-expected convert-or-expected)"
  )


(defvar *resolution-test-data*
  '(
	(()
	 "P V ~P")
	(() 
	 "((P > Q) > P) > P")
	(()
	 "P - P")
	(()
	 "(~P V Q) - ~(P & ~Q)")
	(()
	 "(P & Q) - (Q & P)")
	(()
	 "~(P & ~P)")
	(()
	 "P & (P > Q) > Q")
	(()
	 "~~P - P")
	(()
	 "(~P & (P V Q)) > Q")
	(()
	 "P > (P V Q)")
	(()
	 "P & Q > P")
	(()
	 "(P > Q) - (~Q > ~P)")
	(()
	 "(P > Q) & (Q > R) > (P > R)")
	(()
	 "(P > (Q > R)) > (P & Q > R)")
	(()
	 "~(P > Q) - (P & ~Q)")
	(()
	 "((P > R) & (Q > R)) > (P V Q > R)")
	(()
	 "P > (Q > P)")
	(()
	 "~P > (P > Q)")
	(()
	 "P V P > P")
	(()
	 "P & P > P")
	(()
	 "~Q & (P > Q) > ~P")
	(()
	 "(P > Q) > ((P > R) > (P > (Q & R)))")
	
	;(()
	;"(P > (Q > R)) - (Q > (P > R))")

	(("Ax.(number(x) > number(succ(x)))"
	  "number(Zero)")
	  "number(succ(succ(succ(succ(Zero)))))")

	(("Ax.(human(x) > mortal(x))" 
	  "human(Socrates)") 
	  "mortal(Socrates)")

	(("AxEy.(man(x) & handsome(x) > cute(y) & woman(y) & couple(x,y))"
	  "AxAy.(couple(x,y) > happy(x) & happy(y))"
	  "Ax.(happy(x) > longlife(x))")
	  "Ax.(man(x) & handsome(x) > longlife(x))")

	(("Ax.(have(Jhon,x) > wants(Mike , x))"
	  "have(Jhon,Car) & have(Jhon,Bike)")
	  "wants(Mike,Car) & wants(Mike,Bike)")

	;(("AxAy.(Ez.hate(y,z) > hate(x,y))"
	;  "hate(Jhon,Mike)")
	;  "AxAy.hate(x,y)")

	)
  )


(defun parse (str)
  (flexpr.parser:string->lexpr str))

(defun dump1  (lexpr)
  "convert lexpr to string expression from inner structure.
  lexpr must be atomic-lexpr or normal-lexpr or lexpr"
  (flexpr.dump:lexpr->string lexpr))

(defun dump2 (clause-form op)
  "convert clause form to string expression.
  clause form must be set of set of %literal structure."
  (flexpr.dump:clause-form->string clause-form op))

(defun formal (str op q)
  "convert to prenex normal form end matrix is cnf or dnf"
  (dump1 (flexpr.formalize:formalize (parse str) op q)))

(defun convert (str op q)
  "convert to clause form"
  (dump2 
	(flexpr.formalize:convert (parse str) op q) op))

(defun pl (&rest strings)
  (mapcar #'parse strings))

(define-test formalize-test 
	(dolist (each-case *formalize-test-data*)
	  (destructuring-bind (data fa fo ca co) each-case
		(let ((fa-ans (formal data *and* +FORALL+))
			  (fo-ans (formal data *or*  +FORALL+))
			  (ca-ans (convert data *and* +FORALL+))
			  (co-ans (convert data *or* +FORALL+)))
		  
		      (or (null fa) (assert-equal fa fa-ans))
			  (or (null fo) (assert-equal fo fo-ans))
			  (or (null ca) (assert-equal ca ca-ans))
			  (or (null co) (assert-equal co co-ans))))))

(define-test resolution-test
	(dolist (each *resolution-test-data*)
	  (assert-true 
		(flexpr.infer::resolution 
		  (apply #'pl (first each))
		  (parse (second each))))))


(print-errors (run-tests '(formalize-test)))
(print-errors (run-tests '(resolution-test)))
