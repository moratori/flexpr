

(ql:quickload :flexpr)
(ql:quickload :lisp-unit)
(use-package :lisp-unit)

(defvar *and* (flexpr.struct:operator flexpr.constant:+AND+))
(defvar *or* (flexpr.struct:operator flexpr.constant:+OR+))


;; テストデータ作るのが死ぬほど大変なので
;; 意味論的に同値であるかを判定する機能つけなきゃやってられない
;; => けど述語論理に対応できなくなる
;; => けど今でもPLのテストはしてない
(defvar *test-data*
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

	)
  "(test formal-and-expected  formal-or-expected
		 convert-and-expected convert-or-expected)"
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

(defun formal (str op)
  "convert to prenex normal form end matrix is cnf or dnf"
  (dump1 (flexpr.formalize:formalize (parse str) op)))

(defun convert (str op)
  "convert to clause form"
  (dump2 
	(flexpr.formalize:convert (parse str) op) op))


(define-test formalize-test 
	(dolist (each-case *test-data*)
	  (destructuring-bind (data fa fo ca co) each-case
		(let ((fa-ans (formal data *and*))
			  (fo-ans (formal data *or*))
			  (ca-ans (convert data *and*))
			  (co-ans (convert data *or*)))
		  
		      (or (null fa) (assert-equal fa fa-ans))
			  (or (null fo) (assert-equal fo fo-ans))
			  (or (null ca) (assert-equal ca ca-ans))
			  (or (null co) (assert-equal co co-ans))))))


(print-failures (run-tests '(formalize-test)))


