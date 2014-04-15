

(ql:quickload :flexpr)
(ql:quickload :lisp-unit)
(use-package :lisp-unit)

(import '(flexpr.system.constant:+FORALL+
		  flexpr.system.constant:+EXISTS+))

(defvar *and* (flexpr.system.struct:operator flexpr.system.constant:+AND+))
(defvar *or* (flexpr.system.struct:operator flexpr.system.constant:+OR+))


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
	
	(()
	"(P > (Q > R)) - (Q > (P > R))")

	(()
	 "P > (Q > (P & Q))")

	(("P & Q"
	  "P > R")
	  "R")

	(("P V Q"
	  "~P")
	  "Q")

	(("P V Q")
	 "~(~P & ~Q)")

	(("P & Q")
	 "~(~P V ~Q)")

	(("((P > Q) > P)")
	 "P")

	(("P")
	 "P")

	(("P & ~P")
	 "Q")

	(("~(P - P)")
	 "Q - R")

	(("P > Q")
	 "~(P & ~Q)")


	(("(P V Q) & (~P V ~Q)")
	 "P - ~Q"
	 )

	(("P - ~Q")
	 "(P V Q) & (~P V ~Q)"
	 )

	(()
	 "(P - ~Q) - ((P V Q) & (~P V ~Q))")


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

	(("AxAy.(Ez.hate(z,y) > hate(y,x))"
	  "hate(Jhon,Mike)")
	  "AxAy.hate(x,y)")

	  (("number(Zero)")
	    "Ex.number(x)")

	  (("Ax.(P(x) V Q(x))"
		"Ax.~Q(x)")
	    "Ax.P(x)")

	  (("AxAy.P(x,y)")
	    "AxAy.P(x,y)")

	  (("Ex.(P(x) & Ay.Q(x,y))")
	    "ExEy.Q(x,y)")

	  (()
	   "AxAyEz.P(x,y,z) > AxEyEz.P(x,y,z)")

	  (()
	   "Ax.P(x) > Ex.P(x)")

	  (("number(Zero)"
		"even(Zero)"
		"Ax.(even(x) - ~odd(x))")
	    "~odd(Zero)")

	  (("even(Zero)"
		"Ax.((even(x) V odd(x)) & (even(x) - ~odd(x)))"
		"Ax.((even(x) > odd(succ(x))) & (odd(x) > even(succ(x))))")
	    "even(succ(succ(succ(succ(Zero)))))")

	 (( "Ax.=(x,x)"
		"AxAy.(=(x,y) - =(y,x))"
		"AxAyAz.(=(x,y) & =(y,z) > =(x,z))"
		)
	    "=(Z,C) & =(C,D) > =(Z,D)")

	 (("AxAy.(parent(x,y) > ancestor(x,y))"
		"AxAyAz.(parent(x,y) & ancestor(y,z) > ancestor(x,z))"
		"parent(Kh,Ky)"
		"parent(Ts,Kh)"
		"parent(Sj,Ts)"
		"parent(Kj,Sj)"
		"parent(Mj,Kj)"
	  	"parent(Pj,Mj)"
  	    "parent(Hj,Pj)")
	    "ancestor(Hj,Ky)")

	 (( "AxAyAz.(parent(x,y) & parent(x,z) > sibling(y,z))"
	   "AxAy.(sibling(x,y) - sibling(y,x))"
	   "AxAyAz.(sibling(x,y) & parent(x,z) > R(z,y))"
	   "parent(N,S)"
	   "parent(N,K)"
	   "parent(N,W)"
	   "parent(S,T)")
	   "R(T,K) & sibling(K,W)")

	 ((
	   "Ax.=(x,x)"
	   "AxAy.(=(x,y) - =(y,x))"
	   "AxAyAz.(=(x,y) & =(y,z) > =(x,z))"

	   "Ax.(~=(succ(x),ZERO))"
	   "AxAy.(=(succ(x),succ(y)) > =(x,y))"

	   "Ax.(=(f(x,ZERO),x))"
	   "AxAy.(=(f(x,succ(y)),succ(f(x,y))))"
	   )

	   "=(f(ZERO,ZERO),ZERO)")


	 (("Ax.sum(x,ZERO,x)"
	   "AxAyAz.(sum(x,y,z) > sum(x,s(y),s(z)))")
	   "sum(s(ZERO) , s(s(ZERO)) , s(s(s(ZERO))))")
	

	 (("Ax.sum(x,ZERO,x)"
	   "AxAyAz.(sum(x,y,z) > sum(x,s(y),s(z)))")
	   "Eres.sum(s(ZERO) , s(s(ZERO)) , res)")
	 
	(("Ax.sum(x,ZERO,x)"
	  "AxAyAz.(sum(x,y,z) > sum(x,s(y),s(z)))"
	  "Ax.prd(x,ZERO,ZERO)"
	  "AnAmAkAp.(sum(k,m,p) & prd(m,n,k) > prd(m,s(n),p))")
	   "prd(s(s(s(s(ZERO)))),s(s(ZERO)),s(s(s(s(s(s(s(s(ZERO)))))))))")

	(("Ax.(P(x) > B(x))"
	  "Ax.(C(x) > D(x))"
	  "Ax.(Q(x) > F(x))"
	  "Ax.(~G(x) > ~H(x))"
	  "Ax.(B(x) > I(x))"
	  "Ax.(~P(x) > ~J(x))"
	  "Ax.(K(x) > ~D(x))"
	  "Ax.(~H(x) > ~I(x))"
	  "Ax.(~J(x) > Q(x))"
	  "Ax.(G(x) > C(x))")
	 "Ax.(K(x) > F(x))")

	(("Ax.sum(x,ZERO,x)"
	  "AxAyAz.(sum(x,y,z) > sum(x,s(y),s(z)))"
	  "Ax.prd(x,ZERO,ZERO)"
	  "AnAmAkAp.(sum(k,m,p) & prd(m,n,k) > prd(m,s(n),p))")
	 "Ex.prd(s(s(s(s(ZERO)))),s(s(s(ZERO))),x)")

	(("AxAyAz.(parent(x,y) & parent(x,z) > sibling(y,z) & sibling(z,y))"
  	  "AxAyAz.(sibling(x,y) & parent(x,z) > R(z,y))"
  	  "AxAy.(sibling(x,y) & female(x) > sister(x,y))"
      "female(Wakame)"
      "female(Sazae)"
      "parent(Namihei,Sazae)"
      "parent(Namihei,Katuo)"
      "parent(Namihei,Wakame)"
      "parent(Sazae,Tara)")
	 "Ex.(sister(x,Katuo) & R(Tara,x))")


	(("Ax.(sum(x,ZERO,x) & sum(ZERO,x,x))"
  	  "AxAyAz.(sum(x,y,z) > sum(x,s(y),s(z)))")
	 "Ex.sum(s(ZERO),s(ZERO),x)")

	(()
	 "(((P > (R & ~S)) > P) > P)")


	(("AxAs.(~onbox(s) > at(Box,x,pushbox(x,s)))"
	  "As.onbox(climbbox(s))"
	  "As.(onbox(s) & at(Box,C,s) > hb(grasp(s)))"
	  "AxAs.(at(Box,x,s) > at(Box,x,climbbox(s)))"
	  "Es.~onbox(s)")	 
	"Es.hb(s)")

	(("Ax.(cat(x) > animal(x))"
	  "Ax.(cat(x) > cute(x))"
	  "Ex.(cat(x) & blueyes(x))")	 
	 "Ex.(cat(x) & cute(x) & blueyes(x))")

	(("AxAy.(P(x,y) > Q(x) V R(y,x))"
	  "AxAy.(R(y,x) & S(x) > T(y) & U(x))"
	  "AxAy.(S(x) & P(x,y))")
	 "AxAy.(~T(x) V ~U(y) > ~R(x,y))")

	#|
	((
	  "Ac.append(NIL,c,c)"
	  "AxAyAzAr.(append(x,y,z) > append(cons(r,x),y,cons(r,z)))"
	  "reverse(NIL,NIL)"
	  "AxAyAzAe.(reverse(x,z) & append(z,cons(e,NIL),y) > reverse(cons(e,x),y))")
	 "Ex.reverse(cons(A,cons(B,cons(C,cons(D,cons(E,cons(F,cons(G,NIL))))))),x)"
	 )

	((
	"Ax.sum(ZERO,x,x)"
	"AxAyAz.(sum(x,y,z) > sum(s(x),y,s(z)))"

	"Ax.mult(x,ZERO,ZERO)"
	"AkAmAnAp.(sum(k,m,p) & mult(m,n,k) > mult(m,s(n),p))"

	"fact(ZERO,s(ZERO))"
	"AmAkAr.(mult(m,s(k),r) & fact(k,m) > fact(s(k),r))"
		)
	
	 "Ex.fact(s(s(s(ZERO))),x)"
	 )

	|#
	))



(defvar *resolution-error*
  '(

	(("P > Q" 
	  "Q") 
	  "P")

	(("P V Q")
	 "P")

	(("P")
	 "P & Q")

	(("P V Q"
	  "P > R")
	 "R")

	(() 
	 "P(C)")

	(("P > Q"
	  "Q > R"
	  "R > S"
	  "S > T"
	  "T > U"
	  "U > W")
	 "~(~W > ~P)")

	(("Ax.(P(x) > Q(x))")
	 "Ex.P(x) V Ex.Q(x)")

	(("Ax.(P(x) V Q(x))")
	 "Ax.P(x)")

	
	
	))


(defun parse (str)
  (flexpr.system.parser:string->lexpr str))

(defun dump1  (lexpr)
  "convert lexpr to string expression from inner structure.
  lexpr must be atomic-lexpr or normal-lexpr or lexpr"
  (flexpr.system.dump:lexpr->string lexpr))

(defun dump2 (clause-form op)
  "convert clause form to string expression.
  clause form must be set of set of %literal structure."
  (flexpr.system.dump:clause-form->string clause-form op))

(defun formal (str op q)
  "convert to prenex normal form end matrix is cnf or dnf"
  (dump1 (flexpr.system.formalize:formalize (parse str) op q)))

(defun convert (str op q)
  "convert to clause form"
  (dump2 
	(flexpr.system.formalize:convert (parse str) op q) op))

(defun pl (&rest strings)
  (mapcar #'parse strings))


(defun infer (conseq &rest premises)
  (destructuring-bind (r b u)
	(flexpr.system.infer.wrap::resolution
	  (apply #'pl premises)
	  (parse conseq))
	(format t "~%~%RESULT~%~A~%TERM~%~A~%~%" r 
			(loop for eachb in b
				  for eachu in u
				  collect 
				  (cons (flexpr.system.dump::term->string eachb)
						(flexpr.system.dump::term->string eachu))))))


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
		(flexpr.system.infer.wrap::resolution
		  (apply #'pl (first each))
		  (parse (second each)))	)))

(define-test resolution-error-test 
	(dolist (each *resolution-error*)
	  (assert-error
		'flexpr.system.error:undeterminable-error
		(flexpr.system.infer.wrap::resolution
		  (apply #'pl (first each))
		  (parse (second each))))))


;;#|
(print-failures (run-tests '(formalize-test)))
(print-errors (run-tests '(resolution-test)))
(print-errors (run-tests '(resolution-error-test)))
;;|#


#|
(loop repeat 50
			do 
			(run-tests '(resolution-test resolution-error-test)))
|#
