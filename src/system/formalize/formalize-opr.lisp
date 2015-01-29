
(ns:defns flexpr.system.formalize.opr
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
	(:import-from :optima
				  :match)
	(:import-from :flexpr.system.util
				  :opr-equal?
				  :opposite-qnt
				  :opposite-opr)
	(:import-from :flexpr.system.error
				  :struct-unmatch-error
				  :illformed-formalize-error))



@export
(defgeneric remove-operator (a)
	(:documentation "remove operator other than NOT or OR or AND"))


(defmethod remove-operator ((lexpr atomic-lexpr))
  lexpr)



(defmethod remove-operator ((lexpr normal-lexpr))
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 (cond 

	   ((opr-equal? operator (operator +NEG+))
		(normal-lexpr operator
		  (remove-operator l-lexpr)
		  nil))

	   ((or (opr-equal? operator (operator +OR+))
			(opr-equal? operator (operator +AND+)))
		(normal-lexpr operator
			(remove-operator l-lexpr)		  
			(remove-operator r-lexpr)))

	   ((opr-equal? operator (operator +IMPL+))
		(normal-lexpr (operator +OR+)
			(normal-lexpr (operator +NEG+)
			  (remove-operator l-lexpr) nil)
			(remove-operator r-lexpr)))
	   
	   ((opr-equal? operator (operator +EQ+))
		(normal-lexpr (operator +AND+)
			(remove-operator 
			  (normal-lexpr (operator +IMPL+)
					l-lexpr
					r-lexpr))		  
			(remove-operator 
			  (normal-lexpr (operator +IMPL+)
					r-lexpr
					l-lexpr))))
	   (t 
		 (error (make-condition 'illformed-formalize-error
								:ife-mes "conversion rule is not defined"
								:ife-val operator
								:ife-where 'remove-operator_normal-lexpr)))))
	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'remove-operator_normal-lexpr)))))


(defmethod remove-operator ((lexpr lexpr))
  (lexpr 
	(lexpr-qpart lexpr)
	(remove-operator (lexpr-expr lexpr))))




@export
(defgeneric literalize (a)
 	(:documentation "move to the inside"))


(defmethod literalize ((lexpr atomic-lexpr))
  lexpr)

(defmethod literalize ((lexpr normal-lexpr))
  ;; 既にremove-operatorされた式が来ることを意図する
  ;; つまりlexpr中に > や - が含まれていたらエラー
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 
	 (cond 

	   ((or (opr-equal? operator (operator +AND+))
			(opr-equal? operator (operator +OR+)))		
			(normal-lexpr operator
				(literalize l-lexpr)		  
				(literalize r-lexpr)))

	   ((opr-equal? operator (operator +NEG+))
		;; r-lexprはnilだから気にすること無い
		;; l-lexprだけみてればいい
		(match l-lexpr
			
			((normal-lexpr :operator operator_ :l-lexpr l-lexpr_ :r-lexpr r-lexpr_)
			 ;; operator_ は V,&,~のどれか
			 (cond 
			   
			   ((opr-equal? operator_ (operator +NEG+))
				(literalize l-lexpr_))

			   ;; どモルガン
			   ((or (opr-equal? operator_ (operator +AND+))
					(opr-equal? operator_ (operator +OR+)))
				(normal-lexpr (operator (opposite-opr operator_))
					(literalize 
					  (normal-lexpr (operator +NEG+) l-lexpr_ nil))
					(literalize
					  (normal-lexpr (operator +NEG+) r-lexpr_ nil))))
	
			   (t 
					(error (make-condition 'illformed-formalize-error
								:ife-mes "removed operator lexpr required"
								:ife-val operator_
								:ife-where 'literalize_normal-lexpr)))))
			
			((lexpr :qpart qpart_ :expr expr_)
			 ;; ~AxAy...の形
				(let* ((quant-lst (quantsp-each-quant qpart_))
					   (head      (car quant-lst)))

				  (literalize
					(lexpr
					  (apply #'quantsp 
							 (cons (quant (quant-qnt head)
										  (quant-var head)
										  (1+ (quant-neg head))) 
								   (cdr quant-lst)))
					  expr_))))
			
			(otherwise 
			  (unless (typep l-lexpr 'atomic-lexpr)
					(error (make-condition 'illformed-formalize-error
								:ife-mes "l-lexpr must be atomic"
								:ife-val operator_
								:ife-where 'literalize_normal-lexpr)))
			  lexpr ;; 負リテラル
			  )))
		
	   (t 
		 (error (make-condition 'illformed-formalize-error
								:ife-mes "removed operator lexpr required"
								:ife-val operator
								:ife-where 'literalize_normal-lexpr)))))	 
	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'literalize_normal-lexpr)))))




(defun remove-quant-negation (lexpr)
  (assert (typep lexpr 'lexpr))

  (match lexpr
    ((lexpr :qpart qpart :expr expr)
		(lexpr
			(apply #'quantsp
				   (labels 
					 ((main (lst result acc)
						(if (null lst) (reverse result)
						  (match (car lst)
						    ((quant :qnt qnt :var var :neg neg)
							 (if (evenp (+ neg acc)) 
							   (main 
								 (cdr lst)
								 (cons (quant qnt var 0) result) 
								 0)
							   (main 
								 (cdr lst)
								 (cons (quant (opposite-qnt (car lst)) var 0) result) 
								 1)))
							(otherwise 
								(error (make-condition 'struct-unmatch-error 
											:sue-val (car lst)
					   						:sue-where 'remove-quant-negation)))))))
					 (main (quantsp-each-quant qpart) nil 0)))
			  (if (evenp 
					(reduce 
					  (lambda (x y) (+ x (quant-neg y))) 
					  (quantsp-each-quant qpart) :initial-value 0))
				expr
				(normal-lexpr (operator +NEG+) expr nil))))
	(otherwise 
	 (error (make-condition 'struct-unmatch-error 
				:sue-val lexpr
				:sue-where 'remove-quant-negation)))))


(defmethod literalize ((lexpr lexpr))
  (let ((tmp (remove-quant-negation lexpr)))
	(lexpr 
	  (lexpr-qpart tmp)
	  (literalize (lexpr-expr tmp)))))



