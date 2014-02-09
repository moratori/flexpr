

(ns:defns formalize
	(:use :cl
		  :constant
		  :struct)
	(:import-from :optima
				  :match)
	(:import-from :util
				  :term-using?
				  :opr-equal?))



@export
(defgeneric remove-disuse-quant (a)
	(:documentation "remove disuse quantifier in expression a"))

(defmethod remove-disuse-quant ((lexpr atomic-lexpr))
  lexpr)


(defmethod remove-disuse-quant ((lexpr normal-lexpr))
	(match lexpr
		((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
		 (normal-lexpr 
		   operator
		   (remove-disuse-quant l-lexpr)
		   (remove-disuse-quant r-lexpr)))	 
		(otherwise (error "remove-disuse-quant(normal-lexpr): unexpected error"))))


(defmethod remove-disuse-quant ((lexpr lexpr))
  (match lexpr
	((lexpr :qpart qpart :expr expr)
	 ;; まずどうでもいい束縛を除去
	 ;; (Qτ ).φ についてτ が φ 中で自由出現しないなら(Qτ)を除去レ
	 (let ((new-qpart 
			 (remove-if 
			   (lambda (qnt)
				 (not (term-using? (quant-var qnt) expr)))
			   (quantsp-each-quant qpart))))
	   
	   (if (null new-qpart)
		 (remove-disuse-quant expr)
		 (lexpr (apply #'quantsp new-qpart) 
				(remove-disuse-quant expr)))))
	(otherwise (error "remove-disuse-quant(lexpr): unexpected error"))))




@export
(defgeneric remove-operator (a)
	(:documentation "remove operator other than NOT or OR or AND"))


(defmethod remove-operator ((lexpr atomic-lexpr))
  lexpr)




(defmethod remove-operator ((lexpr normal-lexpr))
  (match lexpr
	((lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 (cond
	   ((opr-equal? operator +IMPL+)
		)
	   ((opr-equal? operator +EQ+)
		)
	   ;; add another operator
	   )
	 )	 
	(otherwise (error "remove-operator(normal-lexpr): unexpected error"))))


(defmethod remove-operator ((lexpr lexpr))
  (match lexpr
	((lexpr :qpart qpart :expr expr)
	 (lexpr qpart
			(remove-operator expr)))	 
	(otherwise (error "remove-operator(lexpr): unexpected error"))))



