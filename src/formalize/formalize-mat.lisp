
(ns:defns flexpr.formalize.mat
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :optima
				  :match)
	(:import-from :flexpr.util
				  :opr-equal?
				  :opposite-opr
				  :clause?)
	(:import-from :flexpr.error
				  :struct-unmatch-error))


@export
(defgeneric c/dnf (lexpr op)
	(:documentation "formalize in cnf or dnf"))

(defmethod c/dnf ((lexpr atomic-lexpr) (op operator))
  lexpr)


(defmethod c/dnf ((lexpr normal-lexpr) (op operator))
  (let ((revop (opposite-opr op)))
	(if (clause? lexpr (operator revop)) lexpr
		(match lexpr
		;; F V (G & H) -> (F V G) & (F V H)
		;; F & (G V H) -> (F & G) V (F & H)
    	;; (G & H) V F -> (F V G) & (F V H)
		;; (G V H) & F -> (F & G) V (F & H)

		((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
		 	
		 	(cond 

			  ((and (opr-equal? (operator revop) operator)
					(normal-lexpr-p r-lexpr)
					(opr-equal? op (normal-lexpr-operator r-lexpr)))
			   
			   		(normal-lexpr op
						(c/dnf (normal-lexpr (operator revop) 
									  l-lexpr 
									  (normal-lexpr-l-lexpr r-lexpr))
							   op)
						(c/dnf (normal-lexpr (operator revop) 
									  l-lexpr 
									  (normal-lexpr-r-lexpr r-lexpr))
							   op)))
			  
			  ((and (opr-equal? (operator revop) operator)
					(normal-lexpr-p l-lexpr)
					(opr-equal? op (normal-lexpr-operator l-lexpr)))
			   		
			   		(normal-lexpr op
						(c/dnf (normal-lexpr (operator revop) 
									  r-lexpr 
									  (normal-lexpr-l-lexpr l-lexpr))
							   op)
						(c/dnf (normal-lexpr (operator revop) 
									  r-lexpr 
									  (normal-lexpr-r-lexpr l-lexpr))
							   op)))


			  ((and (opr-equal? (operator revop) operator)
					(normal-lexpr-p l-lexpr)
					(opr-equal? operator (normal-lexpr-operator l-lexpr)))
			   
			   (c/dnf 
				 (normal-lexpr (operator revop)
					(normal-lexpr-l-lexpr l-lexpr)
					(c/dnf 
					  (normal-lexpr (operator revop)
							(normal-lexpr-r-lexpr l-lexpr)		  
							r-lexpr) op)) op))

			  ((and (opr-equal? (operator revop) operator)
					(normal-lexpr-p r-lexpr)
					(opr-equal? operator (normal-lexpr-operator r-lexpr)))
			   
			   (c/dnf 
				 (normal-lexpr (operator revop)
					(c/dnf (normal-lexpr (operator revop)		  
								  l-lexpr
								  (normal-lexpr-l-lexpr r-lexpr))
					op)
				   (normal-lexpr-r-lexpr r-lexpr)
					) op))

			  ((opr-equal? operator op)
			   (normal-lexpr op
					(c/dnf l-lexpr op)
					(c/dnf r-lexpr op)))
			  (t  lexpr)))
		(otherwise 
		  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'c/dnf_normal-lexpr)))))))

;; 連言標準形に変換
(defmethod c/dnf ((lexpr lexpr) (op operator))
  (lexpr (lexpr-qpart lexpr)
		 (c/dnf (lexpr-expr lexpr) op)))


