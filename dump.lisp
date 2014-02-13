

(ns:defns flexpr.dump
	(:use :cl 
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :flexpr.util
				  :opr-equal?
				  :opr->strength
				  :opr-strong?)
	(:import-from :flexpr.error
				  :struct-unmatch-error)
	(:import-from :optima
				  :match))


@export
(defgeneric opr->string (opr)
	(:documentation "convert operator 2 string expression"))


(defmethod opr->string ((opr operator))
  (second (assoc (operator-opr opr) +OPERATOR+)))





@export
(defgeneric term->string (a)
	(:documentation "convert term to string expression"))


(defmethod term->string ((term vterm))
  (format nil "~A" (vterm-var term)))


(defmethod term->string ((term fterm))
  (match term
	((fterm :fsymbol fsymbol :terms terms)
	 (format nil "~A(~{~A~^,~})" 
			 fsymbol 
			 (mapcar #'term->string terms)))
	(otherwise (error (make-condition 'struct-unmatch-error 
					   :sue-val term
					   :sue-where 'term->string_fterm)))))





@export
(defgeneric quant->string (quant)
	(:documentation "convert quantifier to string expression"))


(defmethod quant->string ((quant quant))
  (match quant
	((quant :qnt qnt :var var :neg neg)
	 (format nil "~A~A~A"
			 (if (not (zerop neg)) 
			   (make-sequence 'string
							  neg 
							  :initial-element 
							  (char 
				 				(opr->string 
				   					(operator +NEG+)) 0))
			   "")
			 (second (assoc qnt +QUANTS+))
			 (term->string var)))	 
	(otherwise (error (make-condition 'struct-unmatch-error 
					   :sue-val quant
					   :sue-where 'quant->string_quant)))))





@export
(defgeneric quantsp->string (quantsp)
	(:documentation "convert quantifier part to string expression"))

(defmethod quantsp->string ((quantsp quantsp))
  (format nil "~{~A~}." 
		  (mapcar #'quant->string (quantsp-each-quant quantsp))))







@export
(defgeneric lexpr->string (a)
	(:documentation "convert logical expr to string"))


(defmethod lexpr->string ((lexpr atomic-lexpr))
  ;; 実質的に 原始論理式は 関数記号を含んだ項と同型なのでその処理でいく
  (match lexpr
	((atomic-lexpr :pred-sym pred-sym :terms terms)
	 (if (null terms) 
	   (format nil "~A" pred-sym)
	   (term->string 
		 (apply #'fterm pred-sym terms))))
	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'lexpr->string_atomic-lexpr)))))


(defmethod lexpr->string ((lexpr normal-lexpr))
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 ;;	オペレータの強さと
	 ;;	l-lexprとr-lexprの関係から適当に括弧を省く処理が必要
	 (cond 
	   ((opr-equal? operator (operator +NEG+))
		(format nil "~A(~A)" 
				(opr->string operator)
				(lexpr->string l-lexpr)))
	   (t (format nil "(~A) ~A (~A)"
				(lexpr->string l-lexpr)
				(opr->string operator)
				(lexpr->string r-lexpr)))))	 
	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'lexpr->string_normal-lexpr)))))



(defmethod lexpr->string ((lexpr lexpr))
  (match lexpr
	((lexpr :qpart qpart :expr expr)
	 (format nil "~A(~A)" 
			 (quantsp->string qpart)
			 (lexpr->string expr)))
	(otherwise (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'lexpr->string_lexpr)))))


