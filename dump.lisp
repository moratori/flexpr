

(defpackage dump
  (:use :cl
		:optima
		:constant
		:struct
		:util)
  (:export 
	:opr->string
	:term->string
	:quant->string
	:quantsp->string
	:lexpr->string))
(in-package dump)



(defmethod opr->string ((opr operator))
  (second (assoc (operator-opr opr) +OPERATOR+)))



(defgeneric term->string (a)
	(:documentation "convert term to string"))


(defmethod term->string ((term vterm))
  (format nil "~A" (vterm-var term)))


(defmethod term->string ((term fterm))
  (match term
	((fterm fsymbol terms)
	 (format nil "~A(~{~A~^,~})" 
			 fsymbol 
			 (mapcar #'term->string terms)))
	(otherwise (error "term->string: invalid data structure"))))



(defmethod quant->string ((quant quant))
  (match quant
	((quant qnt var neg)
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
	(otherwise (error "quant->string: invalid data structure"))))


(defmethod quantsp->string ((quantsp quantsp))
  (format nil "~{~A~}." 
		  (mapcar #'quant->string (quantsp-each-quant quantsp))))





(defgeneric lexpr->string (a)
	(:documentation "convert logical expr to string"))


(defmethod lexpr->string ((lexpr atomic-lexpr))
  ;; 実質的に 原始論理式は 関数記号を含んだ項と同型なのでその処理でいく
  (match lexpr
	((atomic-lexpr pred-sym terms)
	 (term->string 
	   (apply #'fterm pred-sym terms)))
	(otherwise 
	  (error "lexpr->string(atomic-lexpr): invalid data structure"))))


(defmethod lexpr->string ((lexpr normal-lexpr))
  (match lexpr
	((normal-lexpr operator l-lexpr r-lexpr)
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
	  (error "lexpr->string(normal-lexpr): invalid data structure"))))



(defmethod lexpr->string ((lexpr lexpr))
  (match lexpr
	((lexpr qpart expr)
	 (format nil "~A(~A)" 
			 (quantsp->string qpart)
			 (lexpr->string expr)))
	(otherwise (error "lexpr->string(lexpr): invalid data structure"))))


