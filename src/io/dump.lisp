

(ns:defns flexpr.dump
	(:use :cl 
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :flexpr.util
				  :opr-equal?
				  :opr->strength
				  :opr-strong?
				  :opposite-opr
				  :literal?
				  :lexpr-literal?
				  :gliteral?)
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


(defun %lexpr->string (format left op right)
  (format nil format 
		  (lexpr->string left)
		  (opr->string op)
		  (lexpr->string right)))

(defun %lexpr->string_c/dnf (upper-opr under-opr expr)
(cond
  ((and (opr-equal? upper-opr under-opr)
		(or (opr-equal? upper-opr (operator +AND+))
			(opr-equal? upper-opr (operator +OR+))))
   (format nil "~A"   (lexpr->string expr)))
  
  ((= (opr->strength upper-opr)
	  (opr->strength under-opr))
   (format nil "(~A)" (lexpr->string expr)))
  (t (format nil "~A"   (lexpr->string expr)))))

(defmethod lexpr->string ((lexpr normal-lexpr))
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 ;;	オペレータの強さと
	 ;;	l-lexprとr-lexprの関係から適当に括弧を省く処理が必要
	 (cond 

	   ((opr-equal? operator (operator +NEG+))
		(if (or (literal?        l-lexpr)
				(lexpr-literal?  l-lexpr))
		  (format nil "~A~A" 
				  (opr->string operator)
				  (lexpr->string l-lexpr))
		  (format nil "~A(~A)" 
				  (opr->string operator)
				  (lexpr->string l-lexpr))))
	  

	   ((and (gliteral? l-lexpr)
			 (gliteral? r-lexpr))
			(%lexpr->string "~A ~A ~A" 
							l-lexpr operator r-lexpr))	

	   ((gliteral? l-lexpr)
	;	(%lexpr->string "~A ~A (~A)" 
	;					l-lexpr operator r-lexpr)
		(format nil "~A ~A ~A"
				(lexpr->string l-lexpr)
				(opr->string operator)
				(%lexpr->string_c/dnf operator 
									  (normal-lexpr-operator r-lexpr)
									  r-lexpr)))

	   ((gliteral? r-lexpr)
		;(%lexpr->string "(~A) ~A ~A"
		;				l-lexpr operator r-lexpr)
		(format nil "~A ~A ~A"
				(%lexpr->string_c/dnf operator 
									  (normal-lexpr-operator l-lexpr)
									  l-lexpr)	
				(opr->string operator)
				(lexpr->string r-lexpr)))

		
	   ((and (normal-lexpr-p l-lexpr)
			 (normal-lexpr-p r-lexpr))
		;; V と & については 結合律が成り立つのでその規則を入れれば
		;; 自然な感じに 連/選言標準形がdumpできる
		(let* ((l-op (normal-lexpr-operator l-lexpr))
			  (r-op (normal-lexpr-operator r-lexpr))
			  (leftstr 
				(%lexpr->string_c/dnf operator l-op l-lexpr))
			   (rightstr 
				(%lexpr->string_c/dnf operator r-op r-lexpr)))
		  (format nil "~A ~A ~A" leftstr (opr->string operator) rightstr)))

	   (t 
		 
		 (%lexpr->string "(~A) ~A (~A)"
						 l-lexpr operator r-lexpr))))	 

	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'lexpr->string_normal-lexpr)))))



(defmethod lexpr->string ((lexpr lexpr))
  (match lexpr
	((lexpr :qpart qpart :expr expr)
	 (let ((qpstr (quantsp->string qpart)))
	   (if (literal? expr)
		 (format nil "~A~A"   qpstr (lexpr->string expr))
		 (format nil "~A(~A)" qpstr (lexpr->string expr)))))
	(otherwise (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'lexpr->string_lexpr)))))


(defun %literal->string (%literal)
	(lexpr->string 
	  (if (%literal-negation %literal)
 		(normal-lexpr (operator +NEG+)
			(apply #'atomic-lexpr 
				   (%literal-pred %literal)
				   (%literal-terms %literal)) nil)
		(apply #'atomic-lexpr 
			   (%literal-pred %literal)
			   (%literal-terms %literal)))))

(defun clause->string (clause op)
  (format nil 
		  (format nil "(~~{~~A~~^ ~A ~~})" (opr->string op))
		  (mapcar #'%literal->string clause)))

@export
(defun clause-form->string (clause-form op) 
  (format nil
		  (format nil "~~{~~A~~^ ~A ~~}" (opr->string op))
		  (mapcar 
			(lambda (clause)
	  			(clause->string 
				  clause
				  (operator (opposite-opr op)))) clause-form)))

