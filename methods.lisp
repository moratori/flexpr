
#|

	(atomic-lexpr pred-sym term1 ...)
	(normal-lexpr operator left-lexpr rigt-lexpr)
	(quant qnt var)
	(quantsp (quant qnt var) ...)
	(lexpr qpart expr)
	
|#


(deftype operator-type ()
  `(satisfies operator-pred))

(deftype term-type ()
  `(satisfies terms-pred))

(deftype normal-lexpr-type ()
  `(satisfies normal-lexpr-pred))

(deftype quant-type ()
  `(satisfies quant-pred))

(deftype quantsp-type ()
  `(satisfies quantsp-pred))

(deftype lexpr-type ()
  `(satisfies lexpr-pred))


;; 最も基本となる 変数のみからなる項
(defstruct (vterm  (:constructor vterm (var)))
  (var (error "vterm: variable symbol required") 
	   :type t ;;symbol ここをsymbolにしちゃうと 1とか2とかのシンボルで無いのが扱えなくなる
	   ))



;; 一つの関数記号と、その引数 terms をとる
;; terms は 各要素が fterm または vterm 型である
(defstruct (fterm (:constructor fterm (fsymbol &rest terms)))
  (fsymbol (error "fterm: function symbol required") 
		   :type symbol)
  (terms   nil
		   :type term-type))


(defstruct (atomic-lexpr (:constructor atomic-lexpr (pred-sym &rest terms)))
  (pred-sym (error "atomic-lexpr: predicate symbol required")
			:type symbol)
  (terms    nil
			:type term-type))


(defstruct (operator (:constructor operator (opr)))
  (opr (error "operator: operator required") 
	   :type operator-type))



;;; normal-lexpr とは 量化子のないような論理式のこと
(defstruct (normal-lexpr (:constructor normal-lexpr (operator l-lexpr r-lexpr)))
  (operator (error "normal-lexpr: operator required") 
			:type operator)
  (l-lexpr  (error "normal-lexpr: left logical expression must be required") 
		    :type normal-lexpr-type)
  ;; operator が neg じゃない時はかならずここは nil ではいけない
  (r-lexpr nil 
		    :type normal-lexpr-type))



(defstruct (quant (:constructor quant (qnt var &optional (neg nil))))
  (qnt (error "quants: qnt required") :type quant-type)
  (var (error "quants: var required") :type vterm)
  (neg nil :type boolean) ;; 奇数回、量化子を否定するならここがt
  )


(defstruct (quantsp (:constructor quantsp (&rest each-quant)))
  (each-quant nil :type quantsp-type))



(defstruct (lexpr (:constructor lexpr (qpart expr)))
  (qpart (error "lexpr: qpart required") 
		 :type quantsp)
  (expr  (error "logical expression required")
		:type lexpr-type))



(defun terms-pred (terms)
  (and 
	(listp terms)
	(every (lambda (x)
			 (or (vterm-p x)
				 (fterm-p  x))) terms)))

(defun operator-pred (op)
  (member op +OPERATOR+ 
		  :test (lambda (x y) 
				  (declare (ignore x))
				  (eq op (car y)))))

(defun normal-lexpr-pred (nlexpr)
  (or (null nlexpr)
	  (atomic-lexpr-p nlexpr)
	  (normal-lexpr-p nlexpr)
	  (lexpr-p nlexpr)))

(defun quant-pred (q)
  (member q +QUANTS+ :test 
		  (lambda (x y) 
			(declare (ignore x))
			(eq q (car y)))))

(defun quantsp-pred (ls)
  (every #'quant-p ls))


(defun lexpr-pred (l)
  (or (atomic-lexpr-p l)
	  (normal-lexpr-p l)
	  (lexpr-p l)))





















(defmethod opr->string ((opr operator))
  (second (assoc (operator-opr opr) +OPERATOR+)))


(defmethod opr->strength ((opr operator))
  (third  (assoc (operator-opr opr) +OPERATOR+)))


(defmethod opr-strong? ((opr1 operator) (opr2 operator))
  (< (opr->strength opr1)
	 (opr->strength opr2)))


(defmethod opr-equal? ((opr1 operator) (opr2 operator))
  (eq (operator-opr opr1)
	  (operator-opr opr2)))








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
			 (if neg (opr->string (operator +NEG+)) "")
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



