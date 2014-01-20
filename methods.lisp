
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






(defgeneric lexpr->string (a)
	(:documentation "convert logical expr to string"))


(defmethod lexpr->string ((lexpr atomic-lexpr))
  ;; 実質的に 原始論理式は 関数記号を含んだ項と同型なのでその処理でいく
  (match lexpr
	((atomic-lexpr pred-sym terms)
	 (term->string 
	   (apply #'fterm pred-sym terms)))
	(otherwise 
	  (error "lexpr->string: invalid data structure"))))


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
	  (error "lexpr->string: invalid data structure"))))


