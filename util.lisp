


(ns:defns util
	(:use :cl
		  :constant
		  :struct)
	(:import-from :optima
				  :match))

@export
(defmethod opr->strength ((opr operator))
  (third  (assoc (operator-opr opr) +OPERATOR+)))

@export
(defmethod opr-strong? ((opr1 operator) (opr2 operator))
  (< (opr->strength opr1)
	 (opr->strength opr2)))

@export
(defmethod opr-equal? ((opr1 operator) (opr2 operator))
  (eq (operator-opr opr1)
	  (operator-opr opr2)))




@export
(defgeneric term= (a b)
	;;統語論的に完全に一致するかを確かめる
	;;だから 変数 x と 変数 yは一致しないし
	;;f(x) と f(z)も同一でない. シンボルが違うから
	(:documentation "identity predicate"))


(defmethod term= ((t1 vterm) (t2 vterm))
  (and
	(eq (vterm-var t1) (vterm-var t2))
	(eq (vterm-const t1) (vterm-const t2))))


(defmethod term= ((t1 fterm) (t2 fterm))
  (and 
	(eq (fterm-fsymbol t1) 
		(fterm-fsymbol t2))
	(let ((argv1 (fterm-terms t1))
		  (argv2 (fterm-terms t2)))
	  (and 
		(= (length argv1) 
		   (length argv2))
		(every 
		  (lambda (x y) (term= x y)) argv1 argv2)))))


(defmethod term= (a b) nil)







#|

	normalization 

|#



(defmethod term-using? ((vterm vterm) (term vterm))
  (term= vterm term))


(defmethod term-using? ((vterm vterm) (term fterm))
  (some 
	(lambda (x)
	  (term-using? vterm x))
	(fterm-terms term)))


;; vterm が lexpr中において自由出現するかいなか
(defmethod term-using? ((vterm vterm) (lexpr atomic-lexpr))
  ;; 命題変数考慮しろ
  (some 
	(lambda (term)
	  (term-using? vterm term))
	(atomic-lexpr-terms lexpr)))


(defmethod term-using? ((vterm vterm) (lexpr normal-lexpr))
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 (if (null r-lexpr)
	   (term-using? vterm l-lexpr)
	   (or (term-using? vterm l-lexpr)
		   (term-using? vterm r-lexpr))))	 
	(otherwise (error "term-using?(normal-lexpr): unexpected error"))))

;; Ax.(Ax.P(x)) のなかで外側は除去れる x は自由出現しないからってかこんな式書く奴いんの
;; Ax.(P(x) > Ax.R(x)) この場合は P(x)で自由出現してるから除去れない
(defmethod term-using? ((vterm vterm) (lexpr lexpr))
  ;; 量化子にxを束縛するようなのがなくてかつ、母式に出現するならterm-using?->true
  ;; 量化子にxを束縛するやつがいる V 母式に出現しない
  (match lexpr
	((lexpr :qpart qpart :expr expr)
	 (and
		(every 
		  (lambda (qnt)
			(not (term= vterm (quant-var qnt))))
		 (quantsp-each-quant qpart))
	   (term-using? vterm expr)))	 
	(otherwise (error "term-using?(lexpr): unexpected error"))))






(defmethod remove-disuse-quant ((lexpr atomic-lexpr))
  lexpr)


(defmethod remove-disuse-quant ((lexpr normal-lexpr))
	(match lexpr
		((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
		 (normal-lexpr 
		   operator
		   (remove-disuse-quant l-lexpr)
		   (remove-disuse-quant r-lexpr)))	 
		(otherwise (error "term-using?(normal-lexpr): unexpected error"))))


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
	(otherwise (error "normalize-lexpr(lexpr): unexpected error"))))







