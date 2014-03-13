


(ns:defns flexpr.system.util
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
	(:import-from :optima
				  :match)
	(:import-from :flexpr.system.error
				  :struct-unmatch-error))

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
(defmethod qnt-equal? ((q1 quant) (q2 quant))
  (eq (quant-qnt q1)
	  (quant-qnt q2)))


@export
(defmethod opposite-qnt ((quant quant))
  (cond 
	((qnt-equal? quant (quant +FORALL+ (vterm '|dummy| nil) 0))
	 +EXISTS+)
	((qnt-equal? quant (quant +EXISTS+  (vterm '|dummy| nil) 0))
	 +FORALL+)
	(t 
	 (error (make-condition 'struct-unmatch-error 
					   :sue-val quant
					   :sue-where 'opposite-qnt)))))


@export
(defmethod opposite-opr ((opr operator))
  (cond
	((opr-equal? opr (operator +AND+))
	 +OR+)
	((opr-equal? opr (operator +OR+))
	 +AND+)
	(t 
	 (error (make-condition 'struct-unmatch-error 
					   :sue-val opr
					   :sue-where 'opposite-opr)))))


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





(defgeneric term-using? (a b)
	(:documentation "check whether term occurred in b"))


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
	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'term-using?_normal-lexpr)))))

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
	(otherwise 
	 (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'term-using?_lexpr)))))


@export
(defgeneric literal? (a)
	(:documentation "check whether literal"))


(defmethod literal? ((lexpr atomic-lexpr))
  t)

(defmethod literal? ((lexpr normal-lexpr))
  (cond
	((not (opr-equal? (operator +NEG+)
					  (normal-lexpr-operator lexpr)))
	 nil)
	(t (literal? 
		 (normal-lexpr-l-lexpr lexpr)))))

(defmethod literal? (a) 
  nil)




@export
(defgeneric lexpr-literal? (a)
	(:documentation "check whether lexpr or negation lexpr"))

(defmethod lexpr-literal? ((lexpr normal-lexpr))
  (cond
	((not (opr-equal? (operator +NEG+)
					  (normal-lexpr-operator lexpr)))
	 nil)
	(t (lexpr-literal? 
		 (normal-lexpr-l-lexpr lexpr)))))

(defmethod lexpr-literal? ((lexpr lexpr))
  t)

(defmethod lexpr-literal? (a)
  nil)


@export
(defun gliteral? (l)
  (or (literal? l)
	  (lexpr-literal? l)))



@export
(defgeneric clause? (a b)
   (:documentation "check whther clause form"))

(defmethod clause? ((lexpr atomic-lexpr) (opr operator))
  t)

(defmethod clause? ((lexpr normal-lexpr) (opr operator))
  (cond 
	((literal? lexpr) t)
	((opr-equal? opr (normal-lexpr-operator lexpr))
	 (and (clause? (normal-lexpr-l-lexpr lexpr) opr)
		  (clause? (normal-lexpr-r-lexpr lexpr) opr)))
	(t nil)))

(defmethod clause? (a b)
  nil)


(defgeneric %closed? (a b)
	(:documentation "check whther formula b closed in environ b"))

(defmethod %closed? ((lexpr atomic-lexpr) bounds)
  ;; bounds is term list
  (every 
	(lambda (term)
	  (labels 
		((main (x)
			(cond 
			  ;((vterm-const x) t)
			  ((vterm-p x)
			   (or (vterm-const x)
				   (member x bounds :test #'term=)))
			  ((fterm-p x)
			   (every 
				 #'main
				 (fterm-terms x)))
			  (t 
				(error 
				  (make-condition 'struct-unmatch-error 
							:sue-val term
							:sue-where '%closed?_atomic))))))
		(main term))) 
	(atomic-lexpr-terms lexpr)))


(defmethod %closed? ((lexpr normal-lexpr) bounds)
  (and
	(%closed? 
	  (normal-lexpr-l-lexpr lexpr) bounds)
	(let ((right (normal-lexpr-r-lexpr lexpr)))
	  (if (not (null right))
		(%closed? right bounds) t))))


(defmethod %closed? ((lexpr lexpr) bounds)
  (%closed? 
	(lexpr-expr lexpr)
	(append 
	  bounds
	  (loop for each in (quantsp-each-quant (lexpr-qpart lexpr))
			collect (quant-var each)))))

@export
(defun closed? (lexpr)
  (%closed? lexpr nil))



@export
(defun %literal= (a b)
  (assert (and (typep a '%literal)
			   (typep b '%literal)))
  (and 
	(eq (%literal-negation a)
		(%literal-negation b))
	(eq (%literal-pred a)
		(%literal-pred b))
	(every 
	  (lambda (x y) 
		(term= x y)) 
	  (%literal-terms a)
	  (%literal-terms b))))

@export
(defun opposite-%literal= (a b)
  ;; b が aの負リテラルであるか
  (assert (and (typep a '%literal)
			   (typep b '%literal)))
  (and 
	(eq (not (%literal-negation a))
		(%literal-negation b))
	(eq (%literal-pred a)
		(%literal-pred b))
	(every 
	  (lambda (x y) 
		(term= x y)) 
	  (%literal-terms a)
	  (%literal-terms b)))
  )


@export
(defun %clause= (a b)
 (assert (and (typep a 'clause)
			  (typep b 'clause)))
 (and 
   (null 
	 (set-difference 
	   (clause-%literals a)
	   (clause-%literals b) 
	   :test #'%literal=))
   (null 
	 (set-difference 
	   (clause-%literals b)
	   (clause-%literals a)
	   :test #'%literal=))))


;; これとおんなじようなのが unifier にも定義されてるけど後でこれ
;; 使うようにリファクタリング〜
@export
(defun substitute-term (term old new)
  (cond 
	((vterm-p term)
	 (if (term= old term) new term))
	((fterm-p term)
	 (apply #'fterm 
			(fterm-fsymbol term)
			(mapcar 
			  (lambda (x) 
				(substitute-term x old new)) 
				(fterm-terms term))))
	(t (error "subst error ! ~A" term))))


