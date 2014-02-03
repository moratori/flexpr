


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



;;; normalization 


;;; 量化子を先頭に移動(冠頭形に変換)
@export
(defgeneric lexpr->prenex (a)
	(:documentation "prenex normal formalizetion"))


(defmethod  lexpr->prenex ((lexpr atomic-lexpr))
  lexpr)


;;; スコーレム標準形にする
@export
(defgeneric lexpr->skolem (a)
	(:documentation "skolemization"))


(defmethod  lexpr->skolem ((lexpr atomic-lexpr))
  lexpr)


;;; 節形式に変換する(連言標準形(CNF) (L V L ...) & (L V L ...) & ... (L V L ...) )
@export
(defgeneric lexpr->clause-form (a)
	(:documentation "clause formalization"))


(defmethod  lexpr->clause-form ((lexpr atomic-lexpr))
  lexpr)


