


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




;;; normalization 


;;; 量化子を先頭に移動(冠頭形に変換)
@export
(defgeneric lexpr->prenex (a)
	(:documentation "prenex normal formalizetion"))


;;; スコーレム標準形にする
@export
(defgeneric lexpr->skolem (a)
	(:documentation "skolemization"))


;;; 節形式に変換する(連言標準形(CNF) (L V L ...) & (L V L ...) & ... (L V L ...) )
@export
(defgeneric lexpr->clause-form (a)
	(:documentation "clause formalization"))





