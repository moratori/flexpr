

(ns:defns unifier
	(:use :cl
		  :constant
		  :struct)
	(:import-from :optima
				  :match))


;;;; 単一化の実装


;;; 関数記号を含まない、変数定数に対する単一化だけ
;;; 先に書いてしまう


@export
(defgeneric unify (a b)
	(:documentation "get most general unifier"))


(defmethod unify ((t1 vterm) (t2 vterm))

  )
