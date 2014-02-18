

(ns:defns flexpr.infer
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct
		  :flexpr.unifier)
	(:import-from :flexpr.formalize
				  :formalize))


;;;  効率的な導出を実装
;;;  導出原理つかうかタブロー使うか
;;;  他の使うか


(defmethod getclause ((lexpr atomic-lexpr))
  (list lexpr))

(defmethod getclause ((lexpr normal-lexpr))
  ;; get leaf
  )

(defmethod getclause ((lexpr lexpr))
  (getclause (lexpr-expr lexpr)))

;;; 節形式は集合の集合
;;; {{P} , {P , Q} , {R , S} ...}
(defun convert (lexprs)
  (assert (every (lambda (x)
				   (or (typep x 'atomic-lexpr)
					   (typep x 'normal-lexpr)
					   (typep x 'lexpr))) lexprs))
  (mapcar (lambda (x) 
			(formalize x)
			) 
		  lexprs)
  )







