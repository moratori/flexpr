

(ns:defns flexpr.infer
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :flexpr.error
				  :struct-unmatch-error
				  :illformed-error)
	(:import-from :flexpr.unifier
				  :mgu)
	(:import-from :flexpr.formalize
				  :formalize)
	(:import-from :flexpr.util
				  :literal?
				  :opr-equal?
				  :closed?
				  :clause?
				  :opposite-opr))


;;;  効率的な導出を実装
;;;  導出原理つかうかタブロー使うか
;;;  他の使うか


(defgeneric get-clause (a b)
	(:documentation "b is operator that expressing which formalization"))

(defmethod get-clause ((lexpr atomic-lexpr) (op operator))
  )

(defmethod get-clause ((lexpr normal-lexpr) (op operator))
  )

(defmethod get-clause ((lexpr lexpr) (op operator))
  )



;;; 節形式は集合の集合
;;; {{P} , {P , Q} , {R , S} ...}
(defun convert (lexprs op)
  (assert (every (lambda (x)
				   (or (typep x 'atomic-lexpr)
					   (typep x 'normal-lexpr)
					   (typep x 'lexpr))) lexprs))
  (mapcar (lambda (x) 
			(let ((formed (formalize x op)))
			  (unless (closed? formed)
				(error 
				  (make-condition 'illformed-error
								  :ie-mes "closed formula required."
								  :ie-val formed
								  :ie-where 'convert)))
			  (get-clause formed op))) 
		  lexprs))







