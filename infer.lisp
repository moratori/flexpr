

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
  (list (list lexpr)))

(defmethod get-clause ((lexpr normal-lexpr) (op operator))
  (cond 
	((literal? lexpr)
	 (list (list lexpr)))
	((clause? lexpr (operator (opposite-opr op)))
	 ;; get leaf
	 (labels 
	   ((collect (lexpr result)
			(if (literal? lexpr)
			  (cons lexpr result)
			  (let ((left  (normal-lexpr-l-lexpr lexpr))
					(right (normal-lexpr-r-lexpr lexpr)))
				(cond 
				  ((and (literal? left)
						(literal? right)) 
				   (cons left (cons right result)))
				  ((literal? left)
				   (collect right (cons left result)))
				  ((literal? right)
				   (collect left (cons right result)))
				  (t (make-condition 'illformed-error
							:ie-mes "formalized formula(CNF/DNF) required."
							:ie-val lexpr
							:ie-where 'get-clause)))))))
	  (list (collect lexpr nil))))
	(t
	  (append
		(get-clause 
		  (normal-lexpr-l-lexpr lexpr) op)
		(get-clause 
		  (normal-lexpr-r-lexpr lexpr) op)))))

(defmethod get-clause ((lexpr lexpr) (op operator))
  (get-clause (lexpr-expr lexpr) op))

;;; 節形式は集合の集合
;;; {{P} , {P , Q} , {R , S} ...}
(defun convert (lexpr op)
  (assert (or (typep lexpr 'atomic-lexpr)
			  (typep lexpr 'normal-lexpr)
		      (typep lexpr 'lexpr)))

  (let ((formed (formalize lexpr op)))
	(unless (closed? formed)
		(error 
		  (make-condition 'illformed-error
			:ie-mes "closed formula required."
			:ie-val formed
			:ie-where 'convert)))	
	(get-clause formed op)))


