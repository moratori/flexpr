

(ns:defns flexpr.system.infer.sld
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
	(:import-from :flexpr.system.util
				  :%clause=)
	(:import-from :flexpr.system.formalize.mat
				  :rename-clause)
	(:import-from :flexpr.system.unifier
				  :eliminate?
				  :unify)
	(:import-from :flexpr.system.error
				  :maximum-depth-exceeded-error
				  :undeterminable-error))


(defun resolution? (goal-clause clause)
  (assert (and (typep goal-clause 'clause)
			   (typep clause 'clause)))

  (let ((left-literal (car (clause-%literals goal-clause))))
	;; clause が規則節であり、 ヘッドがleft-literalと融合可能である
	;; clauseが事実節であり、left-literalと融合可能である

	;; (flag resolted rule)
	(let ((rule (eliminate? 
				  left-literal
				  (car (clause-%literals clause)))))
	  (if (null rule)
		(list nil nil nil)
		(list t
				(unify 
				  rule
				  (clause 
					(append 
					  (cdr (clause-%literals goal-clause))
					  (cdr (clause-%literals clause)))
					0)) 
				rule)))))



;; もっとも使われてない節を優先
(defun primary-unused (clause-form*)
  (sort clause-form*
		(lambda (x y)
		  (< 
			(clause-used (first x))
			(clause-used (first y))))))


(defun clause*-len (clause*)
  (length (clause-%literals (second (second clause*)))))


;;; 導出後の節が最も小さくなるものを優先
;;; 但しそれも節の大きさが同じ場合、使われたことの回数が少ない
;;; 節を優先して使う
(defun primary-short (clause-form*)
  (if (null clause-form*) nil
	(let ((flen (clause*-len (car clause-form*))))
	  (if (every (lambda (x) (= (clause*-len x) flen)) clause-form*) 
		(primary-unused clause-form*)
		(sort clause-form*
			  (lambda (x y)
				(< 
				  (length
					(clause-%literals 
					  (second (second x))))
				  (length 
					(clause-%literals
					  (second (second y)))))))))))


(defun cf-sort (clause-form*)
  (primary-short clause-form*))



(defun choices (goal-clause all)
  (cf-sort
	(loop for each-c in all
		for res = (resolution? goal-clause each-c)
		if (first res)
		collect (list each-c res))))


;; 正リテラルを全て左に持ってくる
(defun normalize (premises-clause-form)
  (mapcar 
	(lambda (clause)
	  (clause
		(cons
		  ;; 当該リテラル１つを見っけて左にもってくるだけなので
		  ;; これだと効率はよくないことに注意
		  (find-if 
			(lambda (l) 
			  (not (%literal-negation l)))
			(clause-%literals clause))
		  (remove-if 
			(lambda (l)
			  (not (%literal-negation l)))
			(clause-%literals clause)))
		0))
	premises-clause-form))


@export
(defun resolution-sld (premises-clause-form 
					   conseq-clause-form 
					   exist-terms
					   &key 
					     (depth +DEPTH+)
						 (output nil))
  
  (labels 
	((main (clause-form 
			r-clause-form 
			goal-clause
			dep)

 			(when (> 1 dep)
			  (error (make-condition 'maximum-depth-exceeded-error
									 :mde-val depth
								     :mde-where 'resolution-sld)))

			(let* ((choices (choices goal-clause clause-form))
				   (choices* 
					 (append choices
							 (when (every (lambda (x) (> (clause-used x) 0)) clause-form)
								(choices goal-clause r-clause-form)
							   )
							 )))
			  
			  (some 
				(lambda (cand)
				  (destructuring-bind (clause (flag resolted mgu)) cand
					(cond
					  ((and flag (null (clause-%literals resolted)))
					   t)
					  
					  (t 
						(main 
							(append 
							  (remove clause clause-form :test #'%clause=)
							  (list ;; append するための list
								(clause 
								  (rename-clause (clause-%literals clause)) 
								  (1+ (clause-used clause)))))
							
							(adjoin 
							  (clause 
								(clause-%literals goal-clause)
								(1+ (clause-used goal-clause)))
							  r-clause-form
							  :test #'%clause=)

							resolted
							(1- dep)

						  )
						))))
				choices)
			  
			  )))
	(main 
	  (normalize premises-clause-form)
	  nil
	  ;; ゴール節は１つなので
	  (car conseq-clause-form)
	  depth
	  )))


