

(ns:defns flexpr.infer
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :flexpr.util
				  :%literal=
				  :%clause=)
	(:import-from :flexpr.unifier
				  :eliminate?
				  :unify)
	(:import-from :flexpr.formalize
				  :convert)
	(:import-from :flexpr.error
				  :maximum-depth-exceeded)
	)


;;;  効率的な導出を実装
;;;  導出原理つかうかタブロー使うか
;;;  他の使うか



;; 
;; (values {C1,C2,C3...} {C1,C2,C3...})
;; 上の合併集合から空節が求まればおｋ
(defun preproc (premises conseq &optional (quants-form +FORALL+) (mat-form (operator +AND+)))
  (values
	(loop for each in 
		  (mapcar 
			(lambda (x) 
			  (convert x mat-form quants-form)) premises)
		  append each)
	(convert 
	  (normal-lexpr (operator +NEG+) conseq nil)
	  mat-form quants-form)))

;; (values flag result)
;; flag で導出できたかいなか
;; result clause1 と clause2 を融合した結果
(defun resolution? (clause1 clause2)
  ;; 節 clause1 と clause2 が融合できるか
  ;; できる場合には 単一化した次の節を返してしまう
  ;; clause1 と clause2 を走査して お互いに単一化によって逆になるもの
  ;; 最初の物を考える
  (assert (and (typep clause1 'clause)
			   (typep clause2 'clause)))
  (let ((res 
   ;; -- dirty code --
   (loop named exit
		 for x in (clause-%literals clause1)
		 do 
		 (loop for y in (clause-%literals clause2 )
			   for rule = (eliminate? x y)
			   do (unless (null rule)
					(return-from exit (list rule x y)))))))
	(if (null res)
	  (values nil nil)
	  (destructuring-bind (rule lit1 lit2) res
		(values 
		  t
			(unify rule 
				   (clause
					 (append 
					   (remove lit1 (clause-%literals clause1) :test #'%literal=) 
					   (remove lit2 (clause-%literals clause2) :test #'%literal=))
					 0)))))))




@export
(defun resolution (premises conseq &optional (depth +DEPTH+))
  (multiple-value-bind 
	(premises-clause-form conseq-clause-form)
	(preproc premises conseq)
	(let ((clause-form (append premises-clause-form
							   conseq-clause-form)))
	  ;; clause-form が矛盾していることを 導く	
	  	(labels 
		  ((main (clause-form selected-clause dep)
				 ;(format t "SELECTED~%~A~%~%" (flexpr.dump::clause->string selected-clause (operator +OR+)))
				 ;(sleep 0.5)
				 (when (zerop dep)
				   (error (make-condition 'maximum-depth-exceeded
										  :mde-val depth
										  :mde-where 'resolution)))
				 (let ((best
					    (iterate:iter 
						  	  (iterate:for each iterate:in 
								(loop for each-c in clause-form 
								      for rule = (multiple-value-list (resolution? each-c selected-clause)) 
								      if (not (null (first rule)))
								        collect (list each-c rule)))
							  ;; minimizing されて最も短い節としか融合されなくなるってことか...
							  (iterate:finding each iterate:minimizing  
								(destructuring-bind (clause (flag result)) each
							  		(length (clause-%literals result)))))))

				  (if (null best) nil
					;; ここでの clause が selected-clause とペアになる節
					(destructuring-bind (clause (flag result)) best
					  (cond 
						 ((and flag
							   (null (clause-%literals result))) t)
						 (t 
						   (main 
							 (append 
								(remove clause clause-form :test #'%clause=)
								(list 
								  (clause 
									(clause-%literals clause)
									(1+ (clause-used clause)))))
							 result
							 (1- dep)))))))))

		  (some 
			(lambda (c-clause)
			  (assert (typep c-clause 'clause))
			  (main 
				(remove c-clause clause-form :test #'%clause=) 
				c-clause
				depth
				)) 
			conseq-clause-form)))))



