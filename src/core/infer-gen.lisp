

(ns:defns flexpr.infer.general
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
				  :maximum-depth-exceeded-error
				  :undeterminable-error))


;;; 任意の一階述語論理式の集合に対して
;;; 充足不可能性を確認する. 




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
				 
				 (when (zerop dep)
				   (error (make-condition 'maximum-depth-exceeded-error
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
							  ;; ループの原因っぽくなりそうな匂いがする
							  ;; ここで一番短くなるやつを選ぶんじゃなくて他の選択肢でも some すればおｋ?
							  (iterate:finding each iterate:minimizing  
								(destructuring-bind (clause (flag result)) each
							  		(length (clause-%literals result)))))))

				 (if (null best) nil
					;; ここでの clause が selected-clause とペアになる節
					;; 導出されたのが result 節
					(destructuring-bind (clause (flag result)) best
					  ;(format t "~A~%" (flexpr.dump::clause->string result (operator +OR+)))
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

		;; 以下の２つの探索(someのやつ)で
		;;

		  (or
			;; 結論の否定を前提の節に作用させれば
			;; 手っ取り早く矛盾を導けるだろうというヒューリスティクスが以下
			(some 
			  (lambda (c-clause)
				(handler-case 
				 	(main 
				  		(remove c-clause clause-form :test #'%clause=)  
						c-clause 
						depth) 
				(maximum-depth-exceeded-error (c) 
					(declare (ignore c)) nil))) 
				conseq-clause-form)
				
			(some 
			  	  ;; 前提の節についての探索を行う
				  (lambda (p-clause)
					(handler-case 
						(main 
						  (remove p-clause clause-form :test #'%clause=)
						  	p-clause
					  		depth)
					  (maximum-depth-exceeded-error (c)
						(declare (ignore c)) nil))) 
				  (sort 
					premises-clause-form
					(lambda (c1 c2) 
					  (< (length (clause-%literals c1))
						 (length (clause-%literals c2))))))
			
			(error (make-condition 'undeterminable-error)))))))

