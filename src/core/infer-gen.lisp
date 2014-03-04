

(ns:defns flexpr.infer.general
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :flexpr.util
				  :%literal=
				  :%clause=)
	(:import-from :flexpr.unifier
				  :eliminate?
				  :unify
				  :reverse-unify)
	(:import-from :flexpr.formalize
				  :formalize
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
  (multiple-value-bind 
	(lexpr exist-terms) 
	(convert (normal-lexpr (operator +NEG+) conseq nil) mat-form quants-form )
	(values 
	  (loop for each in 
	    (mapcar 
		  (lambda (x) 
			(convert x mat-form quants-form)) premises)
			append each)
	  lexpr
	  exist-terms)))

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
					 0))
			rule
			)))))

;; もっとも使われてない節を優先
(defun primary-unused (clause-form*)
  (sort clause-form*
		(lambda (x y)
		  (< 
			(clause-used (first x))
			(clause-used (first y))))))

;;; 導出後の節が最も小さくなるものを優先
(defun primary-short (clause-form*)
  (sort clause-form*
		(lambda (x y)
		  (< 
			(length
			  (clause-%literals 
				(second (second x))))
			(length 
			  (clause-%literals
				(second (second y))))))))


(defun cf-sort (clause-form*)
  (primary-short clause-form*))


(defun addrule (acc new)
  (if (not (eq new t)) 
	(append acc new)
	acc ))


;;; 
;;; 導出に失敗 => unterminable error
;;; 導出に成功!
;;; 	1. conseqに存在量化子が含まれていた       => (t  result-list)
;;;     2. conseqに存在量化子は含まれていなかった => (t nil)
;;; 

@export
(defun resolution (premises conseq &optional (depth +DEPTH+))
  (multiple-value-bind 
	(premises-clause-form conseq-clause-form exist-terms)
	(preproc premises conseq)
	
	(let ((clause-form (append premises-clause-form
							   conseq-clause-form))
		  ;; これは副作用なので扱い注意
		  (specific-term nil))
	  	
	    (labels 
		  ((main (clause-form selected-clause dep substrule)
				 (when (zerop dep)
				   (error (make-condition 'maximum-depth-exceeded-error
										  :mde-val depth
										  :mde-where 'resolution)))
				 (let ((best
						 (cf-sort
						   (loop for each-c in clause-form
								 for rule = (multiple-value-list (resolution? each-c selected-clause)) 
								 if (not (null (first rule)))
								   collect (list each-c rule)))))

				   (some 
					 (lambda (best)
					;; ここでの best が selected-clause とペアになる節
					
					(destructuring-bind (clause (flag resolted mgu)) best
					  (cond 
						 ((and flag (null (clause-%literals resolted))) 
						  (setf specific-term
								(reverse-unify 
								  exist-terms
								  (addrule substrule mgu))) 
						  t)

						 (t 
						   (handler-case 
							 (main 
							   (append 
							     (remove clause clause-form :test #'%clause=)
								 (list 
								   (clause 
								   (clause-%literals clause)
								   (1+ (clause-used clause)))))
							   resolted
							   (1- dep)
							   (addrule substrule mgu))
							 (maximum-depth-exceeded-error (c)
								(declare (ignore c)) nil)))))) best))))

		  (or
			;; 結論の否定を前提の節に作用させれば
			;; 手っ取り早く矛盾を導けるだろうというヒューリスティクスが以下
		    (when (some 
					(lambda (c-clause)
					  (handler-case 
						(main 
						  (remove c-clause clause-form :test #'%clause=)  
				          c-clause 
					      depth
					      nil) 
					  (maximum-depth-exceeded-error (c) 
						(declare (ignore c)) nil))) 
					conseq-clause-form)
			  (list t specific-term))	

			  (format t "!! SECOND RESOLUTION !!~%")
			
			;; 上の経験則は必ずしも成り立たないので
			;; こっから全探索
			;; まだテストが少ないからなんとも言えないけど
			;; 上のやつで、(52/54)はうまくいってしまう

			(when (some 
			  	  ;; 前提の節についての探索を行う
				  (lambda (p-clause)
					(handler-case 
						(main 
						  (remove p-clause clause-form :test #'%clause=)
						  	p-clause
					  		depth
							nil)
					  (maximum-depth-exceeded-error (c)
						(declare (ignore c)) nil))) 
				  (sort 
					premises-clause-form
					(lambda (c1 c2) 
					  (< (length (clause-%literals c1))
						 (length (clause-%literals c2))))))
			  (list t specific-term))
			
			(error (make-condition 'undeterminable-error)))))))

