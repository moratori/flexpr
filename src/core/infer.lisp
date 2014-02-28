

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
				  :convert))


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



(defun primary-order (clause-form)
  clause-form)


@export
(defun resolution (premises conseq)
  (multiple-value-bind 
	(premises-clause-form conseq-clause-form)
	(preproc premises conseq)
	(let ((clause-form (append premises-clause-form
							   conseq-clause-form)))
	  ;; clause-form が矛盾していることを 導く	
	  	(labels 
		  ((main (clause-form selected-clause)
				(loop named exit 
					  for each in (primary-order clause-form)
					  do
					  (multiple-value-bind 
						;; 導出できる節でもっとも導出後に小さくなるものが好ましい
						;; これ実装しないとかなりあれっぽい.
						;; テストでうまく動かないのはこれやってないからっぽい(手計算済み)
						(flag result-clause) (resolution? each selected-clause)
						(assert (typep each 'clause))
						(cond 
						  ((and flag (null (clause-%literals result-clause)))
						   (return-from exit t))
						  ((null flag) )
						  (t 
							(return-from exit 
								(main 
									;;今回使った節を一番後ろに入れることで
									;;節をローテートさせてるけど、節の数がでかくなったら
									;;効率悪いので何回使ったかカウントしないとだめ
									(append 
										(remove each clause-form :test #'%clause=)
										(list 
										  (clause 
											(clause-%literals each)
											(1+ (clause-used each)))))	
										result-clause))))))))
		  (some 
			(lambda (c-clause)
			  (assert (typep c-clause 'clause))
			  (main (remove c-clause clause-form :test #'%clause=) c-clause)) 
			conseq-clause-form)))))



