

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
				  :%formalize
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

  (let* ((convp 
		  (loop for each in 
		  	(mapcar 
			  (lambda (x) 
			  	  (convert x mat-form quants-form)) premises)
			append each))
		(convf 
		  (%formalize conseq mat-form nil)))
	;; conseq から量化子だけを上にもってきてやるようにしなきゃだめだ
	;; 否定つけて %formalize したら全称量化に化けるに決まってるじゃん
	;; そうなる前の初めの状態の存在量化の変数をもってこなければ
	(values convp (convert (normal-lexpr (operator +NEG+) convf nil) mat-form quants-form t)
			(when (lexpr-p convf)
			  (loop for eachq in (quantsp-each-quant (lexpr-qpart convf))
					if (eq (quant-qnt eachq) +EXISTS+)
					collect (quant-var eachq))))))

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


@export
(defun resolution (premises conseq &optional (depth +DEPTH+))
  (multiple-value-bind 
	(premises-clause-form conseq-clause-form exist-terms)
	(preproc premises conseq)
	
	(print exist-terms)

	(let ((clause-form (append premises-clause-form
							   conseq-clause-form)))
	  ;; clause-form が矛盾していることを 導く	
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
					;; ここでの clause が selected-clause とペアになる節
					
					;; main は maximum-depth-exceeded-error お シグナルするわけだけど
					;; ここで 受けないとhandler-caseが待ってる呼び出しまで戻っっちゃうきがする
					;; つまり結論の否定を初めに一個ずつ呼ぶわけだけど
					;; それを A として A と融合可能な節々(B C D E)が best に束縛され
					;; それらの各々をここの some で受け取るんだけど
					;; B で main を呼んだ時に error　をシグナリングされると
					;; 残ってる C D E が実行されないうちに handler-case まで戻っちゃっ手
					;; 結局 A は失敗だったって事になってしまうので
					;; 下の main で handler-case するべきかもしれん
					(destructuring-bind (clause (flag resolted mgu)) best
		;			  (format t "SELECTED-CLAUSE: ~A~%" (flexpr.dump::clause->string selected-clause (operator +OR+))  )
		;			  (format t "PAIR-CLAUSE: ~A~%" (flexpr.dump::clause->string clause (operator +OR+)) )
		;			  (format t "RESOLUTED: ~A~%~%" (flexpr.dump::clause->string resolted (operator +OR+)))
		;			  (sleep 0.3)
					  (cond 
						 ((and flag
							   (null (clause-%literals resolted))) 
						  ;;
						  ;; 存在量化されていた項 exist-terms
						  ;; が最終的になにに置換されたのかを
						  ;; 以下の substrule とから求めて
						  ;; values で t と返す
						  ;;
						  (print (cons mgu substrule))
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
							 (if (not (eq mgu t)) 
							   (cons mgu substrule)
							   substrule))
							 (maximum-depth-exceeded-error (c)
								(declare (ignore c))
							;	(format t "!!! GUARD INNER ERROR !!!~%")
								nil
								)
							 )
						   )))) best))))

		  (or
			;; 結論の否定を前提の節に作用させれば
			;; 手っ取り早く矛盾を導けるだろうというヒューリスティクスが以下
			(some 
			  (lambda (c-clause)
				(handler-case 
				 	(main 
				  		(remove c-clause clause-form :test #'%clause=)  
						c-clause 
						depth
						nil) 
				(maximum-depth-exceeded-error (c) 
					(declare (ignore c)) 
					;(format t "!!! GUARD OUTER ERROR1 !!!~%")
					nil))) 
				conseq-clause-form)

			 ; (format t "!! SECOND RESOLUTION !!~%")
			
			;; 上の経験則は必ずしも成り立たないので
			;; こっから全探索

			(some 
			  	  ;; 前提の節についての探索を行う
				  (lambda (p-clause)
					(handler-case 
						(main 
						  (remove p-clause clause-form :test #'%clause=)
						  	p-clause
					  		depth
							nil)
					  (maximum-depth-exceeded-error (c)
						(declare (ignore c)) 
				;		(format t "!!! GUARD OUTER ERROR2 !!!~%")
						nil))) 
				  (sort 
					premises-clause-form
					(lambda (c1 c2) 
					  (< (length (clause-%literals c1))
						 (length (clause-%literals c2))))))
			
			(error (make-condition 'undeterminable-error)))))))

