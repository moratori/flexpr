

(ns:defns flexpr.system.infer.general
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
	(:import-from :flexpr.system.util
				  :%literal=
				  :%clause=)
	(:import-from :flexpr.system.unifier
				  :eliminate?
				  :unify
				  :reverse-unify)
	(:import-from :flexpr.system.formalize.mat
				  :rename-clause)
	(:import-from :flexpr.system.formalize
								:remove-alphabet-equal)
	(:import-from :flexpr.system.error
				  :maximum-depth-exceeded-error
				  :undeterminable-error)
	(:import-from :flexpr.system.dump
				  :clause->string
				  :mgu->string)
	(:import-from :parallel
					:mp-some
	  )
	)

;;; 任意の一階述語論理式の集合に対して
;;; 充足不可能性を確認する. 



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
	  (values nil nil nil)
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


(defun clause*-len (clause*)
  (length (clause-%literals (second (second clause*)))))


;;; 導出後の節が最も小さくなるものを優先(これ必ずしもよくないので後で改良)
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


(defun choices (selected-clause all) 
	(cf-sort
	  (loop for each-c in all
			for rule = (multiple-value-list (resolution? each-c selected-clause)) 
			if (first rule)
			  collect (list each-c rule))))

;; 前までは 深度が1深まる毎にlimitを1減らしてたけど
;; 深くなればなるほどその枝の重要度を減らして行く事にする
;; それより幅優先で探索スべき? -> この線形探索結構効率いいと思う
(defun newdupf (limit)
  (lambda (x) 
	(- x 1 (floor (/ limit (+ x (/ limit 10)))))))

;;; 
;;; 導出に失敗 => unterminable error
;;; 導出に成功!
;;; 	1. conseqに存在量化子が含まれていた       => (t  result-list)
;;;     2. conseqに存在量化子は含まれていなかった => (t nil)
;;; 


(defmacro special-let* (bounds &rest body)
  ;;; 値 output が tの場合のみちゃんと束縛を作る
  ;;; nilの時はboundsをnilで埋める
  ;;; このマクロは output を意図的に自由変数としてもつ
  `(if output 
	 (let* ,bounds ,@body)
	 (let* 
	   ,(mapcar 
		  (lambda (x) 
			(destructuring-bind (sym expr) x
			  (list sym nil)
			  )
			) bounds)
	   ,@body)))

(defun dump-defnode (defnode)
  (mapcar 
	(lambda (x)
	  (destructuring-bind (lexpr . name) x
		;; ここの +OR+ は 節が何で結合してるかを表してる
		;; ;; 普通は V なので OR
		(cons (clause->string lexpr (operator +OR+)) name)))
	defnode))

;; Logical inference per second
(defun lips (total start)
  (/ total (- (get-universal-time) start)))


(defun lookupper (clause-form)
  (let*
	((prefix "NODE")
	 (defnode 
	   (mapcar 
		 (lambda (x)
		   (cons x (symbol-name (gensym prefix)))) clause-form))
	 (lookup 
	   (lambda (x)
		 (if (null x) 
		   (dump-defnode defnode) 			 
		   (let ((i (assoc x defnode :test #'%clause=)))
			 (if (null i)
			   (let ((name (symbol-name (gensym prefix)))) 
				 (setf defnode (acons x name defnode))
				 name)
			   (cdr i)))))))
	lookup))



;;; 結論となる節が複数ある場合、
;;; 並行してmainを呼ぶ
(defun call-main (main fuse-list universe-clause depth exist-terms)
  (some
		(lambda (c-clause)
			(handler-case 
				(funcall 
					main 
					(remove c-clause universe-clause :test #'%clause=)  
					nil
					c-clause 
					depth
					exist-terms
					(lookupper universe-clause)
					nil
					nil) 
	  (maximum-depth-exceeded-error (c) 
		(declare (ignore c)) nil)))	
	fuse-list))

@export
(defun resolution-gen (premises-clause-form 
					   conseq-clause-form 
					   original-exist-terms 
					   &key (depth +DEPTH+) 
					        (func (newdupf depth)) 
							(output nil))


  (labels 
		  ((main (clause-form     ;; 節集合
				  r-clause-form   ;; 今まで導出された全てを持つリスト
				  selected-clause ;; 先の導出(又は初期値として)選ばれた節
				  dep             ;; 現在の深さ 
				  exist-terms 
				  lookup
				  node-relation   ;; ノードとノードの関係
				  app-flag            ;; 導出済み(前提にはなかった節)をくっつけるべきかのflag
				  )
				 (when (> 1 dep)
				   (error (make-condition 'maximum-depth-exceeded-error
										  :mde-val depth
										  :mde-where 'resolution-gen)))


				 ;; choices には今までの導出で出てきた節は含まれていない
				 ;; choices* にはそれらが含まれている
				 (let*  ((choices  (choices selected-clause clause-form))
						 (checked  (unless app-flag 
									 (every (lambda (x) (> (clause-used x)  0)) clause-form)))
					     (choices* (append choices 
									;; clause-form の節がすでに一度以上使われた節であるなら
									;; いままでに導出されたやつをくっつける
									;; 毎回 every で clause-form を走査するのは非効率的なので
									;; 一回ここにきたらフラグたてる
									(when (or app-flag checked)
									   (choices selected-clause r-clause-form)))))

				   (some
					 (lambda (each-choice)

					;; ここでの clause が selected-clause とペアになる節					
					(destructuring-bind (clause (flag resolted mgu)) each-choice
						(special-let* 
						  ((selected-clause-name (funcall lookup selected-clause))
						   (opposite-clause-name (funcall lookup clause))
						   (resolted-clause-name (funcall lookup resolted))
						   (relation 
							 (append 
							   (list (list selected-clause-name 
										   resolted-clause-name)
									 (list opposite-clause-name 
										   resolted-clause-name))
							   node-relation)))
					  
						  (cond 
							((and flag (null (clause-%literals resolted))) 
							 
							 (let ((base 
									(list 
									  t 
									  original-exist-terms 
									  (reverse-unify  exist-terms mgu))))
							  (if output 
								(append 
								  base 
								  (list 
									(funcall lookup nil)
									relation 
										))
								base)))
							
							(t 
							  
							  (handler-case 
							   (main 
								 
								 (if (member clause clause-form :test #'%clause=)
								   (append 
									 (remove clause clause-form :test #'%clause=)
									 (list ;; append するための list
									   (clause 
										 (rename-clause (clause-%literals clause)) 
									     (1+ (clause-used clause)))))
									 clause-form)
							     
								 (adjoin 
								   (clause 
								     (clause-%literals selected-clause)
								     (1+ (clause-used selected-clause)))
								   r-clause-form
								   :test #'%clause=)

							     resolted
							     (funcall func  dep)
							     (reverse-unify exist-terms mgu)
							     lookup
							     relation
								 (or app-flag checked))
							 (maximum-depth-exceeded-error (c)
								(declare (ignore c)) nil))))))) 
					 choices*))))


		(let* ((clause-form (append premises-clause-form conseq-clause-form))
			   (res1 (call-main 
				 	  #'main
					  conseq-clause-form 
					  clause-form
					  depth
					  original-exist-terms))
			   (res2 (when (null res1)
					   (call-main 
						 #'main
						 (sort premises-clause-form
						   (lambda (c1 c2) 
					         (< (length (clause-%literals c1))
						        (length (clause-%literals c2)))))
						 clause-form
						 depth
						 original-exist-terms))))
		  (unless (or res1 res2)
			(error (make-condition 'undeterminable-error)))
		  (or res1 res2))))

