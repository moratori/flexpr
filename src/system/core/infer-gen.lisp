

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
				  :formalize
				  :convert)
	(:import-from :flexpr.system.error
				  :maximum-depth-exceeded-error
				  :undeterminable-error)
	(:import-from :flexpr.system.dump
				  :clause->string
				  :mgu->string)
	)


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


(defun choices (selected-clause all) 
	(cf-sort
	  (loop for each-c in all
			for rule = (multiple-value-list (resolution? each-c selected-clause)) 
			if (not (null (first rule)))
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
	   ,@body
	   )
	 )
  )

@export
(defun resolution-gen (premises conseq &key (depth +DEPTH+) (func (newdupf depth)) (output t))
  (assert (and 
			(every (lambda (x) (typep x 'lexpr-type)) premises)
			(typep conseq 'lexpr-type)))

  (multiple-value-bind 
	(premises-clause-form conseq-clause-form exist-terms)
	(preproc premises conseq)
	
	(let ((clause-form (append premises-clause-form conseq-clause-form))
		  (specific-term nil)
		  (defnode nil)
		  (noderel nil))

	    (labels 
		  ((main (clause-form     ;; 節集合
				  r-clause-form   ;; 今まで導出された全てを持つリスト
				  selected-clause ;; 先の導出(又は初期値として)選ばれた節
				  dep             ;; 現在の深さ 
				  exist-terms 
				  lookup
				  node-relation   ;; ノードとノードの関係
				  )
				 (when (> 1 dep)
				   (error (make-condition 'maximum-depth-exceeded-error
										  :mde-val depth
										  :mde-where 'resolution-gen)))

				 ;; choices には今までの導出で出てきた節は含まれていない
				 ;; choices* にはそれらが含まれている
				 (let*  ((choices  (choices selected-clause clause-form))
					     (choices* (append choices 
										   ;; clause-form の節がすでに一度以上使われた節であるなら
										   ;; いままでに導出されたやつをくっつける
										   (when (every (lambda (x) (> (clause-used x)  0)) clause-form)
											 (choices selected-clause r-clause-form)))))

				   (some 
					 (lambda (each-choice)

					;; ここでの clause が selected-clause とペアになる節					
					(destructuring-bind (clause (flag resolted mgu)) each-choice
						(special-let* 
						  ((selected-clause-name (funcall lookup selected-clause))
						   (opposite-clause-name (funcall lookup clause))
						   (resolted-clause-name (funcall lookup resolted))
						   (mgu-str 
							 (mgu->string mgu))
						   (relation 
							 (append 
							   (list (list selected-clause-name resolted-clause-name mgu-str)
									 (list opposite-clause-name resolted-clause-name mgu-str))
							   node-relation)))
					  
						  (cond 
							((and flag (null (clause-%literals resolted))) 
							 (print relation)
							 (print (funcall lookup nil))
							 
							 ;; output が真なら最後結果値を返すために
							 ;; setfする. したの 一番下の some で listに包んで返す必要あり
							 (when output
							   ;; nil でlookupを呼ぶと今までためたplistを返す
							   (setf noderel  relation
								     defnode  (funcall lookup nil)))

							 (setf specific-term (reverse-unify  exist-terms mgu)) 
							 t)
							
							(t 
							  
							  (handler-case 
							   (main 
								 (append 
								   (remove clause clause-form :test #'%clause=)
								   (list ;; append するための list
									 (clause 
									   (rename-clause (clause-%literals clause)) 
									   (1+ (clause-used clause)))))
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
							     relation)
							 (maximum-depth-exceeded-error (c)
								(declare (ignore c)) nil))))))) 
					 choices*))))

		 ;; 以下の let 束縛はグラフにするための
		 ;; lookup : LEXPR -> UNIQUE_NAME
		(let* ((prefix "NODE")
			   (defnode 
				(when output 
				  (mapcar 
					(lambda (x)
					  (cons x (symbol-name (gensym prefix)))) clause-form)))
			   (lookup 
				 (lambda (x)
				   (if (null x) 
					 (mapcar 
					   (lambda (x)
						 (destructuring-bind (lexpr . name) x
						   ;; ここの +OR+ は 節が何で結合してるかを表してる
						   ;; 普通は V なので OR
						   (cons (clause->string lexpr (operator +OR+)) name)))
					   defnode)
					 (let ((i (assoc x defnode :test #'%clause=)))
					   (if (null i)
						 (let ((name (symbol-name (gensym prefix)))) 
						   (setf defnode (acons x name defnode))
						   name)
						 (cdr i)))))))
		(or
			;; 結論の否定を前提の節に作用させれば
			;; 手っ取り早く矛盾を導けるだろうというヒューリスティクスが以下
		    (when (some 
					(lambda (c-clause)
					  (handler-case 
						(main 
						  (remove c-clause clause-form :test #'%clause=)  
						  nil
				          c-clause 
					      depth
						  exist-terms
						  lookup
						  nil) 
					  (maximum-depth-exceeded-error (c) 
						(declare (ignore c)) nil))) 
					conseq-clause-form)
			  (list t exist-terms specific-term))	


			(when (some 
			  	  ;; 前提の節についての探索を行う
				  (lambda (p-clause)
					(handler-case 
						(main 
						    (remove p-clause clause-form :test #'%clause=)
							nil
						  	p-clause
					  		depth
							exist-terms
							lookup
							nil)
					  (maximum-depth-exceeded-error (c)
						(declare (ignore c)) nil))) 
				  (sort 
					premises-clause-form
					(lambda (c1 c2) 
					  (< (length (clause-%literals c1))
						 (length (clause-%literals c2))))))
			  (list t exist-terms specific-term))
			
			(error (make-condition 'undeterminable-error))))))))

