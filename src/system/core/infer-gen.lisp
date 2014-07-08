

(ns:defns flexpr.system.infer.general
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
	(:import-from :flexpr.system.util
				  :%literal=
          :set-defined-flag
          :equal-literal?
          :substitute-term
          :equal-contap?
				  :%clause=)
	(:import-from :flexpr.system.unifier
				  :eliminate-gen?
				  :unify
				  :reverse-unify)
  (:import-from :flexpr.system.paramod-unifier
   :eliminate-paramod?
   :paramod-unify)
	(:import-from :flexpr.system.formalize.mat
				  :rename-clause)
	(:import-from :flexpr.system.formalize
								:remove-alphabet-equal)
	(:import-from :flexpr.system.error
				  :maximum-depth-exceeded-error
				  :undeterminable-error)
	(:import-from :flexpr.system.dump
          :deb-trace-resolution
				  :clause->string
				  :mgu->string)
	)

;;; 任意の一階述語論理式の集合に対して
;;; 充足不可能性を確認する. 



(defun get-rule (clause1 clause2)
 
  (loop named exit
                for x in (clause-%literals clause1)
                do 
        (loop for y in (clause-%literals clause2 )
              for rule-g = (eliminate-gen? x y)
              for (rule-p target old new) = (multiple-value-list (eliminate-paramod? x y))
              do 
              ;; 導出原理が使える時は
              ;; paramodulationよりもそっちを優先
              ;; ずーっと導出原理のほうに偏って詰む
              ;; どちらも等号でかつ導出原理が使えるなら
              ;; 導出原理を優先なぜなら消えるから
             (cond 
               ((and (equal-literal? x)
                     (equal-literal? y)
                     rule-g)
                 (return-from exit (list nil rule-g x y)))
               (rule-p
                 ;(setf paramod-flag t)
                 (return-from exit (list t rule-p x y target old new)))
               (rule-g
                 (return-from exit (list nil rule-g x y))))))
  
  )


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

  (let ((res (get-rule clause1 clause2)))
    (force-output *standard-output*)

    (when res
      (destructuring-bind (paramod-flag rule lit1 lit2 . more) res
        (let ((others 
                (remove-if 
                  (lambda (x) 
                    (or (%literal= x lit1)
                        (%literal= x lit2))) 
                  (append 
                    (clause-%literals clause1)
                    (clause-%literals clause2)))))

          (values 
            t

            (if paramod-flag 
              (destructuring-bind (target old new) more
                ;; ここに来たって事は rule は rec-matchで生成されたもの
                ;; だよね
                ;; ここらへんでの clause 生成しまくるのかなり無駄だ
                (let ((target 
                        (car (clause-%literals (unify rule (clause (list target) 0))))
                        ))
                  (assert (typep target '%literal)) 

               
               (let ((unified 
                               (paramod-unify (cons old new) target)))
                 (mapcar 
                   (lambda (lit)
                     (clause (cons lit others) 0))
                   unified))))

              (list (unify rule (clause others 0))))
          rule))))))


;; もっとも使われてない節を優先
(defun primary-unused (clause-form*)
  (sort clause-form*
		(lambda (x y)
		  (< 
			(clause-used (first x))
			(clause-used (first y))))))


(defun clause*-len (clause*)
  (length 
    (clause-%literals 
      (car (second (second clause*))))))


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
            (clause*-len x)
            (clause*-len y))))))))

;; 複雑度という指数をいれたらより
;; ヒューリスティックな探索ができるかも

(defun cf-sort (clause-form*)
  (primary-short clause-form*))


(defun choices (selected-clause all) 

  (cf-sort
	  (loop for each-c in all
			    for rule = (multiple-value-list (resolution? each-c selected-clause)) 
			    if (first rule)
			    collect (list each-c rule)))
  
  )

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
		(list
      (clause->string lexpr (operator +OR+)) 
      name
      (clause-defined lexpr))))
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
		   (cons x (symbol-name (gensym prefix)))) 
     (mapcar #'set-defined-flag clause-form)
     ))
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
		  ((main 
				 (clause-form     ;; 節集合
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


				 (let*  ((choices  
									 (choices selected-clause clause-form))
								 (checked  
									 (unless app-flag 
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
            ;; resolted は導出された節のリストであることにした
            ;; なぜなら paramodulation で全てのパターンの置き換えパターンが生じるから
            (assert (typep resolted 'list))
            
            (deb-trace-resolution mgu clause selected-clause resolted)  
                    
						(special-let* 
						  ((selected-clause-name 
                 (funcall lookup selected-clause))
						   (opposite-clause-name 
                 (funcall lookup clause))
						   (resolted-clause-name-list 
                 (mapcar (lambda (x)(funcall lookup x) ) resolted))
						   (relation
                 (append 
                   (list (list selected-clause-name 
                               (car resolted-clause-name-list))
									 (list opposite-clause-name 
                         (car resolted-clause-name-list)))
                   node-relation)))

					  
              (let ((resolted1 (car resolted)))
                
                (cond 
                ;; 空節がでるか
                ;; ~=(x,x) が出たら終了
                ;; equal-contap? によって矛盾であるかを調べるのは 等号公理を入れればいらないのでは
                ;; ただ、これで特化してる方がこれはこれでいい気もする
                ((or (and flag (null (clause-%literals resolted1)))
                     (some #'equal-contap? resolted)) 
                 
                   (let ((base (list t original-exist-terms (reverse-unify  exist-terms mgu))))
                     (if output 
                         (append base (list (funcall lookup nil) relation))
                         base)))
                
                (t
                 (handler-case 
                   (main 
                     ;; 節 clause は selected-clause と導出に使われる親節であり
									   ;; resolted-clause を生むもの
									   ;; この辺で吸収戦略したい

                    (if (member clause clause-form :test #'%clause=)

                       (append 
                         (remove clause clause-form :test #'%clause=)
                         (list ;; append するための list
                           (set-defined-flag
                             (clause 
                               (rename-clause (clause-%literals clause)) 
                               (1+ (clause-used clause)))
                             (clause-defined clause))))

                       clause-form) 
                     
                     (append 
                       (adjoin 
                       (set-defined-flag
                         (clause 
                           (clause-%literals selected-clause)
                           (1+ (clause-used selected-clause)))
                         (clause-defined selected-clause)) 
                       r-clause-form
                       :test #'%clause=)
                       (cdr resolted)
                       )
                     
                     resolted1

                     (funcall func  dep)

                    (reverse-unify exist-terms mgu) 

                     lookup

                     relation

                     (or app-flag checked))

                   (maximum-depth-exceeded-error (c)
                     (declare (ignore c)) nil)))))))) 
             choices*))))


		(let* ((clause-form (append premises-clause-form conseq-clause-form))

         ;; まず入力節自体が等号に関して矛盾していないかチェック

			   (res1 (call-main 
				 	  #'main
					  conseq-clause-form 
					  clause-form
					  depth
					  original-exist-terms))
			   (res2 (when (null res1)
							;; ユーザ入力節がなくても矛盾しているような
							;; 節集合はこっちに落ちてくる
							;; なぜなら、線形戦略は
							;; S - {C} が充足可能で S が矛盾しているならば
							;; C(つまりユーザの入力)を頂節とする導出反駁図を
							;; 必ず導いてくれる完全な戦略だから
					   (call-main 
						 #'main
						 (sort premises-clause-form
						   (lambda (c1 c2) 
					         (< (length (clause-%literals c1))
						        (length (clause-%literals c2)))))
						 clause-form
						 depth
						 original-exist-terms))))
		  
		  (or
        (some 
         (lambda (clause)
            (when (equal-contap? clause)
                  (list t nil nil nil nil))) clause-form)
        res1
        res2
        (error (make-condition 'undeterminable-error))))))


