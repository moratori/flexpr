

(ns:defns flexpr.system.infer.snl
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
	(:import-from :flexpr.system.util
      :set-defined-flag
      :%clause=)
	(:import-from :flexpr.system.formalize.mat
				  :rename-clause)
	(:import-from :flexpr.system.dump
          :deb-trace-resolution
				  :clause->string)
	(:import-from :flexpr.system.unifier
				  :eliminate-gen?
				  :reverse-unify
				  :unify)
	(:import-from :flexpr.system.error
				  :maximum-depth-exceeded-error
				  :undeterminable-error)
	)



(defun resolution? (goal-clause clause)
  (assert (and (typep goal-clause 'clause)
			   (typep clause 'clause)))

  (let ((left-literal (car (clause-%literals goal-clause))))
	;; clause が規則節であり、 ヘッドがleft-literalと融合可能である
	;; clauseが事実節であり、left-literalと融合可能である

	;; (flag resolted rule)
	(let ((rule (eliminate-gen? 
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
;;; => 必ずしもこの方針、良くなるとは限らない
;;; 但しそれも節の大きさが同じ場合、使われたことの回数が少ない
;;; 節を優先して使う
(defun primary-short (clause-form*)
  (if (null clause-form*) nil
	(let ((flen (clause*-len (car clause-form*))))
	  (if (every (lambda (x) (= (clause*-len x) flen)) clause-form*) 
		(primary-unused clause-form*)
		(sort clause-form*
			  (lambda (x y)
				(< (clause*-len x) (clause*-len y))))))))


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

(defun lookupper (clause-form)
  (let*
	((prefix "NODE")
	 (defnode 
	   (mapcar
       (lambda (x)
         (cons x (symbol-name (gensym prefix)))) 
       (mapcar #'set-defined-flag clause-form)))
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


@export
(defun resolution-snl (premises-clause-form 
					   conseq-clause-form 
					   original-exist-terms
					   &key 
					     (depth +DEPTH+)
						 (output nil))
  (labels 
	((main (clause-form 
			goal-clause
			dep
			exist-terms
			lookup
			node-relation
			)

 			(when (> 1 dep)
			  (error (make-condition 'maximum-depth-exceeded-error
										 :mde-val depth
								     :mde-where 'resolution-snl)))

			(let* ((choices (choices goal-clause clause-form)))
			  
			  (some	
					
				(lambda (cand)
				  (destructuring-bind (clause (flag resolted mgu)) cand

           (deb-trace-resolution mgu clause goal-clause (list resolted)) 
            
            
					(special-let* 
						  ((selected-clause-name (funcall lookup goal-clause))
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
										 (list t original-exist-terms (reverse-unify exist-terms mgu))))

						 (if output 
						   (append base (list (funcall lookup nil) relation))
						   base))) 

					  (t 
						(handler-case 
						  (main 
							(append 
							  (remove clause clause-form :test #'%clause=)
							  (list ;; append するための list
                  
                  (set-defined-flag 
                    (clause 
                      (rename-clause 
                        (clause-%literals clause)) 
                      (1+ (clause-used clause)))
                    (clause-defined clause))))

							resolted
							(1- dep)
							(reverse-unify exist-terms mgu)
							lookup
							relation
							)
						  (maximum-depth-exceeded-error (c)
							(declare (ignore c)) nil)))))))
				choices
				))))
		
	(let* ((normal (normalize premises-clause-form))
				 (clause-form (append normal conseq-clause-form))
				 (res 
					 (main 
             normal
						 ;; 結論となる節がひつとしかないので
						 ;; (ホーン節を仮定している) car で１つだけとればおｋ
						 ;; ていうか１つしか無い
             (car conseq-clause-form)
				     (* 2 depth)
				     original-exist-terms
				     (lookupper clause-form)
				     nil)))
	  (unless res
			(error (make-condition 'undeterminable-error))) 
		res)))


