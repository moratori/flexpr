

(ns:defns flexpr.unifier
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :flexpr.util
				  :term=)
	(:import-from :flexpr.error
				  :struct-unmatch-error))


;;;; 単一化の実装




@export
(defgeneric mgu (a b)
	;; 単一化成功なら((x . f(y)) ...) または t を返す
	;; 失敗なら nil を返す
	(:documentation "get most general unifier"))


(defmethod mgu ((t1 vterm) (t2 vterm))
  ;; (a . b) a -> b
  (cond 
	;; 完全に一致したvterm同士は無条件で単一化成功
	;; 単一化子は必要ない
	((term= t1 t2) t)
	((and (vterm-const t1) 
		  (not (vterm-const t2)))
	 (acons t2 t1 nil))
	((and (vterm-const t2) 
		  (not (vterm-const t1)))
	 (acons t1 t2 nil))
	((not (or (vterm-const t1)
			  (vterm-const t2)))
	 ;;;ここのcons逆でもいいよね
	 ;;;ここの選び方で後に影響を与えることはある?
	 ;;;シングルトン変数を持つような時に問題ありかも?
	 ;(acons t1 t2 nil) がもともとだったけど
	 ;逆にしたら(acons t2 t1 nil)テスト通ったんだけど
	 ;それってそのテストケースに固有な解決方法になってないか
	 (acons t2 t1 nil)
	 )
	(t nil)))


(defmethod mgu ((t1 fterm) (t2 vterm))
  (cond 
	((vterm-const t2)
	 (eq (fterm-fsymbol t1)
		 (vterm-var t2)))
	(t (acons t2 t1 nil))))

(defmethod mgu ((t1 vterm) (t2 fterm))
  (mgu t2 t1))




(defun subst-term (new old seq)
  (substitute-if new (lambda (x) (term= x old)) seq))


;;; 新しいルールを今までに見つかった置換規則に適用する
(defun subst-old-rule (old-rules new-rules)
  ;; new-rules = ((old . new))
  ;; old-rules = ((old1 . new1) (old2 . new2) ...)
  (if (null new-rules) old-rules
	 (destructuring-bind (old . new) (car new-rules)
	   (subst-old-rule
(mapcar 
	  (lambda (x)
		(destructuring-bind (each-old . each-new) x
		 (cons each-old
			   (cond 
				 ((vterm-p each-new)
					(car (subst-term new old (list each-new))))
				 ((fterm-p each-new)
					(apply #'fterm (fterm-fsymbol each-new) 
						   (subst-term new old (fterm-terms each-new))))
				 (t (error (make-condition 'struct-unmatch-error 
					   :sue-val each-new
					   :sue-where 'subst-old-rule)))))))
	  old-rules)
		(cdr new-rules)
		 )
	   )
	)
 )

;;; 新しいルールを更に今まで見つかったので置換
(defun subst-new-rule (old-rules new-rules)
  ;; new-rules = ((old . new))
  ;; old-rules = ((old1 . new1) (old2 . new2) ...)
  (reduce 
	(lambda (x y)
	  (subst-old-rule x (list y))) 
	old-rules :initial-value new-rules))


;; 前に得られた置換にも遡って置換した方がいいかもしれない
;; いまのだと 
;; f(g(x),x) と f(y,C) のMGUを求めると
;; <y -> g(x) , x -> C>
;; を返すけど
;; {x -> C , y -> g(C)}
;; を返すようにしたほうがいいのかな?
;; 今のでも先頭から置換を施せば
;; 同じ結果になるような気がする
;; つまり、g(x) のxはx -> C に依存してるような感じにどれもなる?
;; ==> ならないので修正
;;     つまり以前に得られた規則に最新の置換を施さなきゃだめだ
;;     しかも逆に最新の置換規則に、今まで見つかったやつも施さないとだめだ
;;
;; 無限ループする例: P(x,C,f(z,x)) 
;; 					 P(g(y,w),y,w)
;; x -> f(x)
;; みたいにsrc と ranどちらにも同じシンボルが出てたら無限ループ
(defmethod mgu ((t1 fterm) (t2 fterm))
  (cond 
	;; 変数の場合と同様、全く同一なら単一化の処理は必要ない
	((term= t1 t2) t)
	((not (eq (fterm-fsymbol t1)
			  (fterm-fsymbol t2)))
	 nil)
	((/= (length (fterm-terms t1))
		 (length (fterm-terms t2)))
	 nil)
	(t
	  (labels 
		((main (result argv1 argv2)
			(if (null argv1) result
			  (let ((unifier (mgu (car argv1) 
								  (car argv2))))
					(cond 
					  ;; 単一化は失敗
					  ((null unifier) nil)
					  
					  ((listp unifier)
						(apply #'main 
							   (append (subst-old-rule result unifier) 
									   (subst-new-rule result unifier))
							   (reduce 
								 (lambda (x y)
								   (destructuring-bind (old . new) y
									 (destructuring-bind (a1 a2) x
									   (list 
										 (subst-term new old a1)
										 (subst-term new old a2))))) 
								 unifier :initial-value (list (cdr argv1) (cdr argv2)))))
					  
					  (t (main result
						   	   (cdr argv1)
							   (cdr argv2))))))))
		
		(main nil
		  	  (fterm-terms t1)
			  (fterm-terms t2))))))




(defmethod mgu ((lexpr1 atomic-lexpr) (lexpr2 atomic-lexpr))
  ;; 実際には関数項と同じ扱い
  ;; だけど命題変数への扱いをかんがえなければならない
  (if (and (null (atomic-lexpr-terms lexpr1))
		   (null (atomic-lexpr-terms lexpr2))) 
		(eq (atomic-lexpr-pred-sym lexpr1)
			(atomic-lexpr-pred-sym lexpr2))
		(mgu (apply #'fterm (atomic-lexpr-pred-sym lexpr1)
							(atomic-lexpr-terms    lexpr1)) 
			 (apply #'fterm (atomic-lexpr-pred-sym lexpr2)
							(atomic-lexpr-terms    lexpr2)))))

@export
(defun eliminate? (lit1 lit2)
  (assert (and (typep lit1 '%literal)
			   (typep lit2 '%literal)))
  (cond 
	((eq (%literal-negation lit1)
		 (%literal-negation lit2)) nil)
	(t (mgu 
		 (apply #'atomic-lexpr 
		   (%literal-pred lit1) (%literal-terms lit1))
		 (apply #'atomic-lexpr 
		   (%literal-pred lit2) (%literal-terms lit2))))))



@export
(defun unify (rule clause)
  ;; ( (term1 . new1) (term2 . new2) )
  (if (eq rule t) clause
	 (clause
	   (mapcar 
		 (lambda (x)
		   (assert (typep x '%literal))
		   (%literal 
			 (%literal-negation x)
			 (%literal-pred x)
			 (labels 
			   ((main (term)
					  (cond 
						((vterm-p term)
						 (let ((subs (assoc term rule :test #'term=)))
						   (if (null subs) 
							 term
							 (cdr subs))))
						((fterm-p term)
						 (apply #'fterm (fterm-fsymbol term)
								(mapcar #'main (fterm-terms term))))
						(t 
						  (error (make-condition 'struct-unmatch-error 
					   :sue-val term
					   :sue-where 'unify))))))
			   (mapcar #'main (%literal-terms x)))
			 0))
		 (clause-%literals clause))
	   0))) 




(defun specific-term (term rule)
  (if (null rule) term
	(let* ((rest
			(member-if 
			  (lambda (x)
				(term= (car x) term)) rule))
		   (target (car rest)))
	  (if (null rest) term
		 (destructuring-bind (self . new) target
		(if (vterm-p new) 
		  (specific-term new (cdr rest))
		  (apply #'fterm
			(fterm-fsymbol new)
			(mapcar 
			  (lambda (x)
				;; ここで rule を直に渡してるのはわざとで、
				;; 関数記号の場合の引数となるシンボルが
				;; rest 以前に含まれている可能性があるとして
				;; こうしてるけど 無限ループするんじゃ?
				;;
				;; x -> f(x) みたいなパターンでてくるのクソ厄介
				;; 無限項のパターンマッチは起こってないはずなのに
				;; これなんでおきんの
				;; 今は以下のように (cdr rest)にして
				;; 減らしてるけど、これだと再帰ルールの時とかに誤る
				(specific-term x (cdr rest))) 
			  (fterm-terms new)))))))))


@export
(defun reverse-unify (exist-terms rule)
  ;; exist-terms is (vterm1 vterm2 ...)
  ;; rule is ((a1 . b1) (a2 . b2) ...)
  (print exist-terms)
  (format t " !!!-- DEBUG PRING RULE  --!!!~%~A~%~%~%" rule)

  (mapcar 
	(lambda (x)
	  (assert (typep x 'vterm))
	(cons x (specific-term x rule))) 
	
	exist-terms)
  
  )

