

(ns:defns unifier
	(:use :cl
		  :constant
		  :struct)
	(:import-from :util
				  :term=))


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
	 ;;ここのcons逆でもいいよね
	 ;;ここの選び方で後に影響を与えることはある?
	 (acons t1 t2 nil))
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
  (destructuring-bind ((old . new)) new-rules
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
				 (t (error "subst-rule: unexpected error"))))))
	  old-rules)))

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
  (mgu (apply #'fterm (atomic-lexpr-pred-sym lexpr1)
			  		  (atomic-lexpr-terms    lexpr1))
	   (apply #'fterm (atomic-lexpr-pred-sym lexpr2)
			  		  (atomic-lexpr-terms    lexpr2))))


