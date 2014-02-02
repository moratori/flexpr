

(ns:defns unifier
	(:use :cl
		  :constant
		  :struct)
	(:import-from :util
				  :term=)
	(:import-from :optima
				  :match))


;;;; 単一化の実装


;;; 関数記号を含まない、変数定数に対する単一化だけ先に書いてしまう


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
  ;; 0引数関数に対応しなければならない
  )

(defmethod mgu ((t1 vterm) (t2 fterm))
  (mgu t2 t1))



#|
	
	f(x,y,x)
	f(a,b,g(a,b))
	


|#


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
	  #|
	  ;;; 不一致なのを求めて単一化
	  ;;; f(a,b)
	  ;;; f(x,y)
	  (loop named exit
			with result = nil
			for arg-t1 in (fterm-terms t1)
			for arg-t2 in (fterm-terms t2)
			finally (return result)
			do
			(let ((unifier (mgu arg-t1 arg-t2)))
			  (when (null unifier)
				(return-from exit nil))

			  (when (listp unifier)
				(push unifier result)
				;; substしてmguに回す
				)))
	  |#
	  )))







