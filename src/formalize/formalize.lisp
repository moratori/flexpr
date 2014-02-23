

(ns:defns flexpr.formalize
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct
		  :flexpr.formalize.opr
		  :flexpr.formalize.quant
		  :flexpr.formalize.mat)
	(:import-from :flexpr.util
				  :literal?
				  :opr-equal?
				  :closed?
				  :clause?
				  :opposite-opr)
	(:import-from :flexpr.error
				  :illformed-error))


;; 1.remove-disuse-quant 
;;  束縛されていない量化子を削除する
;; 2.remove-operator
;;  -> , <-> を取り除く
;; 3.literalize
;;  リテラルの選言.連言となるように変形する
;; 4.rename-bound-var
;;  束縛変数を正規化する
;; 5.prefix
;; 	冠頭標準形にする
;; 6.cnf
;;  母式を連言標準形に変形
;; 6.dnf 
;; 	母式を選言標準形に変形
;;



@export
(defun formalize (lexpr &optional (op (operator +AND+)))
  (c/dnf 
	(prefix 
	(rename-bound-var 
	  (literalize 
		(remove-operator 
		  (remove-disuse-quant lexpr))) nil nil))op))




(defgeneric get-clause (a b)
	(:documentation "b is operator that expressing which formalization"))

(defmethod get-clause ((lexpr atomic-lexpr) (op operator))
  (list (list lexpr)))

(defmethod get-clause ((lexpr normal-lexpr) (op operator))
  (cond 
	((literal? lexpr)
	 (list (list lexpr)))
	((clause? lexpr (operator (opposite-opr op)))
	 ;; get leaf
	 (labels 
	   ((collect (lexpr result)
			(if (literal? lexpr)
			  (cons lexpr result)
			  (let ((left  (normal-lexpr-l-lexpr lexpr))
					(right (normal-lexpr-r-lexpr lexpr)))
				(cond 
				  ((and (literal? left)
						(literal? right)) 
				   (cons left (cons right result)))
				  ((literal? left)
				   (collect right (cons left result)))
				  ((literal? right)
				   (collect left (cons right result)))
				  (t (make-condition 'illformed-error
							:ie-mes "formalized formula(CNF/DNF) required."
							:ie-val lexpr
							:ie-where 'get-clause)))))))
	  (list (collect lexpr nil))))
	(t
	  (append
		(get-clause 
		  (normal-lexpr-l-lexpr lexpr) op)
		(get-clause 
		  (normal-lexpr-r-lexpr lexpr) op)))))

(defmethod get-clause ((lexpr lexpr) (op operator))
  (get-clause (lexpr-expr lexpr) op))

;;; {{P} , {P , Q} , {R , S} ...}
(defun convert (lexpr op)
  (assert (or (typep lexpr 'atomic-lexpr)
			  (typep lexpr 'normal-lexpr)
		      (typep lexpr 'lexpr)))

  (let ((formed (formalize lexpr op)))
	(unless (closed? formed)
		(error 
		  (make-condition 'illformed-error
			:ie-mes "closed formula required."
			:ie-val formed
			:ie-where 'convert)))	
	(get-clause formed op)))


;;; convertされた集合を対象にさらに
;;; 内部形式 %literal へと変換する
;;; (このストラクチャのdumpなどは行わないとする.
;;;  別にやるとしても atomic-lexpr と normalのopがnegなだけだからいつでもできる
;;;  その他の処理についても多分同様)
;;; 導出原理はこの%literalについて行うとする
;;; さらに変換された%literalの節集合表現を単純化する処理reductionを定義する
;;;
;;; P <OPERATOR> P => P
;;; (P V ~P V ... ) & (Q V R V ...) & ... => (Q V R V ...) & ...
;;; (P & ~P & ... ) V (Q & R & ...) V ... => (Q & R & ...) V ...
