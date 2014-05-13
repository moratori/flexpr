

(ns:defns flexpr.system.formalize
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct
		  :flexpr.system.formalize.opr
		  :flexpr.system.formalize.quant
		  :flexpr.system.formalize.mat)
	(:import-from :flexpr.system.util
				  :literal?
				  :opr-equal?
				  :closed?
				  :clause?
				  :opposite-opr
				  :%literal=
				  :opposite-%literal=
					:tautology?
				  :%clause=)
	(:import-from :flexpr.system.unifier
								:mgu
								:absurd)
	(:import-from :flexpr.system.error
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




(defun %formalize (lexpr &optional (op (operator +AND+)))
  ;; ここのフラグが立っていると rename remove-op remove-dis しない
  (c/dnf 
	  (prefix 
		(rename-bound-var 
		  (literalize 
			(remove-operator 
			  (remove-disuse-quant lexpr))) nil nil)) op))

;;; スコーレム標準形までもっていく処理
;;; 命題論理式の場合はCNFまたはDNF形にするだけ
;;; かなりキモい式(同値演算子でやたら式の長さが増えるようなの)をformalizeしようとすると
;;; ヒープ食いつぶす場合あり
@export
(defun formalize (lexpr &optional (op (operator +AND+)) (quant +EXISTS+))
  (skolemization
		(%formalize lexpr op) quant))





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
;;;

;;; 重複を削除する
;;; P <OPERATOR> P => P
(defun reduction-clause (clause)
  (assert (typep clause 'clause))
  (clause
	(remove-duplicates 
	  (clause-%literals clause) :test #'%literal=)
	0
	)
  )


(defun disuse-clause? (clause)
  (assert (typep clause 'clause))
	(some 
	  (lambda (literal)
		(find-if 
		  (lambda (x)
			(opposite-%literal= literal x)) 
		  (clause-%literals clause))) 
	  (clause-%literals clause)))

;;; (P V ~P V ... ) & (Q V R V ...) & ... => (Q V R V ...) & ...
;;; (P & ~P & ... ) V (Q & R & ...) V ... => (Q & R & ...) V ...
(defun reduction-clause-form (clause-form op)
  (assert
	(every 
	  (lambda (clause)
		(typep clause 'clause)
		) clause-form))
  ;; まず重複を削除して排中律と矛盾に関する規則
  (remove-if #'disuse-clause?
			 (remove-duplicates 
			   (mapcar #'reduction-clause clause-form) :test #'%clause=)))


(defun literal->%literal (literal)
  (assert (or (typep literal 'atomic-lexpr)
			  (typep literal 'normal-lexpr)))
	(cond 
	  ((normal-lexpr-p literal)
	   (unless (and (opr-equal? (normal-lexpr-operator literal)
								(operator +NEG+))
					(atomic-lexpr-p (normal-lexpr-l-lexpr literal)))
			(error 
			  (make-condition 'illformed-error
							:ie-mes "formalized formula required"
							:ie-val literal
							:ie-where 'literal->%literal)))
			   (%literal t 
						 (atomic-lexpr-pred-sym 
						   (normal-lexpr-l-lexpr literal))
						 (atomic-lexpr-terms 
						   (normal-lexpr-l-lexpr literal))
						 0))
	  
	  ((atomic-lexpr-p literal)
	   (%literal nil
				 (atomic-lexpr-pred-sym literal)
				 (atomic-lexpr-terms literal)
				 0))
			  
	  (t 
		(error 
		  (make-condition 'illformed-error
							:ie-mes "formalized formula required"
							:ie-val literal
							:ie-where 'literal->%literal)))))


(defun check-term (unifier)
	(every 
		(lambda (x)
			(destructuring-bind (from . to) x
				(and 
					(vterm-p from)
					(vterm-p to)
					(not (vterm-const from))
					(not (vterm-const to))))) 
		unifier))

;; 節c1と節c2について
;; 変数名の付け替えであるか否か
;; (mgu x y)するんだけど、
;; x -> ZERO
;; とか x -> f(y) とかを許しちゃいけない
;; 単に変数の付け替えであるか否かなので
(defun alphabet-equal? (c1 c2)
	(let ((literals1 (clause-%literals c1))
				(literals2 (clause-%literals c2)))

	(multiple-value-bind 
		(flag res)
		(loop 
			named exit1
			with rule = nil
			finally (return-from exit1 (values t rule))
			for x in literals1
			do
			(let ((each 
							(loop 
								named exit2
								for y in literals2
								for unifier = (mgu x y)
								do
								(when (or (eq unifier t)
													;; x -> C
													;; x -> f(z) 
													(check-term unifier))
									(return-from exit2 unifier)))))
				(if each 
					(when (listp each) 
						(setf rule (append rule each)))
					(return-from exit1 (values nil nil)))))
		
		(and flag (absurd res)))))



@export
(defun remove-alphabet-equal (clause-form)	
	(remove-duplicates 
		clause-form
		:test 
		(lambda (c1 c2)
			(and (alphabet-equal? c1 c2)
					 (alphabet-equal? c2 c1)))))



;; 各節のリテラルの変数名を付け替える
(defun rename-clause-form (clause-form)
	(mapcar 
		(lambda (x)
			(assert (typep x 'clause))
			(clause 
				(rename-clause (clause-%literals x))
				(clause-used x)))
		clause-form))


;;; {{P} , {P , Q} , {R , S} ...}
;;; この処理で完全に節形式にもっていく
;;; つまり %literal の集合の集合
;;; これを dump するには clause-form->string を使う
;;; 節形式にした時に 節集合が nil になる時があるけど
;;; それは, opの違いで何を表しているかで変わる
;;;
;;; op が or の時: その節集合は矛盾を表す
;;; op が and の時: その節集合は真を表す
;;;
;;; どちらも、演算における単位元の扱いの性質
;;;
;;; bquant で表される 量化子に束縛される変数を取得する
@export
(defun convert (lexpr op quant &optional (bquant +FORALL+))

  (assert (or (typep lexpr 'atomic-lexpr)
			  (typep lexpr 'normal-lexpr)
		      (typep lexpr 'lexpr)))


(let* ((%formed (%formalize lexpr op))
					 (formed  (skolemization %formed quant)))
			(unless (closed? formed)
				(error 
					(make-condition 
						'illformed-error
						:ie-mes "closed formula required."
						:ie-val formed
						:ie-where 'convert)))
	(values 
		(remove-if 
			#'tautology?
			;; 以下でリネームしたいんだけど、exist の回収ができなくなるのでちょっと
			;; 考えなければ. でこのままだと mgu が計算できない
			;(rename-clause-form 
				(reduction-clause-form 
					(mapcar 
						(lambda (clause)
							(clause
								(mapcar 
									(lambda (literal)
										(literal->%literal literal)) clause)0)) 
				(get-clause formed op)) op)
				;)
			)  
		;; exist-termを回収する処理
		(when (lexpr-p %formed)
				(loop for each in (quantsp-each-quant (lexpr-qpart %formed))
							if (eq bquant (quant-qnt each))
							collect (quant-var each))))))

