/

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
				  :opposite-opr
				  :%literal=
				  :opposite-%literal=
				  :%clause=)
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




;;; スコーレム標準形までもっていく処理
;;; 命題論理式の場合はCNFまたはDNF形にするだけ
@export
(defun formalize (lexpr &optional (op (operator +AND+)))
(skolemization
	(c/dnf 
	(prefix 
	(rename-bound-var 
	  (literalize 
		(remove-operator 
		  (remove-disuse-quant lexpr))) nil nil))op)))





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
  (assert (every (lambda (l) (typep l '%literal)) clause))
  (remove-duplicates clause :test #'%literal=))


(defun disuse-clause? (clause)
	(some 
	  (lambda (literal)
		(find-if 
		  (lambda (x)
			(opposite-%literal= literal x)) clause)) clause))

;;; (P V ~P V ... ) & (Q V R V ...) & ... => (Q V R V ...) & ...
;;; (P & ~P & ... ) V (Q & R & ...) V ... => (Q & R & ...) V ...
(defun reduction-clause-form (clause-form op)
  (assert
	(every 
	  (lambda (clause)
		(every (lambda (lit) (typep lit '%literal)) clause)
		) clause-form)
	)
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
						   (normal-lexpr-l-lexpr literal))))
	  
	  ((atomic-lexpr-p literal)
	   (%literal nil
				 (atomic-lexpr-pred-sym literal)
				 (atomic-lexpr-terms literal)))
			  
	  (t 
		(error 
		  (make-condition 'illformed-error
							:ie-mes "formalized formula required"
							:ie-val literal
							:ie-where 'literal->%literal)))))


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
@export
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
	(reduction-clause-form 
		(mapcar 
		  (lambda (clause)
			(mapcar 
			  (lambda (literal)
				(literal->%literal literal)) clause)) 
		  (get-clause formed op)) op)))


