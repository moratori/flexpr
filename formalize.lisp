

(ns:defns flexpr.formalize
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :optima
				  :match)
	(:import-from :flexpr.util
				  :term-using?
				  :term=
				  :opr-equal?
				  :opposite-qnt
				  :opposite-opr
				  :clause?)
	(:import-from :flexpr.error
				  :struct-unmatch-error
				  :illformed-formalize-error))




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




(defgeneric remove-disuse-quant (a)
	(:documentation "remove disuse quantifier in expression a"))

(defmethod remove-disuse-quant ((lexpr atomic-lexpr))
  lexpr)


(defmethod remove-disuse-quant ((lexpr normal-lexpr))
	(match lexpr
		((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
		 (normal-lexpr 
		   operator
		   (remove-disuse-quant l-lexpr)
		   (unless (null r-lexpr)
			 (remove-disuse-quant r-lexpr))))	 
		(otherwise 
		  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'remove-disuse-quant_normal-lexpr)))))


(defmethod remove-disuse-quant ((lexpr lexpr))
  (match lexpr
	((lexpr :qpart qpart :expr expr)
	 ;; まずどうでもいい束縛を除去
	 ;; (Qτ ).φ についてτ が φ 中で自由出現しないなら(Qτ)を除去レ
	 (let ((new-qpart 
			 (remove-if 
			   (lambda (qnt)
				 (not (term-using? (quant-var qnt) expr)))
			   (quantsp-each-quant qpart))))
	   
	   (if (null new-qpart)
		 (remove-disuse-quant expr)
		 (lexpr (apply #'quantsp new-qpart) 
				(remove-disuse-quant expr)))))
	(otherwise 
	 (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'remove-disuse-quant_lexpr)))))




(defgeneric remove-operator (a)
	(:documentation "remove operator other than NOT or OR or AND"))


(defmethod remove-operator ((lexpr atomic-lexpr))
  lexpr)



(defmethod remove-operator ((lexpr normal-lexpr))
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 (cond 

	   ((opr-equal? operator (operator +NEG+))
		(normal-lexpr operator
		  (remove-operator l-lexpr)
		  nil))

	   ((or (opr-equal? operator (operator +OR+))
			(opr-equal? operator (operator +AND+)))
		(normal-lexpr operator
			(remove-operator l-lexpr)		  
			(remove-operator r-lexpr)))

	   ((opr-equal? operator (operator +IMPL+))
		(normal-lexpr (operator +OR+)
			(normal-lexpr (operator +NEG+)
			  (remove-operator l-lexpr) nil)
			(remove-operator r-lexpr)))
	   
	   ((opr-equal? operator (operator +EQ+))
		(normal-lexpr (operator +AND+)
			(remove-operator 
			  (normal-lexpr (operator +IMPL+)
					l-lexpr
					r-lexpr))		  
			(remove-operator 
			  (normal-lexpr (operator +IMPL+)
					r-lexpr
					l-lexpr))))
	   (t 
		 (error (make-condition 'illformed-formalize-error
								:ife-mes "conversion rule is not defined"
								:ife-val operator
								:ife-where 'remove-operator_normal-lexpr)))))
	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'remove-operator_normal-lexpr)))))


(defmethod remove-operator ((lexpr lexpr))
  (lexpr 
	(lexpr-qpart lexpr)
	(remove-operator (lexpr-expr lexpr))))






;; 否定を内側に移動して全てリテラルの選言か連言にする

(defgeneric literalize (a)
 	(:documentation "move to the inside"))


(defmethod literalize ((lexpr atomic-lexpr))
  lexpr)

(defmethod literalize ((lexpr normal-lexpr))
  ;; 既にremove-operatorされた式が来ることを意図する
  ;; つまりlexpr中に > や - が含まれていたらエラー
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 
	 (cond 

	   ((or (opr-equal? operator (operator +AND+))
			(opr-equal? operator (operator +OR+)))		
			(normal-lexpr operator
				(literalize l-lexpr)		  
				(literalize r-lexpr)))

	   ((opr-equal? operator (operator +NEG+))
		;; r-lexprはnilだから気にすること無い
		;; l-lexprだけみてればいい
		(match l-lexpr
			
			((normal-lexpr :operator operator_ :l-lexpr l-lexpr_ :r-lexpr r-lexpr_)
			 ;; operator_ は V,&,~のどれか
			 (cond 
			   
			   ((opr-equal? operator_ (operator +NEG+))
				(literalize l-lexpr_))

			   ;; どモルガン
			   ((or (opr-equal? operator_ (operator +AND+))
					(opr-equal? operator_ (operator +OR+)))
				(normal-lexpr (operator (opposite-opr operator_))
					(literalize 
					  (normal-lexpr (operator +NEG+) l-lexpr_ nil))
					(literalize
					  (normal-lexpr (operator +NEG+) r-lexpr_ nil))))
	
			   (t 
					(error (make-condition 'illformed-formalize-error
								:ife-mes "removed operator lexpr required"
								:ife-val operator_
								:ife-where 'literalize_normal-lexpr)))))
			
			((lexpr :qpart qpart_ :expr expr_)
			 ;; ~AxAy...の形
				(let* ((quant-lst (quantsp-each-quant qpart_))
					   (head      (car quant-lst)))

				  (literalize
					(lexpr
					  (apply #'quantsp 
							 (cons (quant (quant-qnt head)
										  (quant-var head)
										  (1+ (quant-neg head))) 
								   (cdr quant-lst)))
					  expr_))))
			
			(otherwise 
			  (unless (typep l-lexpr 'atomic-lexpr)
					(error (make-condition 'illformed-formalize-error
								:ife-mes "l-lexpr must be atomic"
								:ife-val operator_
								:ife-where 'literalize_normal-lexpr)))
			  lexpr ;; 負リテラル
			  )))
		
	   (t 
		 (error (make-condition 'illformed-formalize-error
								:ife-mes "removed operator lexpr required"
								:ife-val operator
								:ife-where 'literalize_normal-lexpr)))))	 
	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'literalize_normal-lexpr)))))




(defun remove-quant-negation (lexpr)
  (assert (typep lexpr 'lexpr))

  (match lexpr
    ((lexpr :qpart qpart :expr expr)
		(lexpr
			(apply #'quantsp
				   (labels 
					 ((main (lst result acc)
						(if (null lst) (reverse result)
						  (match (car lst)
						    ((quant :qnt qnt :var var :neg neg)
							 (if (evenp (+ neg acc)) 
							   (main 
								 (cdr lst)
								 (cons (quant qnt var 0) result) 
								 0)
							   (main 
								 (cdr lst)
								 (cons (quant (opposite-qnt (car lst)) var 0) result) 
								 1)))
							(otherwise 
								(error (make-condition 'struct-unmatch-error 
											:sue-val (car lst)
					   						:sue-where 'remove-quant-negation)))))))
					 (main (quantsp-each-quant qpart) nil 0)))
			
			  (if (evenp 
					(reduce 
					  (lambda (x y) (+ x (quant-neg y))) 
					  (quantsp-each-quant qpart) :initial-value 0))
				expr
				(normal-lexpr (operator +NEG+) expr nil))))
	(otherwise 
	 (error (make-condition 'struct-unmatch-error 
				:sue-val lexpr
				:sue-where 'remove-quant-negation)))))


(defmethod literalize ((lexpr lexpr))
  (let ((tmp (remove-quant-negation lexpr)))
	(lexpr 
	  (lexpr-qpart tmp)
	  (literalize (lexpr-expr tmp)))))





(defgeneric rename-bound-var (lexpr old new)
	(:documentation "rename bound variable"))



(defmethod rename-bound-var ((lexpr atomic-lexpr) old new)
  (if (not (or (null old) 
			   (null new)))
	(match lexpr
		((atomic-lexpr :pred-sym pred-sym :terms terms)
	 		(apply #'atomic-lexpr
				pred-sym
				(loop for each in terms
					  collect
					  (labels 
						((main (term)
							(cond 
							  ((vterm-p term)
							   (if (term= term old) new term))
							  ((fterm-p term)
							   (apply #'fterm (fterm-fsymbol term)
									  (mapcar #'main (fterm-terms term))))
							  (t 
								(error (make-condition 'illformed-formalize-error
										:ife-mes "vterm or fterm required"
										:ife-val term
										:ife-where 'rename-bound-var_atomic-lexpr))))))
						(main each)))))
		(otherwise 
		   (error (make-condition 'struct-unmatch-error 
				:sue-val lexpr
				:sue-where 'remove-bound-var_atomic-lexpr))))
	lexpr))


(defmethod rename-bound-var ((lexpr normal-lexpr) old new)
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 (normal-lexpr operator
	   (rename-bound-var l-lexpr old new)
	   (unless (null r-lexpr)
		 (rename-bound-var r-lexpr old new))))
	(otherwise 
	 (error (make-condition 'struct-unmatch-error 
				:sue-val lexpr
				:sue-where 'remove-bound-var_normal-lexpr)))))

(defmethod rename-bound-var ((lexpr lexpr) old new)
  ;;remove-disuse-quantやった後じゃないとだめ
  (match lexpr
	((lexpr :qpart qpart :expr expr)
	 (let ((quants (quantsp-each-quant qpart)))
	   ;; quants is quant lst
	   ;; below lst is rule for rename
	  
	   (let ((rule (loop for each in quants
						 collect (cons (quant-var each)
									   (vterm (gensym +RENAME-PREFIX+)  nil)))))
		(lexpr
		  (apply #'quantsp
				 (loop for pair in rule 
					   for qnt  in quants
					   collect (quant (quant-qnt qnt)
									  (cdr pair)
									  (quant-neg qnt))))
		  (let ((res (reduce 
					   (lambda (x y)
						 (rename-bound-var x (car y) (cdr y)))  
					   rule :initial-value expr)))
			
				(if (and (not (null old))
						 (not (null new))
						 (find-if-not 
						   (lambda (x) (term= (car x) old)) rule))
				  (rename-bound-var res old new)
				  res
				  
				  )))))) 
	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
				:sue-val lexpr
				:sue-where 'remove-bound-var_lexpr)))))




;; S は x を自由変数として持たない
;; D は x を自由変数として持つ
;; Ax.D & S -> Ax.(D & S)
;; Ax.D V S -> Ax.(D V S)
;; Ex.D & S -> Ex.(D & S)
;; Ex.D V S -> Ex.(D V S)
;;
;; Ax.(P(x) & (Q(x) V Ay.R(y)))

(defgeneric prefix (a)
	;; prefixに来るためには特に束縛変数が正規化されてるひつようあり
	(:documentation "prefix normalization"))


(defmethod prefix ((lexpr atomic-lexpr))
  lexpr)



(defun get-quantsp-lst (lexpr)
  (when (lexpr-p lexpr)
	(quantsp-each-quant (lexpr-qpart lexpr))))

(defun get-expr (lexpr)
  (if (lexpr-p lexpr) (lexpr-expr lexpr)
	lexpr))

(defun both-lexpr (l-lexpr op r-lexpr)
	(let ((left-mat  (prefix (lexpr-expr l-lexpr)))
		  (right-mat (prefix (lexpr-expr r-lexpr))))
	  (lexpr 
		(apply #'quantsp 
			   (append (get-quantsp-lst l-lexpr)
					   (get-quantsp-lst left-mat)
					   (get-quantsp-lst r-lexpr)
					   (get-quantsp-lst right-mat)))
		(normal-lexpr op
					  (get-expr left-mat)
				      (get-expr right-mat)))))


(defmethod prefix ((lexpr normal-lexpr))
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 
	 (cond 
	
	   ;;先の正規化でnegationは原子式のまえにしかついてないはず
	   ((opr-equal? operator (operator +NEG+))
		lexpr)

	   ;;どっちもatomicだったらこれ以上むり
	   ((and (atomic-lexpr-p l-lexpr)
			 (atomic-lexpr-p r-lexpr))
			(normal-lexpr operator
				l-lexpr
				r-lexpr))

	   ;; どちらもlexprだったら
		((and (lexpr-p l-lexpr)
			  (lexpr-p r-lexpr))
			(both-lexpr l-lexpr operator r-lexpr))


		(t 
		  
		  (let ((left  (prefix l-lexpr))
				(right (prefix r-lexpr)))
				
			(cond

			  ;; どっちもlexprになったら
			  ((and (lexpr-p left)
					(lexpr-p right))
			   (both-lexpr left operator right))

			  ;; どっちも量化されない
			  ((not (or (lexpr-p left)
						(lexpr-p right)))
			   (normal-lexpr operator
				left
				right))

			  ;; 何れか一方は lexpr type

			  ((lexpr-p left)
				(lexpr 
				   (lexpr-qpart left)
				   (normal-lexpr operator
						(lexpr-expr left)
						right)))

			  ((lexpr-p right)
				(lexpr 
				   (lexpr-qpart right)
				   (normal-lexpr operator
						left
						(lexpr-expr right))))
			  
			  (t 
				(error (make-condition 'illformed-formalize-error
								:ife-mes "formula does not satisfy any condition"
								:ife-val lexpr
								:ife-where 'prefix_normal-lexpr))))))))
	(otherwise 
		(error
		  (make-condition 'struct-unmatch-error
						  :sue-val lexpr
						  :sue-where 'prefix_normal-lexpr)))))




(defmethod prefix ((lexpr lexpr))
  (match lexpr
	((lexpr :qpart qpart :expr expr)
	 (let ((res (prefix expr)))
	   (cond 
		 ((or (atomic-lexpr-p res)
			  (normal-lexpr-p res))
		  (lexpr qpart expr))
		 ((lexpr-p res)
		  (lexpr 
			(apply #'quantsp (append (quantsp-each-quant qpart) 
									 (get-quantsp-lst res))) 
			(lexpr-expr res)))
		 (t 
		   (error
			 (make-condition 'struct-unmatch-error
							 :sue-val res
							 :sue-where 'prefix_lexpr))))))
	(otherwise 
	  (error 
		(make-condition 'struct-unmatch-error
						:sue-val lexpr
						:sue-where 'prefix_lexpr)))))


(defgeneric c/dnf (lexpr op)
	(:documentation "formalize in cnf or dnf"))

(defmethod c/dnf ((lexpr atomic-lexpr) (op operator))
  lexpr)


(defmethod c/dnf ((lexpr normal-lexpr) (op operator))
  (let ((revop (opposite-opr op)))
	(if (clause? lexpr (operator revop)) lexpr
		(match lexpr
		;; F V (G & H) -> (F V G) & (F V H)
		;; F & (G V H) -> (F & G) V (F & H)
    	;; (G & H) V F -> (F V G) & (F V H)
		;; (G V H) & F -> (F & G) V (F & H)

		((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
		 	
		 	(cond 

			  ((and (opr-equal? (operator revop) operator)
					(normal-lexpr-p r-lexpr)
					(opr-equal? op (normal-lexpr-operator r-lexpr)))
			   
			   		(normal-lexpr op
						(c/dnf (normal-lexpr (operator revop) 
									  l-lexpr 
									  (normal-lexpr-l-lexpr r-lexpr))
							   op)
						(c/dnf (normal-lexpr (operator revop) 
									  l-lexpr 
									  (normal-lexpr-r-lexpr r-lexpr))
							   op)))
			  
			  ((and (opr-equal? (operator revop) operator)
					(normal-lexpr-p l-lexpr)
					(opr-equal? op (normal-lexpr-operator l-lexpr)))
			   		
			   		(normal-lexpr op
						(c/dnf (normal-lexpr (operator revop) 
									  r-lexpr 
									  (normal-lexpr-l-lexpr l-lexpr))
							   op)
						(c/dnf (normal-lexpr (operator revop) 
									  r-lexpr 
									  (normal-lexpr-r-lexpr l-lexpr))
							   op)))


			  ((and (opr-equal? (operator revop) operator)
					(normal-lexpr-p l-lexpr)
					(opr-equal? operator (normal-lexpr-operator l-lexpr)))
			   
			   (c/dnf 
				 (normal-lexpr (operator revop)
					
					(normal-lexpr-l-lexpr l-lexpr)
						  
					
					(c/dnf (normal-lexpr (operator revop)
						(normal-lexpr-r-lexpr l-lexpr)		  
						r-lexpr) op))
				 
				 op))


			  ((and (opr-equal? (operator revop) operator)
					(normal-lexpr-p r-lexpr)
					(opr-equal? operator (normal-lexpr-operator r-lexpr)))
			   
			   (c/dnf 
				 (normal-lexpr (operator revop)
					(c/dnf (normal-lexpr (operator revop)		  
								  l-lexpr
								  (normal-lexpr-l-lexpr r-lexpr))
					op)
				
				 
				   (normal-lexpr-r-lexpr r-lexpr)
				  
				  	
					) op))

			  ((opr-equal? operator op)
			   
			   (normal-lexpr op
					(c/dnf l-lexpr op)
					(c/dnf r-lexpr op)
					)
			   )
			  
			  (t  lexpr)))


		(otherwise 
		  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'c/dnf_normal-lexpr))))
	  )
	))

;; 連言標準形に変換
(defmethod c/dnf ((lexpr lexpr) (op operator))
  (lexpr (lexpr-qpart lexpr)
		 (c/dnf (lexpr-expr lexpr) op)))




@export
(defun formalize (lexpr &optional (op (operator +AND+)))
  (c/dnf 
	(prefix 
	(rename-bound-var 
	  (literalize 
		(remove-operator 
		  (remove-disuse-quant lexpr))) nil nil))op))





