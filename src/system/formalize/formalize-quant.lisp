
(ns:defns flexpr.system.formalize.quant
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
	(:import-from :optima
				  :match)
	(:import-from :flexpr.system.util
				  :term-using?
				  :term=
				  :opr-equal?
				  :qnt-equal?
				  :opposite-qnt)
	(:import-from :flexpr.system.error
				  :struct-unmatch-error
				  :illformed-formalize-error))




@export
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


@export
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

@export
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



;; quant-sym だけが量化子として残るようにする
@export
(defgeneric skolemization (a b )
	(:documentation "skolemize a"))


(defun substvar (matrix binds)
  ;; ((y (x)) (w (x)) (v (x z)))
  (cond
	((atomic-lexpr-p matrix)
	 (apply #'atomic-lexpr 
			(atomic-lexpr-pred-sym matrix)
			 (let ((terms (atomic-lexpr-terms matrix)))
	   (loop for each in terms
			 collect 
			 (labels 
			   ((main (each)
				(cond 
				  ((vterm-p each)
				   (let ((new (find-if 
								(lambda (x) (term= (car x) each)) binds)))
					 (if (null new) each
					   (second new))))
				  ((fterm-p each)
				   (apply #'fterm (fterm-fsymbol each)
						  (mapcar #'main (fterm-terms each))))
				  (t (error "term requried!")))))
			   (main each))))))
	((normal-lexpr-p matrix)
	 (normal-lexpr (normal-lexpr-operator matrix)
		(substvar (normal-lexpr-l-lexpr matrix) binds)
		(unless (null (normal-lexpr-r-lexpr matrix))
		  (substvar (normal-lexpr-r-lexpr matrix) binds))))
	(t (error "matrix must be atomic or normal"))))


(defun %skolemization (target-rm lexpr binds)
	(let ((new-qpart
			(remove-if  
			  (lambda (x)
				(eq (quant-qnt x) target-rm))
			  (quantsp-each-quant (lexpr-qpart lexpr)))))
	  
		(if (null new-qpart)
		  (substvar (lexpr-expr lexpr) binds)
		  (lexpr 
			(apply #'quantsp new-qpart)
			(substvar (lexpr-expr lexpr) binds)))))


(defmethod skolemization ((lexpr lexpr) quant-sym)
  ;; target-rm はタダの シンボル
  (let ((target-rm (opposite-qnt 
					 (quant quant-sym (vterm '|dummy| nil) 0)))
		(each (quantsp-each-quant (lexpr-qpart lexpr))))

		;; AxEyEwAzEv
		;; でEを削除したいとする.
		;; (y (x))
		;; (w (x))
		;; (v (x z))

		(%skolemization 
		  target-rm
		  lexpr
		  (labels 
		  ((main (qlist tmp result)
				 (if (null qlist) result
				   (let ((head (car qlist)))
					 (cond  
					   ((eq quant-sym (quant-qnt head))
						(main (cdr qlist)
							  (cons (quant-var head) tmp)
							  result))
					   ((eq target-rm (quant-qnt head))
						(main (cdr qlist)
							  tmp
							  (cons 
								(list (quant-var head)
									  (if (null tmp) 
										  (vterm (gensym +SKOLEM_CONST+) t)
										  (apply #'fterm 
												 (gensym +SKOLEM_FUNC+)
												 (reverse tmp)))) result)))
						(t 
						  (error "A or E required")))))))
		  (reverse (main each nil nil))))))


(defmethod skolemization (a b)
  a)




