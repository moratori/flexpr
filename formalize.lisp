

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
				  :opposite-opr))



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
		(otherwise (error "remove-disuse-quant(normal-lexpr): unexpected error"))))


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
	(otherwise (error "remove-disuse-quant(lexpr): unexpected error"))))




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
	   (t (error "remove-operator(normal-lexpr): conversion rule is not defined"))))
	(otherwise (error "remove-operator(normal-lexpr): unexpected error"))))


(defmethod remove-operator ((lexpr lexpr))
  (match lexpr
	((lexpr :qpart qpart :expr expr)
	 (lexpr qpart
			(remove-operator expr)))	 
	(otherwise (error "remove-operator(lexpr): unexpected error"))))








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
	
			   (t (error "literalize(normal-lexpr): removed operator lexpr required"))))
			
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
				(error "literalize(normal-lexpr): l-lexpr must be atomic"))
			  lexpr ;; 負リテラル
			  )))
		
	   (t (error "literalize(normal-lexpr): removed operator lexpr required"))))	 
	(otherwise (error "literalize(normal-lexpr): unexpected error"))))




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
							  (error "remove-quant-negation: unexpected error"))))))
					 (main (quantsp-each-quant qpart) nil 0)))
			
			  (if (evenp 
					(reduce 
					  (lambda (x y) (+ x (quant-neg y))) 
					  (quantsp-each-quant qpart) :initial-value 0))
				expr
				(normal-lexpr (operator +NEG+) expr nil))))
	(otherwise (error "remove-quant-negation: unexpected error"))))


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
							  (t (error "rename-bound-var(atomic-lexpr): unexpected error")))))
						(main each)))))
		(otherwise (error "rename-bound-var(atomic-lexpr): unexpected error")))
	lexpr))


(defmethod rename-bound-var ((lexpr normal-lexpr) old new)
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 (normal-lexpr operator
	   (rename-bound-var l-lexpr old new)
	   (unless (null r-lexpr)
		 (rename-bound-var r-lexpr old new))))
	(otherwise (error "rename-bound-var(normal-lexpr): unexpected error"))))

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
	(otherwise (error "rename-bound-var(lexpr): unexpected error"))))



(defgeneric prefix (a)
	(:documentation "prefix normalization"))


(defmethod prefix ((lexpr atomic-lexpr))
  lexpr)


(defmethod prefix ((lexpr normal-lexpr))
  )


(defmethod prefix ((lexpr lexpr))
  )





@export
(defun formalize (lexpr)
  (rename-bound-var 
	(literalize 
	  (remove-operator 
		(remove-disuse-quant lexpr))) nil nil))




