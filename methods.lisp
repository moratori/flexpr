


#|

	(atomic-lexpr pred-sym term1 ...)
	(normal-lexpr operator left-lexpr rigt-lexpr)
	(quant qnt var)
	(quantsp (quant qnt var) ...)
	(lexpr qpart expr)
	
|#


(deftype operator-type ()
  `(satisfies operator-pred))

(deftype vterm-type ()
  `(satisfies vterm-pred))

(deftype terms-type ()
  `(satisfies terms-pred))

(deftype normal-lexpr-type ()
  `(satisfies normal-lexpr-pred))

(deftype quant-type ()
  `(satisfies quant-pred))

(deftype quantsp-type ()
  `(satisfies quantsp-pred))

(deftype lexpr-type ()
  `(satisfies lexpr-pred))


;; 最も基本となる 変数のみからなる項
(defstruct (vterm  (:constructor vterm (var freev)))
  (var (error "vterm: variable symbol required") 
	   :type vterm-type ;;symbol ここをsymbolにしちゃうと 1とか2とかのシンボルで無いのが扱えなくなる
	   )
  (freev nil :type boolean))



;; 一つの関数記号と、その引数 terms をとる
;; terms は 各要素が fterm または vterm 型である
(defstruct (fterm (:constructor fterm (fsymbol &rest terms)))
  (fsymbol (error "fterm: function symbol required") 
		   :type symbol)
  (terms   nil
		   :type terms-type))


(defstruct (atomic-lexpr (:constructor atomic-lexpr (pred-sym &rest terms)))
  (pred-sym (error "atomic-lexpr: predicate symbol required")
			:type symbol)
  (terms    nil
			:type terms-type))


(defstruct (operator (:constructor operator (opr)))
  (opr (error "operator: operator required") 
	   :type operator-type))



;;; normal-lexpr とは 量化子のないような論理式のこと
(defstruct (normal-lexpr (:constructor normal-lexpr (operator l-lexpr r-lexpr)))
  (operator (error "normal-lexpr: operator required") 
			:type operator)
  (l-lexpr  (error "normal-lexpr: left logical expression must be required") 
		    :type normal-lexpr-type)
  ;; operator が neg じゃない時はかならずここは nil ではいけない
  (r-lexpr nil 
		    :type normal-lexpr-type))



(defstruct (quant (:constructor quant (qnt var &optional (neg nil))))
  (qnt (error "quants: qnt required") :type quant-type)
  (var (error "quants: var required") :type vterm)
  (neg nil :type boolean) ;; 奇数回、量化子を否定するならここがt
  )


(defstruct (quantsp (:constructor quantsp (&rest each-quant)))
  (each-quant nil :type quantsp-type))



(defstruct (lexpr (:constructor lexpr (qpart expr)))
  (qpart (error "lexpr: qpart required") 
		 :type quantsp)
  (expr  (error "logical expression required")
		:type lexpr-type))


(defun vterm-pred (term)
  (or (symbolp term) (numberp term)))

(defun terms-pred (terms)
  (and 
	(listp terms)
	(every (lambda (x)
			 (or (vterm-p x)
				 (fterm-p  x))) terms)))

(defun operator-pred (op)
  (member op +OPERATOR+ 
		  :test (lambda (x y) 
				  (declare (ignore x))
				  (eq op (car y)))))

(defun normal-lexpr-pred (nlexpr)
  (or (null nlexpr)
	  (atomic-lexpr-p nlexpr)
	  (normal-lexpr-p nlexpr)
	  (lexpr-p nlexpr)))

(defun quant-pred (q)
  (member q +QUANTS+ :test 
		  (lambda (x y) 
			(declare (ignore x))
			(eq q (car y)))))

(defun quantsp-pred (ls)
  (every #'quant-p ls))


(defun lexpr-pred (l)
  (or (atomic-lexpr-p l)
	  (normal-lexpr-p l)
	  (lexpr-p l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









(defmethod opr->string ((opr operator))
  (second (assoc (operator-opr opr) +OPERATOR+)))


(defmethod opr->strength ((opr operator))
  (third  (assoc (operator-opr opr) +OPERATOR+)))


(defmethod opr-strong? ((opr1 operator) (opr2 operator))
  (< (opr->strength opr1)
	 (opr->strength opr2)))


(defmethod opr-equal? ((opr1 operator) (opr2 operator))
  (eq (operator-opr opr1)
	  (operator-opr opr2)))








(defgeneric term->string (a)
	(:documentation "convert term to string"))


(defmethod term->string ((term vterm))
  (format nil "~A" (vterm-var term)))


(defmethod term->string ((term fterm))
  (match term
	((fterm fsymbol terms)
	 (format nil "~A(~{~A~^,~})" 
			 fsymbol 
			 (mapcar #'term->string terms)))
	(otherwise (error "term->string: invalid data structure"))))







(defmethod quant->string ((quant quant))
  (match quant
	((quant qnt var neg)
	 (format nil "~A~A~A"
			 (if neg (opr->string (operator +NEG+)) "")
			 (second (assoc qnt +QUANTS+))
			 (term->string var)))	 
	(otherwise (error "quant->string: invalid data structure"))))


(defmethod quantsp->string ((quantsp quantsp))
  (format nil "~{~A~}." 
		  (mapcar #'quant->string (quantsp-each-quant quantsp))))







(defgeneric lexpr->string (a)
	(:documentation "convert logical expr to string"))


(defmethod lexpr->string ((lexpr atomic-lexpr))
  ;; 実質的に 原始論理式は 関数記号を含んだ項と同型なのでその処理でいく
  (match lexpr
	((atomic-lexpr pred-sym terms)
	 (term->string 
	   (apply #'fterm pred-sym terms)))
	(otherwise 
	  (error "lexpr->string(atomic-lexpr): invalid data structure"))))


(defmethod lexpr->string ((lexpr normal-lexpr))
  (match lexpr
	((normal-lexpr operator l-lexpr r-lexpr)
	 ;;	オペレータの強さと
	 ;;	l-lexprとr-lexprの関係から適当に括弧を省く処理が必要
	 (cond 
	   ((opr-equal? operator (operator +NEG+))
		(format nil "~A(~A)" 
				(opr->string operator)
				(lexpr->string l-lexpr)))
	   (t (format nil "(~A) ~A (~A)"
				(lexpr->string l-lexpr)
				(opr->string operator)
				(lexpr->string r-lexpr)))))	 
	(otherwise 
	  (error "lexpr->string(normal-lexpr): invalid data structure"))))



(defmethod lexpr->string ((lexpr lexpr))
  (match lexpr
	((lexpr qpart expr)
	 (format nil "~A(~A)" 
			 (quantsp->string qpart)
			 (lexpr->string expr)))
	(otherwise (error "lexpr->string(lexpr): invalid data structure"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

























#|

	*一階述語論理の文字列表現の定義

	<BOUND-VAR>  ::= <LISP-CAPITAL-SYMBOL>
	<FREE-VAR>   ::= <LISP-SMALL-SYMBOL>

	<FUNC-SYM>   ::= <LISP-SMALL-SYMBOL>
	<PRED-SYM>   ::= <LISP-CAPITAL-SYMBOL>	

	<TERM>       ::= <BOUND-VAR> | <FREE-VAR> | <FUNC-SYM> "(" <TERM>,+  ")"

	<OPERATOR>   ::= > | & | V | ~ | -
	
	<QUANTIFIER> ::= A | E
	<QUANTS>     ::= <QUANTIFIER> <BOUND-VAR>
	<QUANTS-PART>::= <QUANTS>+ "."

	<ATOMIC>     ::= <PRED-SYM> "(" <FREE-VAR>,+ ")"
	<EXPR>       ::= 
		<ATOMIC> | "(" <EXPR> <OPERATOR> <EXPR> ")" | <QUANTS-PART> <EXPR> 

|#




(defun next-paren-acc (acc c sc ec)
  (cond 
	((char= c sc) (1+ acc))
	((char= c ec) (1- acc))
	(t acc)))

(defun innerparen? (str sc ec)
  ;; これが呼ばれたという事は少なくとも
  ;; 始めが ( で始まり 最後が ) で終わるような文字列であるということ
  (labels 
	((main (str acc)
		(cond
		  ((string= str "")
			(if (zerop acc) 
		 	 	t 
		  		(error "parenthesis error!")))
		  ((zerop acc) nil)
		  (t 
		 	(let ((head (char str 0)))
		   		(main (subseq str 1)
					  (next-paren-acc acc head sc ec))))))) 
	(main (subseq str 1) 1)))


;;;最初と最後がカッコ(sc ecで文字コード)でなくなるまで、意味のないカッコを剥ぐ
(defun strip-paren (str sc ec)
  (cond 
	((or (not (char= (char str 0) sc)) 
		 (not (char= (char str (1- (length str))) ec))) str)
	((innerparen? str sc ec)
	 (strip-paren (subseq str 1 (1- (length str))) sc ec))
	(t str)))


;;; ゴミのスペースを除去っていらないカッコ(sc ec)を剥ぐ
(defun init (str sc ec) 
  (if (string=  str "")
	str
	(strip-paren 
		(string-trim 
	  		'(#\space #\Newline #\Return) str) 
		(char sc 0) 
		(char ec 0))))


(defun tokenize (str)
  (labels 
	((main (str paren each  result)
	  (if (string= str "") 
		(reverse 
		  (if (string= each "") 
			result 
			(cons (init each +PAREN-START+ +PAREN-END+) result)))
		(let ((head (subseq str 0 1)))
		  (cond 
			((and (member-if 
					(lambda (x) 
					  (string= head (second x))) +OPERATOR+)
				  (zerop paren)
				  (string/= head +NEG-STR+)) 
			 (main (subseq str 1) 0 "" 
				   (if (string= each "") 
					 (cons head result)
					 (cons head (cons (init each +PAREN-START+ +PAREN-END+) result)))))

			((string= head +PAREN-START+)
			 (main (subseq str 1) (1+ paren) 
				   (concatenate 'string each head) result))

			((string= head +PAREN-END+)
			 (main (subseq str 1) (1- paren) 
				   (concatenate 'string each head) result))
			  
			(t (main (subseq str 1) paren 
					 (concatenate 'string each head) result)))))))
	(main str 0 "" nil)))

(defun split (str spliter)
  (labels 
	((main (str acc  result)
	 (if (string= "" str) (reverse (cons acc result))
	 	(if (char= (char str 0) spliter)
	   		(main (subseq str 1) "" (cons acc result))
	   		(main (subseq str 1) 
			 (concatenate 'string acc (string (char str 0))) result)))))
	(main str "" nil)))



(defun express-opr? (str)
  ;; 文字列がオペレータを表しているか?
  (some 
	(lambda (x)
	  (string= str (second x))) +OPERATOR+))


(defun express-quant? (str)
  (some 
	(lambda (x) 
	  (string= str (second x))) +QUANTS+))


(defun string->operator (str)
  (if (null (express-opr? str))
	(error "string->operator: invalid string")
	(operator 
	  (second 
		(assoc str 
			   (mapcar 
				 (lambda (x) 
				   (reverse (butlast x)))  +OPERATOR+) :test #'string=)))))


(defun weak-point (tklst)
  (let ((oporder 
 			(sort 
			  (mapcar 
				(lambda (x) 
				  (string->operator x))
				(remove-if 
				  (lambda (each)
					(not (express-opr? each))) tklst))
			  (lambda (x y) 
				(> (opr->strength x) (opr->strength y))))))
	(if (null oporder) nil
		(position-if #'express-opr?  tklst
		  :from-end 
		  (every 
			(lambda (x) 
			  (= (opr->strength (car oporder))
				 (opr->strength x))) oporder)))))


(defun upper-str? (str)
  (upper-case-p (char str 0)))


(defun string->atomic (str)
  ;; P(x,f(x,g(y,z))) => (atomic-lexpr 'P ...)
  )


(defun string->quantsp (str)
  ;; AxAyAy~Ew => (quantsp (quant +FORALL+ (vterm )))
  ;; AxAy~~Az~~~Ez
  (labels 
	((main (str result)
		(if (string= str "") (reverse result)

		  )
	 ))

	(apply #'quantsp (main str nil)))


  )


;; leaf となりうるのは
;; 1. 元から括弧を付けられていて優先度の高い論理式
;; 2. 先頭に否定がついてる場合 +
;; 3. 先頭が量化されてる場合 +
;; 4. 原子式	

(defun string->lexpr (str)
  (let*  ((tklst (tokenize str))
		  (wp    (weak-point tklst)))
	(if (null wp)
	  (let* ((target (car tklst))
			 (head (subseq target 0 1)))
		(cond 
			  ((string= +NEG-STR+ head)
			   (normal-lexpr 
				 (string->operator head)
				 (string->lexpr (subseq str 1))
				 nil))

			  ((express-quant? head)
			   (let ((dot-pos (position-if 
								(lambda (x)
								  (string= x +DELIMITER+)) target)))
				 	(when (null dot-pos)
					  (error "string->lexpr: parse error delimiter required"))
					(lexpr 
					  (string->quantsp 
						(subseq target 0 dot-pos))
					  (string->lexpr 
						(subseq target (1+ dot-pos))))))	

			  (t 
				 ;; 原子式なら tokenize してもバラけないはず
				(let ((rtklst (tokenize target)))
				  (if (and (= 1 (length rtklst))
						   (string= target (car rtklst)))
					  (string->atomic target)
					  ;; target はバラける系の式だった(原子式でない)	
					  (string->lexpr target))))))
	  
		;; normal-lexpr への変換

	  )))








