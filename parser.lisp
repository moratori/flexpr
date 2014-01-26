


(ns:defns parser
	(:use :cl
		  :constant
		  :struct
		  :util))


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

(defun strip-bug (str)
  (string-trim '(#\space #\Return #\Newline) str))

(defun innerparen? (str sc ec)
  ;; これが呼ばれたという事は少なくとも
  ;; 始めが ( で始まり 最後が ) で終わるような文字列であるということ
  (labels 
	((main (str acc)
		(cond
		  ((string= str "")
		    (unless (zerop acc)
			  (error "innerparen?: parenthesis error"))
			t)
		  ((zerop acc) 
		   nil)
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
		(strip-bug str) 
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


(defun place (target src)
  (position-if
	(lambda (x) 
	  (string= x target)) src))




;; (atomic-lexpr pred-sym term1 term2 ... )
;; (fterm func-sym term1 term2 ... )
;; (vterm sym const?)
(defun string->atomic (str)
  ;; P(x,f(3,g(y,1))) => (atomic-lexpr 'P ...)
  ;; 0項述語に対応するなら以下のエラーを削除するべき

  (let ((p-s (position +PAREN-START+ str :test #'string=))
		(p-e (position +PAREN-END+ str :test #'string= :from-end t)))
		
		(when (or (null p-s)
				  (null p-e))
		  (error "string->atomic: parentheses not found"))

		;; Pred(x,y)
		;; PRED: Pred
		;; terms: x,y
		(let ((pred  (strip-bug (subseq str 0 p-s)))
			  (terms (subseq str (1+ p-s) p-e)))



		  )

	)

  )


(defun string->quant (str neg-cnt)
  ; Avar -> (quant +FORALL+ (vterm 'var))
  (labels
	((s->q (s)
		(cond 
		  ((string= s +FORALL-STR+) 
		   +FORALL+)
		  ((string= s +EXISTS-STR+)
		   +EXISTS+)
		  (t (error "s->q: unexpected error"))))
	 (main ()
		(let ((var (subseq str 1)))
		  (quant 
		  	(s->q (subseq str 0 1))
			 (vterm (intern var) (upper-str? var))
		 	 neg-cnt))))
	(main)))


(defun split-quant (str neg)
  ;; quant 一個分だけ (quant )型にして切り出す
  ;; あと次に回す文字列も
	(let ((nextpos
				(position-if 
					(lambda (x) 
						(or (string= x +NEG-STR+)
							(string= x +FORALL-STR+)
							(string= x +EXISTS-STR+))) (subseq str 1))))
	  (values 
		(string->quant 
		  (subseq str 0 
				  (if (null nextpos) 
					nil 
					(1+ nextpos))) neg)
		(if (null nextpos) 
		  "" 
		  (subseq str (1+ nextpos))))))


(defun string->quantsp (str)
  ;; AxAyAy~Ew => (quantsp (quant +FORALL+ (vterm )))
  ;; AxAy~~Az~~~Ez
  ;; Ax~Ey
  (labels 
	((main (str result)
		(if (string= str "") 
		  (reverse result)
		  (let ((head (subseq str 0 1)))

				(cond 
				  ((express-quant? head)
				   	(multiple-value-bind (q n) (split-quant str 0)
					  (main n (cons q result))))
				 
				  ((string= +NEG-STR+ head)
				   		;; ~AxEy
						;; ~~~AxEy
				   		(let ((quant-pos 
								(position-if 
								  (lambda (x)
									(or (string= x +FORALL-STR+)
										(string= x +EXISTS-STR+))) str)))
						  		
						  		(when (null quant-pos)
								  (error "string->quants: quantifier required"))

								(multiple-value-bind (q n) 
								  (split-quant (subseq str quant-pos) quant-pos)
								  (main n (cons q result)))))
				  (t 
					(error "string->quants: invalid character found")))))))
	(apply #'quantsp (main str nil))))


;; leaf となりうるのは
;; 1. 元から括弧を付けられていて優先度の高い論理式
;; 2. 先頭に否定がついてる場合 +
;; 3. 先頭が量化されてる場合 +
;; 4. 原子式	
;; string->lexpr と言ってるけど
;; lexpr型で返ってくるわけじゃないので注意

@export
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
			   (let ((dot-pos (place +DELIMITER+ target)))
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




