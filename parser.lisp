


(ns:defns flexpr.parser
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :flexpr.util
				  :opr->strength))


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




(defun next-paren-acc (acc head sc ec)
  (cond 
	((string= head sc) (1+ acc))
	((string= head ec) (1- acc))
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
			(main (subseq str 1)
				  (next-paren-acc acc (subseq str 0 1) sc ec)))))) 
	(main (subseq str 1) 1)))


;;;最初と最後がカッコ(sc ecで文字コード)でなくなるまで、意味のないカッコを剥ぐ
(defun strip-paren (str sc ec)
  (cond 
	((or (not (char= (char str 0) (char sc 0))) 
		 (not (char= (char str (1- (length str))) (char ec 0)))) str)
	((innerparen? str sc ec)
	 (strip-paren (subseq str 1 (1- (length str))) sc ec))
	(t str)))


;;; ゴミのスペースを除去っていらないカッコ(sc ec)を剥ぐ
(defun init (str sc ec) 
  (if (string=  str "")
	str
	(strip-paren 
		(strip-bug str) sc ec)))


(defun tokenize-lexpr (str)
  (labels 
	((main (str paren each  result)
	  (if (string= str "") 
		(progn 
	
		  	(unless (zerop paren)
			  (error "tokenize-lexpr: an unmatched parenthesis"))

			(reverse 
			  (if (string= each "") 
				result 
				(cons 
				  (init each +PAREN-START+ +PAREN-END+) 
				  result))))
	
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
  (when (null (express-opr? str))
	(error "string->operator: invalid string"))
  (operator 
	  (second 
		(assoc str 
			   (mapcar 
				 (lambda (x) 
				   (reverse (butlast x)))  +OPERATOR+) :test #'string=))))


(defun weak-point (tklst)
  (let ((oporder 
 			(sort 
			  (remove-if 
				  (lambda (each)
					(not (express-opr? each))) tklst)
			  (lambda (x y) 
				(> (opr->strength 
					 (string->operator x)) 
				   (opr->strength 
					 (string->operator y)))))))

	(unless (null oporder)
		(position-if (lambda (x)
					   (and (express-opr? x)
							(string= x (car oporder))))  tklst
					 :from-end 
					 (every 
					   (lambda (x) 
						 (= (opr->strength 
							  (string->operator (car oporder)))
							(opr->strength 
							  (string->operator x)))) oporder)))))


(defun upper-str? (str)
  (upper-case-p (char str 0)))



(defun place (target src)
  (position-if
	(lambda (x) 
	  (string= x target)) src))



(defun func-format? (str)
  (and
	(position +PAREN-START+ str :test #'string=)
	(position +PAREN-END+ str :test #'string= :from-end t)))



;;; a,b,f(a,g(b)),h(f(b)) => (a b f(a,g(b)) h(f(b)))
(defun tokenize-term (str)
  (labels 
	((main (str paren each result)
		   (if (string= str "") 
			 (progn 

			   (unless (zerop paren)
				 (error "tokenize-lexpr: an unmatched parenthesis"))
			   
			   (reverse 
				 (if (string= each "") 
				   result 
				   (cons 
					 (init each +PAREN-START+ +PAREN-END+) 
					 result))))
			 (let ((head (subseq str 0 1)))
			   (cond

				 ((or (string= head +PAREN-START+)
					  (string= head +PAREN-END+))
 					(main 
						(subseq str 1) 
						(next-paren-acc 
						  paren head +PAREN-START+ +PAREN-END+)
						(concatenate 'string each head) result))

				 ((string= head +ARG-DELIMITER+)
				  (if (zerop paren)
					(main 
					  (subseq str 1) 0 "" 
					  (cons 
						(init each +PAREN-START+ +PAREN-END+) 
						result))
					(main 
					  (subseq str 1) 
					  paren 
					  (concatenate 'string each head) 
					  result)))
				 
				 (t (main (subseq str 1) paren (concatenate 'string each head) result)))))))
	(main str 0 "" nil)))



;;; symbol(term1 , term2 , term3 ...)
;;; のような文字列を
;;; symbol と term1 , term2 , term3 ... の形に分ける
(defun split-func-format (str)
  	(unless (func-format? str)
	  (error "split-func-format: parentheses not found"))
	(let* ((p-s   (position +PAREN-START+ str :test #'string=))
		   (p-e   (position +PAREN-END+ str :test #'string= :from-end t))
		   (sym   (strip-bug (subseq str 0 p-s)))
		   (terms (subseq str (1+ p-s) p-e)))
	  	(values sym (tokenize-term terms))))


;;; x -> (vterm 'x nil)
;;; f(g(x)) -> (fterm 'f (fterm 'g (vterm 'x)))
(defun string->term (str)
  (if (func-format? str)
	(multiple-value-bind (fsym terms) (split-func-format str)
	  (apply #'fterm 
			 (intern fsym)
			 (let ((argvs  (mapcar #'string->term terms)))
			   (when (and (null argvs)
						  (not (upper-str? fsym)))
				 ;; ここのエラーは些か抽象度低い.リーダの役割であるstring->...系の関数で
				 ;; 発生させるべきエラーでないかもしれない
				 (error "string->term: constant function must be start in capitals string"))
			   argvs)))
	(vterm 
	  (intern str) 
	  (upper-str? str))))



;; (atomic-lexpr pred-sym term1 term2 ... )
;; (fterm func-sym term1 term2 ... )
;; (vterm sym const?)
(defun string->atomic (str)
  ;; この辺の低レイヤな関数には、ゴミがついてくることってあったか確認 -> tokenize で綺麗にされる?
  ;; P(x,f(3,g(y,1))) => (atomic-lexpr 'P ...)
  (if (func-format? str)
	(multiple-value-bind (pred-sym terms) (split-func-format str)
	  (apply #'atomic-lexpr 
		(intern pred-sym)
		(mapcar #'string->term terms)))
	;; 関数の形をしてなかったら(括弧がなかったら),そのままインターン
	;; 命題変数として扱うということ
	(atomic-lexpr (intern str))))


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
		(let ((var (strip-bug (subseq str 1))))
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
		  (strip-bug
			(subseq str 0 
					(unless (null nextpos)
					  (1+ nextpos)))) neg)
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
								  (split-quant 
									(subseq str quant-pos)
									(count +NEG-STR+ 
										   (subseq str 0 quant-pos) 
										   :test #'string=))
								  (main n (cons q result)))))
				  (t 
					(error "string->quants: invalid character found")))))))
	(apply #'quantsp (main str nil))))




(defun string->lexpr% (tklst)
  (let ((weak (weak-point tklst)))
	
	(if (null weak) 
	  (string->lexpr (car tklst))
	(let ((left  (subseq tklst 0 weak))
		  (op    (nth weak tklst))
		  (right (subseq tklst (1+ weak))))
	  (normal-lexpr 
		(string->operator op)
		(string->lexpr% left)
		(string->lexpr% right))))))


;; leaf となりうるのは
;; 1. 元から括弧を付けられていて優先度の高い論理式
;; 2. 先頭に否定がついてる場合 +
;; 3. 先頭が量化されてる場合 +
;; 4. 原子式	
;; string->lexpr と言ってるけど
;; lexpr型で返ってくるわけじゃないので注意

@export
(defun string->lexpr (str)
  (let*  ((tklst (tokenize-lexpr str))
		  (wp    (weak-point tklst)))
	(if (null wp)
	  (let* ((target (car tklst))
			 (head (subseq target 0 1)))
		(cond 
			  ((string= +NEG-STR+ head)
			   (normal-lexpr 
				 (string->operator head)
				 (string->lexpr (subseq target 1))
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
				(let ((rtklst (tokenize-lexpr target)))
				  (if (and (= 1 (length rtklst))
						   (string= target (car rtklst)))
					  (string->atomic target)
					  ;; target はバラける系の式だった(原子式でない)	
					  (string->lexpr target))))))
	  
		;; normal-lexpr への変換
		(string->lexpr% tklst))))




