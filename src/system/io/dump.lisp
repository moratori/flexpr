

(ns:defns flexpr.system.dump
	(:use :cl 
		  :flexpr.system.constant
		  :flexpr.system.struct)
	(:import-from :flexpr.system.util
				  :opr-equal?
				  :opr->strength
				  :opr-strong?
				  :opposite-opr
				  :literal?
				  :lexpr-literal?
				  :substitute-term
				  :gliteral?)
	(:import-from :flexpr.system.error
				  :struct-unmatch-error)
	(:import-from :optima
				  :match))


@export
(defgeneric opr->string (opr)
	(:documentation "convert operator 2 string expression"))


(defmethod opr->string ((opr operator))
  (second (assoc (operator-opr opr) +OPERATOR+)))





@export
(defgeneric term->string (a)
	(:documentation "convert term to string expression"))


(defmethod term->string ((term vterm))
  (format nil "~A" (vterm-var term)))


(defmethod term->string ((term fterm))
  (match term
	((fterm :fsymbol fsymbol :terms terms)
	 (format nil "~A(~{~A~^,~})" 
			 fsymbol 
			 (mapcar #'term->string terms)))
	(otherwise (error (make-condition 'struct-unmatch-error 
					   :sue-val term
					   :sue-where 'term->string_fterm)))))





@export
(defgeneric quant->string (quant)
	(:documentation "convert quantifier to string expression"))


(defmethod quant->string ((quant quant))
  (match quant
	((quant :qnt qnt :var var :neg neg)
	 (format nil "~A~A~A"
			 (if (not (zerop neg)) 
			   (make-sequence 'string
							  neg 
							  :initial-element 
							  (char 
				 				(opr->string 
				   					(operator +NEG+)) 0))
			   "")
			 (second (assoc qnt +QUANTS+))
			 (term->string var)))	 
	(otherwise (error (make-condition 'struct-unmatch-error 
					   :sue-val quant
					   :sue-where 'quant->string_quant)))))





@export
(defgeneric quantsp->string (quantsp)
	(:documentation "convert quantifier part to string expression"))

(defmethod quantsp->string ((quantsp quantsp))
  (format nil "~{~A~}." 
		  (mapcar #'quant->string (quantsp-each-quant quantsp))))







@export
(defgeneric lexpr->string (a)
	(:documentation "convert logical expr to string"))


(defmethod lexpr->string ((lexpr atomic-lexpr))
  ;; 実質的に 原始論理式は 関数記号を含んだ項と同型なのでその処理でいく
  (match lexpr
	((atomic-lexpr :pred-sym pred-sym :terms terms)
	 (if (null terms) 
	   (format nil "~A" pred-sym)
	   (term->string 
		 (apply #'fterm pred-sym terms))))
	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'lexpr->string_atomic-lexpr)))))


(defun %lexpr->string (format left op right)
  (format nil format 
		  (lexpr->string left)
		  (opr->string op)
		  (lexpr->string right)))

(defun %lexpr->string_c/dnf (upper-opr under-opr expr)
(cond
  ((and (opr-equal? upper-opr under-opr)
		(or (opr-equal? upper-opr (operator +AND+))
			(opr-equal? upper-opr (operator +OR+))))
   (format nil "~A"   (lexpr->string expr)))
  
  ((= (opr->strength upper-opr)
	  (opr->strength under-opr))
   (format nil "(~A)" (lexpr->string expr)))
  (t (format nil "~A"   (lexpr->string expr)))))

(defmethod lexpr->string ((lexpr normal-lexpr))
  (match lexpr
	((normal-lexpr :operator operator :l-lexpr l-lexpr :r-lexpr r-lexpr)
	 ;;	オペレータの強さと
	 ;;	l-lexprとr-lexprの関係から適当に括弧を省く処理が必要
	 (cond 

	   ((opr-equal? operator (operator +NEG+))
		(if (or (literal?        l-lexpr)
				(lexpr-literal?  l-lexpr))
		  (format nil "~A~A" 
				  (opr->string operator)
				  (lexpr->string l-lexpr))
		  (format nil "~A(~A)" 
				  (opr->string operator)
				  (lexpr->string l-lexpr))))
	  

	   ((and (gliteral? l-lexpr)
			 (gliteral? r-lexpr))
			(%lexpr->string "~A ~A ~A" 
							l-lexpr operator r-lexpr))	

	   ((gliteral? l-lexpr)
	;	(%lexpr->string "~A ~A (~A)" 
	;					l-lexpr operator r-lexpr)
		(format nil "~A ~A ~A"
				(lexpr->string l-lexpr)
				(opr->string operator)
				(%lexpr->string_c/dnf operator 
									  (normal-lexpr-operator r-lexpr)
									  r-lexpr)))

	   ((gliteral? r-lexpr)
		;(%lexpr->string "(~A) ~A ~A"
		;				l-lexpr operator r-lexpr)
		(format nil "~A ~A ~A"
				(%lexpr->string_c/dnf operator 
									  (normal-lexpr-operator l-lexpr)
									  l-lexpr)	
				(opr->string operator)
				(lexpr->string r-lexpr)))

		
	   ((and (normal-lexpr-p l-lexpr)
			 (normal-lexpr-p r-lexpr))
		;; V と & については 結合律が成り立つのでその規則を入れれば
		;; 自然な感じに 連/選言標準形がdumpできる
		(let* ((l-op (normal-lexpr-operator l-lexpr))
			  (r-op (normal-lexpr-operator r-lexpr))
			  (leftstr 
				(%lexpr->string_c/dnf operator l-op l-lexpr))
			   (rightstr 
				(%lexpr->string_c/dnf operator r-op r-lexpr)))
		  (format nil "~A ~A ~A" leftstr (opr->string operator) rightstr)))

	   (t 
		 
		 (%lexpr->string "(~A) ~A (~A)"
						 l-lexpr operator r-lexpr))))	 

	(otherwise 
	  (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'lexpr->string_normal-lexpr)))))



(defmethod lexpr->string ((lexpr lexpr))
  (match lexpr
	((lexpr :qpart qpart :expr expr)
	 (let ((qpstr (quantsp->string qpart)))
	   (if (literal? expr)
		 (format nil "~A~A"   qpstr (lexpr->string expr))
		 (format nil "~A(~A)" qpstr (lexpr->string expr)))))
	(otherwise (error (make-condition 'struct-unmatch-error 
					   :sue-val lexpr
					   :sue-where 'lexpr->string_lexpr)))))

@export
(defun %literal->string (%literal)
	(lexpr->string 
	  (if (%literal-negation %literal)
 		(normal-lexpr (operator +NEG+)
			(apply #'atomic-lexpr 
				   (%literal-pred %literal)
				   (%literal-terms %literal)) nil)
		(apply #'atomic-lexpr 
			   (%literal-pred %literal)
			   (%literal-terms %literal)))))
@export
(defun clause->string (clause op)
  (assert (typep clause 'clause))
  (format nil 
		  (format nil "(~~{~~A~~^ ~A ~~})" (opr->string op))
		  (mapcar #'%literal->string (clause-%literals clause))))

@export
(defun clause-form->string (clause-form op) 
  (format nil
		  (format nil "~~{~~A~~^ ~A ~~}" (opr->string op))
		  (mapcar 
			(lambda (clause)
	  			(clause->string 
				  clause
				  (operator (opposite-opr op)))) clause-form)))


@export
(defun lexpr->string_clear (lexpr)
  ;; lexpr must be prenex normal form
  ;; 
  ;; gensymして正規化された束縛変数だと余りにも見づらいので
  ;; 適当にw x y zとかに置き換えるだけのクソ機能
  ;; lexprの束縛変数がgensym使って正規化されてる時に使わないとなんの意味もないどころか
  ;; parse されたての式に使ったら束縛変数増やすことになりかねないので要注意
  ;; つまり、自由変数を含むようなやつにやってもだめ
  (let ((conv (lexpr->string lexpr)))
	(if (typep lexpr 'lexpr)
	  (let ((qpart (quantsp-each-quant (lexpr-qpart lexpr))))
		;; ここのマジックナンバー 7っていうのは
		;; 置き換える束縛変数用に w x y zしか用意しないクソ仕様なので
		;; てか本来束縛変数は 可算無限なんだからそれを、すべて慣例的な変数名で
		;; 表すこと自体不可能
		;; どうしても x1 x2 みたいにしかなり得ない
		(if (< (length qpart) 7)
		  (let ((rule (mapcar 
						(lambda (x y) (list (quant-var x) y)) 
						qpart 
						(list 
						  (vterm '|x| nil)
						  (vterm '|y| nil)
						  (vterm '|z| nil)
						  (vterm '|w| nil)
						  (vterm '|u| nil)
						  (vterm '|v| nil)))))
			(lexpr->string 
			  (reduce 
				(lambda (init x) 
				  (destructuring-bind (old new) x
					(substitute-term init old new))) rule :initial-value lexpr)))
		  conv))conv)))


@export
(defun mgu->string (mgu)
  (when (listp mgu)
	(loop for each in mgu
		if (listp each)
		  collect 
		  (destructuring-bind (old . new) each
			(format nil "~A -> ~A"
				(term->string old)
				(term->string new))))))

;; resolution-gen で出力された defnode の結果を dot 形式の文字列にする
@export
(defun defnode->dot (stream defnode)
  (format stream "~A"
		  (apply #'concatenate 'string 
		 (append
       (let ((normal (format nil "~16R" 
                             (- #xffffff 
                                (parse-integer 
                                  +NODE-COLOR+
                                  :radix 16)))))
       (mapcar 
		   (lambda (x)
			 (destructuring-bind (lexpr name def?) x
			   (format 
           nil 
           "~2t~A[label=\"~A\",style=\"filled\",fillcolor=\"#~A\"];~%" 
           name 
           lexpr
           (if def? +NODE-COLOR+ normal))))
		   defnode)  
         ) 
		   (list (format nil "~%"))))))

@export
(defun relation->dot (stream relation)
  (dolist (each relation)
	(destructuring-bind (from to) each
	  (format stream "~2t~A -> ~A;~%" from to))))

(defun simplify (defnode relation)
  (remove-if 
	(lambda (x)
	  (destructuring-bind (lexpr name def?) x
      (declare (ignore def?))
		(every 
		  (lambda (x)
			(and 
			  (string/= (first x) name)
			  (string/= (second x) name))) 
		  relation)))
	defnode))

@export
(defun out-tree (path defnode relation)
  (with-open-file (out path :direction :output :if-exists :supersede)
	(format out "digraph ~A {~%" (pathname-name path))
	(defnode->dot out (simplify defnode relation))
	(relation->dot out relation)
	(format out "}")))


@export
(defun deb-trace-kbcompl (eqexpr left right ruleset)
  (destructuring-bind (flag . mode) +TRACE+
    (declare (ignore mode))
    (when flag
      (format t "~%Inference ----------------------")
      (format t "~%Rewriting Rule:~%~{~2t~A~%~}"
              (mapcar 
                (lambda (each)
                  (format nil "~A -> ~A"
                          (term->string (rw-rule-left each)) 
                          (term->string (rw-rule-right each)))) (reverse ruleset)))
      (format t "~%equation:~%~2t~A = ~A" 
              (term->string (eqexpr-left eqexpr) )
              (term->string (eqexpr-right eqexpr)))
      (format t "~%formal form of left side: ~%~2t~A"
              (term->string left)
              )
      (format t "~%formal form of right side: ~%~2t~A"
              (term->string right))

      (format t   "~%--------------------------------~%")

      )
    )
  )


@export
(defun deb-trace-resolution (mgu p1 p2 child)
  (destructuring-bind (flag . mode) +TRACE+
    (when flag 

      (format t "~%Inference ----------------------")
      (if (eq t mgu) 
        (format t "~%MGU: ~A~%" mgu)
        (format t "~%MGU:~%~{~2t~A~%~}"
                (mapcar 
                  (lambda (each)
                    (format nil "~A -> ~A" 
                            (term->string (car each))
                            (term->string (cdr each)))) mgu)))
      (format t "~%ParentClause: ~%~2t~A~%" 
              (clause->string p1 (operator +OR+)))
      (format t "ParentClause: ~%~2t~A~%" 
              (clause->string p2 (operator +OR+)))
      (format t "Resolvent (or Paramodulant): ~%~{~2t~A~%~}" 
              (mapcar 
                (lambda (x) 
                  (clause->string x (operator +OR+))) child))
      (format t   "--------------------------------~%")


      (if (integerp mode)
        (sleep mode)
        (progn 
          (format t "Enter...")
          (force-output *standard-output*)
          (read-line *standard-input*))))))

