
#|

	(atomic-lexpr pred-sym term1 ...)
	(normal-lexpr operator left-lexpr rigt-lexpr)
	(quant qnt var)
	(quantsp (quant qnt var) ...)
	(lexpr qpart expr)
	
|#


(deftype operator-type ()
  `(satisfies operator-pred))

(deftype term-type ()
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
(defstruct (vterm  (:constructor vterm (var)))
  (var (error "vterm: variable symbol required") 
	   :type t ;;symbol ここをsymbolにしちゃうと 1とか2とかのシンボルで無いのが扱えなくなる
	   ))



;; 一つの関数記号と、その引数 terms をとる
;; terms は 各要素が fterm または vterm 型である
(defstruct (fterm (:constructor fterm (fsymbol &rest terms)))
  (fsymbol (error "fterm: function symbol required") 
		   :type symbol)
  (terms   nil
		   :type term-type))


(defstruct (atomic-lexpr (:constructor atomic-lexpr (pred-sym &rest terms)))
  (pred-sym (error "atomic-lexpr: predicate symbol required")
			:type symbol)
  (terms    nil
			:type term-type))


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
  (neg nil :type boolean) ;; 奇数回、量化子を否定するならここがt
  (qnt (error "quants: qnt required") :type quant-type)
  (var (error "quants: var required") :type vterm))


(defstruct (quantsp (:constructor quantsp (&rest each-quant)))
  (each-quant nil :type quantsp-type))



(defstruct (lexpr (:constructor lexpr (qpart expr)))
  (qpart nil :type quantsp)
  (expr (error "logical expression required")
		:type lexpr-type))



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





