


(defpackage struct 
  (:use :cl
		:constant)
  (:export 
		:vterm
		:vterm-var
		:vterm-const
		:var
		:const
		
		:fterm
		:fterm-fsymbol
		:fterm-terms
		:fsymbol
		:terms
		
		:atomic-lexpr
		:atomic-lexpr-pred-sym
		:atomic-lexpr-terms
		:pred-sym
		:terms

		:operator
		:operator-opr
		:opr

		:normal-lexpr
		:normal-lexpr-operator
		:normal-lexpr-l-lexpr
		:normal-lexpr-r-lexpr
		:operator
		:l-lexpr
		:r-lexpr

		:quant
		:quant-qnt
		:quant-var
		:quant-neg
		:qnt
		:var
		:neg

		:quantsp
		:quantsp-each-quant
		:each-quant

		:lexpr
		:lexpr-qpart
		:lexpr-expr
		:qpart
		:expr
		))
(in-package struct)


#|

	definition of data structure

	(vterm var const)
	(fterm fsymbol terms)
	(atomic-lexpr pred-sym terms)
	(operator opr)
	(normal-lexpr operator l-lexpr r-lexpr)
	(quant qnt var neg)
	(quantsp each-quant)
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
(defstruct (vterm  (:constructor vterm (var const)))
  (var (error "vterm: variable symbol required") 
	   :type vterm-type ;;symbol ここをsymbolにしちゃうと 1とか2とかのシンボルで無いのが扱えなくなる
	   )
  (const nil :type boolean))



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



(defstruct (quant (:constructor quant (qnt var neg)))
  (qnt (error "quants: qnt required") :type quant-type)
  (var (error "quants: var required") :type vterm)
  (neg 0 :type integer) ;; 奇数回、量化子を否定するならここがt
  						;; にしようと思ったけどそれだとはじめから 2n重否定除去をリーダが行っちゃうことになるからよろしくない
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


