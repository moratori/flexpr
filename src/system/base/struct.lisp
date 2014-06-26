


(ns:defns flexpr.system.struct
	(:use :cl 
		  :flexpr.system.constant)
	(:import-from :flexpr.system.error
				  :initval-required-error)
	(:export 
	  ;; defstruct で暗黙的に定義される関数が
	  ;; @exportではエクスポートできないみたいなので
	  ;; 手動でexportする	  
	  	:vterm-p
		:vterm-var
		:vterm-const
		
		:fterm-p
		:fterm-fsymbol
		:fterm-terms
		
		:atomic-lexpr-p
		:atomic-lexpr-pred-sym
		:atomic-lexpr-terms

		:%literal-p
		:%literal-negation
		:%literal-pred
		:%literal-terms
		:%literal-used

		:clause-p
		:clause-%literals
		:clause-used
    :clause-defined

		:operator-p
		:operator-opr

		:normal-lexpr-p
		:normal-lexpr-operator
		:normal-lexpr-l-lexpr
		:normal-lexpr-r-lexpr

		:quant-p
		:quant-qnt
		:quant-var
		:quant-neg

		:quantsp-p
		:quantsp-each-quant

		:lexpr-p
		:lexpr-qpart
		:lexpr-expr
	  ))



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

@export
(deftype lexpr-type ()
  `(satisfies lexpr-pred))


;; 最も基本となる 変数のみからなる項
@export
(defstruct (vterm  (:constructor vterm (var const)))
  (var (error (make-condition 'initval-required-error 
							  :ire-where 'vterm)) 
	   :type vterm-type ;;symbol ここをsymbolにしちゃうと 1とか2とかのシンボルで無いのが扱えなくなる
	   )
  (const nil :type boolean))



;; 一つの関数記号と、その引数 terms をとる
;; terms は 各要素が fterm または vterm 型である
@export
(defstruct (fterm (:constructor fterm (fsymbol &rest terms)))
  (fsymbol (error (make-condition 'initval-required-error 
								  :ire-where 'fterm)) 
		   :type symbol)
  (terms   nil
		   :type terms-type))

@export
(defstruct (atomic-lexpr (:constructor atomic-lexpr (pred-sym &rest terms)))
  (pred-sym (error (make-condition 'initval-required-error 
								   :ire-where 'atomic-lexpr))
			:type symbol)
  (terms    nil
			:type terms-type))


@export
(defstruct (%literal (:constructor %literal (negation pred terms used)))
  (negation nil :type boolean)
  (pred (error (make-condition 'initval-required-error 
								   :ire-where '%literal))
		:type symbol)
  (terms nil :type terms-type)
  (used 0 :type integer))


@export
(defstruct (clause (:constructor clause (%literals used)))
  (%literals nil :type list)
  (used 0 :type integer)
  (defined nil :type t)) ;; ?



@export
(defstruct (operator (:constructor operator (opr)))
  (opr (error (make-condition 'initval-required-error 
							  :ire-where 'operator)) 
	   :type operator-type))



;;; normal-lexpr とは 量化子のないような論理式のこと
@export
(defstruct (normal-lexpr (:constructor normal-lexpr (operator l-lexpr r-lexpr)))
  (operator (error (make-condition 'initval-required-error 
								   :ire-where 'normal-lexpr)) 
			:type operator)
  (l-lexpr  (error (make-condition 'initval-required-error 
								   :ire-where 'normal-lexpr)) 
		    :type normal-lexpr-type)
  ;; operator が neg じゃない時はかならずここは nil ではいけない
  (r-lexpr nil 
		    :type normal-lexpr-type))


@export
(defstruct (quant (:constructor quant (qnt var neg)))
  (qnt (error (make-condition 'initval-required-error 
							  :ire-where 'quant)) 
	   :type quant-type)
  (var (error (make-condition 'initval-required-error 
							  :ire-where 'quant)) 
	   :type vterm)
  (neg 0 :type integer) ;; 奇数回、量化子を否定するならここがt
  						;; にしようと思ったけどそれだとはじめから 2n重否定除去をリーダが行っちゃうことになるからよろしくない
  )

@export
(defstruct (quantsp (:constructor quantsp (&rest each-quant)))
  (each-quant nil :type quantsp-type))


@export
(defstruct (lexpr (:constructor lexpr (qpart expr)))
  (qpart (error (make-condition 'initval-required-error 
								:ire-where 'lexpr)) 
		 :type quantsp)
  (expr  (error (make-condition 'initval-required-error 
								:ire-where 'lexpr))
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


