

(deftype operator-type ()
  `(satisfies operator-pred))

(deftype term-type ()
  `(satisfies terms-pred))

(deftype normal-lexpr-type ()
  `(satisfies normal-lexpr-pred))


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


(defun terms-pred (terms)
  (and 
	(listp terms)
	(every (lambda (x)
			 (or (vterm-p x)
				 (fterm-p  x))) terms)))



(defstruct (operator (:constructor operator (opr)))
  (opr (error "operator: operator required") 
	   :type operator-type))

(defun operator-pred (op)
  (member op +OPERATOR+ 
		  :test (lambda (x y) 
				  (declare (ignore x))
				  (eq op (car y)))))




;;; normal-lexpr とは 量化子のないような論理式のこと
(defstruct (normal-lexpr (:constructor normal-lexpr (operator l-lexpr r-lexpr)))
  (operator (error "normal-lexpr: operator required") 
			:type operator)
  (l-lexpr  (error "normal-lexpr: left logical expression must be required") 
		    :type normal-lexpr-type)
  ;; operator が neg じゃない時はかならずここは nil ではいけない
  (r-lexpr nil 
		    :type normal-lexpr-type))

(defun normal-lexpr-pred (nlexpr)
  (or (null nlexpr)
	  (atomic-lexpr-p nlexpr)
	  (normal-lexpr-p nlexpr)))

