

(ns:defns flexpr.system.paramod-unifier
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct
      :flexpr.system.unifier)
	(:import-from :flexpr.system.util
				  :term=)
  (:import-from :flexpr.system.unifier
   :mgu)
  )

#|
  a = b
  P(f(x))
  rec-match(a,P(f(x)))
  x -> a
|#

@export
(defun rec-match (term1 term2)

  (assert 
    (and (or (typep term1 'vterm)
             (typep term1 'fterm))
         (or (typep term2 'vterm)
             (typep term2 'fterm))))
  (cond 
    ((and (typep term1 'vterm)
          (typep term2 'vterm))
     (mgu term1 mter2))
    ((or 
       (and (typep term1 'vterm)
            (typep term2 'fterm))
       (and (typep term1 'fterm)
            (typep term2 'fterm)))
     ;; 関数項の初めから見ていき
     ;; 初めにマッチしたものを返す。
     ;; 全ては見ない
     ;; recmatch(a,f(b,x,y))
     ;; とかだったら x -> a のみを返す
     (let ((first-order-mgu (mgu term1 term2)))
       (if first-order-mgu 
         first-order-mgu
         (some 
           (lambda (each-term)
             (mgu term1 each-term)) (fterm-terms term2)))))
    ((and (typep term1 'fterm)
          (typep term2 'vterm))
     (rec-match term2 term1))))



(defun eliminate-paramod? (x y)
  (assert (and (typep x '%literal) (typep y '%literal)))

  (let ((pred1 (%literal-pred x))
        (pred2 (%literal-pred y))
        (terms1 (%literal-terms x))
        (terms2 (%literal-terms y)))
    (cond 
      ((eq +EQUAL+ pred1)
       )
      ((eq +EQUAl+ pred2)
       )
      (t nil))))














