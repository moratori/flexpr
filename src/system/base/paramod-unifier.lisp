

(ns:defns flexpr.system.paramod-unifier
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct
      :flexpr.system.unifier)
	(:import-from :flexpr.system.util
          :substitute-term
				  :term=)
  (:import-from :flexpr.system.unifier
   :mgu
   :unify)
  )

#|
  a = b
  P(f(x))
  rec-match(a,P(f(x)))
  x -> a
  rec-matchといえども基底はmguだから
  置換基礎のドメインは普通に変数だわ
|#

@export
(defun rec-match (term1 term2)

  ;; term1 は 必ず等号の左辺でなければいけないとする
  ;; -> そういうのじゃなくて、置き換え元が定数となるようなことがあり得ない
  ;;    ようにすればいいだけじゃね???

  
  (assert 
    (and (or (typep term1 'vterm)
             (typep term1 'fterm))
         (or (typep term2 'vterm)
             (typep term2 'fterm))))
  (cond 
    ((and (typep term1 'vterm)
          (typep term2 'vterm))
     (mgu term1 term2))
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
             (rec-match term1 each-term)) (fterm-terms term2)))))
    ((and (typep term1 'fterm)
          (typep term2 'vterm))
     ;(rec-match term2 term1)
     (mgu term2 term1))))


(defun update-term (rule init)
  (if (eq rule t) init
    (reduce 
    (lambda (res e)
      (substitute-term res (car e) (cdr e))) rule
    :initial-value init)))

@export
(defun eliminate-paramod? (x y)
  (assert (and (typep x '%literal) (typep y '%literal)))

  (let ((pred1 (%literal-pred x))
        (pred2 (%literal-pred y))
        (terms1 (%literal-terms x))
        (terms2 (%literal-terms y)))
    (cond 
      ((and (not (%literal-negation x))
            (eq +EQUAL+ pred1))
       (let* ((left (first terms1))
             (rec-rule (some (lambda (x)(rec-match left x)) terms2)))
         (if (null rec-rule) (values nil nil nil)
           (values 
             rec-rule 
             y 
             (update-term rec-rule left)
             (update-term rec-rule (second terms1)) ))))
      ((and (not (%literal-negation y))
            (eq +EQUAl+ pred2))
       (eliminate-paramod? y x))
      (t (values nil nil nil)))))



;; ドメインが fterm だった場合の置換
;;
(defun substitute-fterm (tar old new)
  (cond 
    ((vterm-p tar)
     tar)
    ((term= tar old) new)
    (t 
     (assert (fterm-p tar))
     (apply #'fterm 
            (fterm-fsymbol tar)
            (mapcar 
              (lambda (each)
                (substitute-fterm each old new)
                ) 
              (fterm-terms tar))))))



;; ここを直せばおｋ
;; rule のドメインには fterm が含まれる
;; unify と違い %literal を返すので注意
@export
(defun paramod-unify (pair lit)
  ;; ここでの clause は clause といいつつも
  ;; リテラルが一個入ってるだけであることを
  ;; 仮定していい

  (assert (typep lit '%literal))
  
  (destructuring-bind (old . new) pair
    (%literal
      (%literal-negation lit)
      (%literal-pred lit)
      (let ((targets (%literal-terms lit)))
        (if (vterm-p old) 
          (mapcar
            (lambda (t-term)
              (substitute-term t-term old new)) 
            targets)
          (mapcar 
            (lambda (t-term)
              (substitute-fterm t-term old new))
            targets))) 
      (%literal-used lit)))
  
  )
