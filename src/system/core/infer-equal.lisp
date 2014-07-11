
(ns:defns flexpr.system.infer.equal
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
  (:import-from :flexpr.system.error
   :endless-critical-pair-error
   :endless-regularization-error
   :set-of-equation-required-error
   :same-complexity-error)
  (:import-from :flexpr.system.util
    :equal-literal?
    :term-using?
    :substitute-term 
    :term=
    :fuzzy-term=)
  (:import-from :flexpr.system.unifier
   :subst-term
   :subst-old-rule
   :subst-new-rule
   :safe-mgu
   :absurd)
  (:import-from :flexpr.system.paramod-unifier
   :substitute-fterm
   :substitute-term-recursive
   :rec-match)
  (:import-from :flexpr.system.dump
   :deb-trace-kbcompl)
  )



(defvar +LIMIT+ 50)



(defmethod get-order ((term vterm))
  (if (vterm-const term) 1 2))

(defmethod get-order ((term fterm))
  (1+ (apply #'+ (mapcar (lambda (x) (* (length (fterm-terms term)) (get-order x)))  (fterm-terms term)))))


(defun get-regular% (term rules)
  (get-regular term rules +LIMIT+)
  )

;;; 正規形を求める
;;; つまりこれ以上簡約できない形まで変形する
(defmethod get-regular ((term vterm) rules cnt)
  (assert (and (listp rules) (every (lambda (x) (typep x 'rw-rule)) rules)))

  (when (> 0 cnt)
    (error (make-condition 'endless-regularization-error)))
  
  ;; どの規則のドメインにも合致しないなら
  ;; やってみて、変わってたらもっかいにした方がよくないか？
  ;; fterm版ではそうする
  (if (every (lambda (rule) (not (safe-mgu (rw-rule-left rule) term))) rules)
    term
    
    (get-regular

      (reduce 
      (lambda (res each)
        (let* ((left (rw-rule-left each))
               (right (rw-rule-right each))
               (unifier (safe-mgu res left)))
          (cond 
            ((null unifier) res)
            ((eq unifier t) right)
            (t 
             ;; ここ unifier 使う必要なくね?
            (let ((one (car unifier)))
              (substitute-term right (car one) (cdr one)))))))
      rules
      :initial-value term)
      
      rules
      (1- cnt)
      )
    )) 
 

(defun matching (term unifier)
  (if (eq unifier t)
    term
    (reduce 
    (lambda (res x)
      (substitute-term res (car x) (cdr x)))
    unifier
    :initial-value term
    )
    )
  
  )


(defmethod get-regular ((term fterm) rules cnt)
(when (> 0 cnt)
    (error (make-condition 'endless-regularization-error)))
  (let ((result 
          (reduce 
    (lambda (res each)
      (let* ((r-left (rw-rule-left each))
             (r-right (rw-rule-right each))
             (unifier (rec-match r-left term)))


        (if (null unifier)
          res
          (substitute-term-recursive 
            (cons 
              (matching r-left unifier) 
              (matching r-right unifier))
            (matching res unifier)

            ))))
    rules
    :initial-value term)))
    (if (term= result term) result
      (get-regular result rules (1- cnt)))))

 

(defmethod show ((term vterm))
  (format nil "~A" (vterm-var term)))

(defmethod show ((term fterm))
  (format nil "~A(~{~A~^,~})"
          (symbol-name (fterm-fsymbol term))
          (mapcar #'show (fterm-terms term))))
 

(defmethod rule= ((r1 rw-rule) (r2 rw-rule))
  (and 
    (term= (rw-rule-left r1) (rw-rule-left r2))
    (term= (rw-rule-right r1) (rw-rule-right r2))))



(defun get-critical-pair (r)
  (let (result)
    (loop for r1 in r do 
      (loop for r2 in r do
        (loop for target in r
           if (and (not (rule= r1 r2)) 
                   (not (rule= r1 target))
                   (not (rule= r2 target))
                   ) do
           (let* ((left (rw-rule-left target))
                  (reg-left1 (get-regular% left (list r1)))
                  (reg-left2 (get-regular% left (list r2)))
                  (right (rw-rule-right target))
                  (reg-right1 (get-regular% right (list r1)))
                  (reg-right2 (get-regular% right (list r2))))
             
             (when (not (term= reg-left1 reg-left2))
               (push (eqexpr reg-left1 reg-left2) result))

             (when (not (term= reg-right1 reg-right2))
               (push (eqexpr reg-right1 reg-right2) result))))))
    result
    ) 
  )


(defun regular-rule (r)
  r)

(defun regular-eq (e)
  e)



(defun dump-rules (r)
  (dolist (each r)
    (format t "~A -> ~A~%" (show (rw-rule-left each)) (show (rw-rule-right each)))
    )
  )

(defun dump-eqexpr (r)
  (dolist (each r)
    (format t "~A = ~A~%" (show (eqexpr-left each)) (show (eqexpr-right each)))
    )
  (format t "~%")
  )


;; 新しいRから生成された等式をEに戻す処理が抜けてる
(defun completion (E &optional (R nil) (limit 150))
  (when (> 0 limit)
    (error 
      (make-condition 'endless-critical-pair-error)))

  (if (null E) R
    (let* ((eqexpr (first E))
           (left  (get-regular% (eqexpr-left eqexpr) r))
           (right (get-regular% (eqexpr-right eqexpr) r))
           (lo (get-order left))
           (ro (get-order right))) 
      (if (term= left right) 
        (completion (cdr e) r (1- limit))
        (cond 
          ;; 順序づけできない場合は失敗にスべきだけど...
          ;; 失敗無し完備化を実装しなければ...
          ((>= lo ro)
           (let ((next-r (regular-rule  (cons (rw-rule left right) r))))
             (completion
               (regular-eq (append (cdr e) (get-critical-pair next-r)))
               next-r
               (1- limit))))
          ((> ro lo)
           (let ((next-r (regular-rule (cons (rw-rule right left) r))))
             (completion
               (regular-eq (append (cdr e) (get-critical-pair next-r)))
               next-r
               (1- limit))))
          (t 
           (error 
             (make-condition 'same-complexity-error))))))))


(defun completion-toplevel (e)
  (let ((rule (completion e)))
    (dolist (each rule)
      (format t "~%~A -> ~A"
              (show (rw-rule-left each))
              (show (rw-rule-right each)))))
  (format t "~%"))



(defun prove-eqexpr% (eqexpr ruleset)
  (let ((left (get-regular%
         (eqexpr-left eqexpr)
         ruleset))
        (right
          (get-regular% 
         (eqexpr-right eqexpr)
         ruleset)))

    (deb-trace-kbcompl eqexpr left right ruleset)

    (fuzzy-term= left right)))



(defun prove-eqexpr (eqexpr axioms)
  (prove-eqexpr% eqexpr (completion axioms)))






@export
(defun special-equality? (premises-clause-form conseq-clause-form) 

  (when 
    (and 
      (every 
        (lambda (clause)
          (let ((literals (clause-%literals clause)))
            (and 
              (= 1 (length literals))
              (equal-literal? (car literals))
              (not (%literal-negation (car literals))))))
        premises-clause-form)
      (let ((literals (clause-%literals (car conseq-clause-form))))
        (and 
          (= 1 (length literals))
          (equal-literal? (car literals))
          (%literal-negation (car literals)))))


    (handler-case
      (values 
        t
        (completion 
          (mapcar 
            (lambda (clause)
              (let ((terms (%literal-terms (car (clause-%literals clause)))))
                (eqexpr 
                  (first terms) 
                  (second terms))))
            premises-clause-form))) 
      (same-complexity-error (e)
        (values nil nil))
      (set-of-equation-required-error (e)
        (values nil nil))
      (endless-critical-pair-error (e)
        (values nil nil))
      (endless-regularization-error (e)
        (values nil nil)))))


@export
(defun prove-special-equality (conseq-clause-form rule)  
  (prove-eqexpr% 
    (apply #'eqexpr 
             (%literal-terms (car (clause-%literals (car conseq-clause-form)))))
    rule))



