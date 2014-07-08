

(ns:defns flexpr.system.infer.wrap
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
  (:import-from :flexpr.system.infer.equal
   :special-equality?
   :prove-special-equality
   )
  (:import-from :flexpr.system.util
   :equal-included-clause-form? )
	(:import-from :flexpr.system.infer.general
				  :resolution-gen)
	(:import-from :flexpr.system.infer.snl
				  :resolution-snl)
	(:import-from :flexpr.system.formalize
				  :formalize
					:remove-alphabet-equal
				  :convert))



;; 
;; (values {C1,C2,C3...} {C1,C2,C3...})
;; 上の合併集合から空節が求まればおｋ
(defun preproc (premises conseq &optional (quants-form +FORALL+) (mat-form (operator +AND+))) 
  (multiple-value-bind 
	(lexpr exist-terms) 
	(convert (normal-lexpr (operator +NEG+) conseq nil) mat-form quants-form)
	(values 
	(remove-alphabet-equal
		(loop 
			for each in 
			(mapcar 
				(lambda (x) 
					(convert x mat-form quants-form)) premises)
			append each))	
	  lexpr
	  exist-terms)))


(defun single? (l)
  (and (not (null l))
	   (null (cdr l))))

;; 正のリテラルが1つ
(defun rule-clause? (clause)
  (= 1
	 (count-if 
	   (lambda (x)
		 (not (%literal-negation x)))
	   (clause-%literals clause))))

(defun fact-clause? (clause)
  (and
	(single? (clause-%literals clause))
	(not (%literal-negation 
		   (car (clause-%literals clause))))))


(defun goal-clause? (clause)
	(every 
	  (lambda (x)
		(%literal-negation x))
	  (clause-%literals clause)))

;; goal節であるか?
;; 全て否定のリテラル. goal clause <- horn-clause
(defun goal-clause-form? (conseq)
  (and 
		(single? conseq)
	;; 上２つで １つの節であるかをしらべる
		(goal-clause? (car conseq))))


;; horn clause であるか否かを判定する
(defun horn-clause-form? (clause-form)
	(every 
		(lambda (x)
			(or 
				(fact-clause? x)
				(goal-clause? x)
				(rule-clause? x)))
		clause-form))


;; t -> snl
;; nil -> gen
(defun which-resolution? (premises-clause-form conseq-clause-form)
	(let ((check-rule 
					(every (lambda (x) 
								 (or (fact-clause? x)
										 (rule-clause? x)))
							 premises-clause-form)))
		(cond 
      ((or (equal-included-clause-form? premises-clause-form)
           (equal-included-clause-form? conseq-clause-form))
       (values (cons #'resolution-gen "Linear (for equality)") nil))
			((and check-rule 
						(goal-clause-form? conseq-clause-form))
			 (values (cons #'resolution-snl "SNL (for horn clause)") nil))
			((and check-rule 
						(some #'goal-clause? conseq-clause-form)
						(every 
							(lambda (x) (or (rule-clause? x) (fact-clause? x)))
							(remove-if #'goal-clause? conseq-clause-form)))
			 (values (cons #'resolution-snl "SNL (for horn clause)") t))
			(t 
				(values (cons #'resolution-gen "Linear (default)") nil)))))



(defun make-equal-axiom ()
  (let* ((eqv (gensym "EV"))
         (var (vterm eqv nil)))

  
  
    (clause 
    (list (%literal nil +EQUAL+ (list var var) 0))
    0
    )
  
  
  
  )
  )






@export
(defun resolution (premises conseq &key (depth +DEPTH+) (output nil))

  
  (multiple-value-bind 
	(premises-clause-form conseq-clause-form exist-terms)
	(preproc premises conseq)

  ;; ここの special-equality? で 完備で甘美な項書換え系での
  ;; 等式証明が可能が判定する
  ;; which-resolution? で全て分類できないのが悔しい!

  (let ((rule (special-equality? premises-clause-form conseq-clause-form)))
    (if (and t rule)
      
      (list 
        (prove-special-equality conseq-clause-form rule)
        nil 
        nil
        "Term Rewriting (for special equality)")
      
      (let ((premises-clause-form 
          ;; 等号の式集合だったら等号公理をぶち込む
          ;; このおかげで resolution-gen のなかでの equal-contap?は本質的には必要なくなるけど
          ;; 効率化のためにとっておくべき(等号の矛盾がワンステップ早く終わる)
          (if (or (equal-included-clause-form? premises-clause-form)
                  (equal-included-clause-form? conseq-clause-form)) 
            (append premises-clause-form (list (make-equal-axiom))) 
            premises-clause-form)))
    
  ;; フラグが t だったら SNL 導出のために変形が必要
	;; ユーザのクエリを 
    (multiple-value-bind (pair flag)
			(which-resolution? premises-clause-form conseq-clause-form)	
			(destructuring-bind (func . id) pair
				(append 
					(if flag 
						(let ((for-snl 
										(append 
											premises-clause-form 
											(remove-if #'goal-clause? conseq-clause-form))))
							(some 
								(lambda (goal-clause)
									(funcall 
										func
										for-snl
										(list goal-clause)
										exist-terms
										:depth depth 
										:output output))
								(remove-if-not #'goal-clause? conseq-clause-form)))
						(funcall 
							func
							premises-clause-form 
							conseq-clause-form
							exist-terms
							:depth depth 
							:output output))
					(list id)))))
      )
    )
  
  ))


