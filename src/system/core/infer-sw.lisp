

(ns:defns flexpr.system.infer.wrap
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
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


;; goal節であるか?
;; 全て否定のリテラル. goal clause <- horn-clause
(defun goal-clause? (conseq)
  (and 
	(single? conseq)
	;; 上２つで １つの節であるかをしらべる
	(every 
	  (lambda (x)
		(%literal-negation x))
	  (clause-%literals (car conseq)))))

;; t -> snl
;; nil -> gen
(defun which? (premises-clause-form conseq-clause-form)
  (if (and 
	  (every (lambda (x) 
			   (or (fact-clause? x)
				   (rule-clause? x)))
			 premises-clause-form)
	  (goal-clause? conseq-clause-form))
	(cons #'resolution-snl "SNL")
	(cons #'resolution-gen "GEN")))



@export
(defun resolution (premises conseq &key (depth +DEPTH+) (output nil))
  (multiple-value-bind 
	(premises-clause-form conseq-clause-form exist-terms)
	(preproc premises conseq)

	(destructuring-bind (func . id) 
	  (which? premises-clause-form conseq-clause-form)
	  (append 
		(funcall 
		  func
		  premises-clause-form 
		  conseq-clause-form
		  exist-terms
		  :depth depth 
		  :output output)
		(list id)))))



