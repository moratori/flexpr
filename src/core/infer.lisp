

(ns:defns flexpr.infer
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :flexpr.unifier
				  :mgu)
	(:import-from :flexpr.formalize
				  :convert))


;;;  効率的な導出を実装
;;;  導出原理つかうかタブロー使うか
;;;  他の使うか



(defun preproc (premises conseq &optional (quants-form +FORALL+) (mat-form (operator +AND+)))
  (append
	(loop for each in 
		  (mapcar 
			(lambda (x) 
			  (convert x mat-form quants-form)) premises)
		  append each)
	(convert 
	  (normal-lexpr (operator +NEG+) conseq nil)
	  mat-form quants-form)))




