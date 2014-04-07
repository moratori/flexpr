

(ns:defns flexpr.system.infer.wrap
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
	(:import-from :flexpr.system.infer.general
				  :resolution-gen)
	(:import-from :flexpr.system.infer.sld
				  :resolution-sld)
	(:import-from :flexpr.system.formalize
				  :formalize
				  :convert))



;; 
;; (values {C1,C2,C3...} {C1,C2,C3...})
;; 上の合併集合から空節が求まればおｋ
(defun preproc (premises conseq &optional (quants-form +FORALL+) (mat-form (operator +AND+))) 
  (multiple-value-bind 
	(lexpr exist-terms) 
	(convert (normal-lexpr (operator +NEG+) conseq nil) mat-form quants-form )
	(values 
	  (loop for each in 
	    (mapcar 
		  (lambda (x) 
			(convert x mat-form quants-form)) premises)
			append each)
	  lexpr
	  exist-terms)))



;; t -> sld
;; nil -> gen
(defun which? (premises-clause-form conseq-clause-form)
  nil ;(format t "--- PREMISES ---~%~A~%~%--- CONSEQ ---~%~A~%~%" premises conseq)
  )


@export
(defun resolution (premises conseq &key (depth +DEPTH+) (output nil))
  (multiple-value-bind 
	(premises-clause-form conseq-clause-form exist-terms)
	(preproc premises conseq)

	(funcall 
	  (if (which? premises-clause-form conseq-clause-form)
		  #'resolution-sld 
		  #'resolution-gen)
		premises-clause-form 
		conseq-clause-form
		exist-terms
		:depth depth 
		:output output)))

