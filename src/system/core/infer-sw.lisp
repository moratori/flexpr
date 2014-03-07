

(ns:defns flexpr.system.infer.wrap
	(:use :cl
		  :flexpr.system.constant
		  :flexpr.system.struct)
	(:import-from :flexpr.system.infer.general
				  :resolution-gen)
	(:import-from :flexpr.system.infer.sld
				  :resolution-sld))




;; t -> sld
;; nil -> gen
(defun which? (premises conseq)
  nil
  )


@export
(defun resolution (premises conseq &optional (depth +DEPTH+))
  (if (which? premises conseq)
	(error "undefined!!")
	(resolution-gen premises conseq depth)))
