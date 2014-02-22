


(defpackage flexpr-asd
	(:use :cl :asdf))

(in-package flexpr-asd)


(defsystem flexpr
	:depends-on (:optima :namespace)
	:serial t
	:components 
		((:file "error")
		 (:file "constant")
		 (:file "struct")
		 (:file "util")
		 (:file "dump")
		 (:file "parser")
		 (:file "unifier")
		 (:file "formalize-opr")
		 (:file "formalize-quant")
		 (:file "formalize-mat")
		 (:file "formalize")
		 (:file "infer")))
