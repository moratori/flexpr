


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
		 (:file "formalize")
		 (:file "infer")))
