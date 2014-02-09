


(defpackage flexpr-asd
	(:use :cl :asdf))

(in-package flexpr-asd)


(defsystem flexpr
	:depends-on (:optima :namespace)
	:serial t
	:components 
		((:file "constant")
		 (:file "struct")
		 (:file "util")
		 (:file "dump")
		 (:file "parser")
		 (:file "unifier")
		 (:file "infer")
		 (:file "formalize")))
