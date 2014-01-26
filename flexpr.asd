


(defpackage flexpr-asd
	(:use :cl :asdf))

(in-package flexpr-asd)


(defsystem flexpr
	:depends-on (:optima)
	:components 
		((:file "constant")
		 (:file "struct")
		 (:file "util")
		 (:file "dump")
		 (:file "parser"))
	:serial t)
