


(defpackage flexpr.asd
	(:use :cl :asdf))

(in-package flexpr.asd)


(defsystem flexpr
	:depends-on (:optima :namespace)
	:serial t
	:version "1.0"
	:author "moratori"
	:components 
		((:module "src"
		  :serial t
		  :components 
				((:file "error")
				 (:file "constant")
                 (:file "struct")
                 (:file "util")
                 (:file "dump")
				 (:file "parser")
		 		 (:file "unifier")
				 (:module "formalize"
				  :serial t
				  :components 
					((:file "formalize-opr")
		 			 (:file "formalize-quant")
		 			 (:file "formalize-mat")
		 			 (:file "formalize")))
				 (:file "infer")))))

