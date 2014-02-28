


(defpackage flexpr.asd
	(:use :cl :asdf))

(in-package flexpr.asd)


(defsystem flexpr
	:depends-on (:optima :namespace :iterate)
	:serial t
	:version "1.0"
	:author "moratori"
	:components 
		((:module "src"
		  :serial t
		  :components 
				((:module "constant"
				  :serial t
				  :components 
					((:file "error")
					 (:file "constant")))
				 (:module "base"
				  :serial t
				  :components 
					((:file "struct")
					 (:file "util")))
				 (:module "io"
				  :serial t
				  :components 
					((:file "dump")
					 (:file "parser"))) 
				 (:module "formalize"
				  :serial t
				  :components 
					((:file "formalize-opr")
		 			 (:file "formalize-quant")
		 			 (:file "formalize-mat")
		 			 (:file "formalize")))
				 (:module "core"
				  :serial t
				  :components 
					((:file "unifier")
					 (:file "infer-gen")
					 (:file "infer-sld")))))))




