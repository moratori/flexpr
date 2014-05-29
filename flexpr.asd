


(defpackage flexpr.asd
	(:use :cl :asdf))

(in-package flexpr.asd)


(defsystem flexpr
	:depends-on (
				 :optima 
				 :namespace 
				 )
	:serial t
	:version "1.0"
	:author "moratori"
	:components 
		((:module "src"
		  :serial t
		  :components 
		  	((:module "system"
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
					 (:file "util")
           (:file "unifier")
					 (:file "paramod-unifier")))
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
					((:file "infer-gen")
					 (:file "infer-snl")
					 (:file "infer-sw")))))
			 (:module "interface"
			  :serial t
			  :components 
				((:file "constant")
				 (:file "loaddef")
				 (:file "config")
				 (:file "repl")))))))


