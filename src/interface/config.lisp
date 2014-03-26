
(ns:defns flexpr.interface.config
		  (:use :cl
				:flexpr.system.constant
				:flexpr.interface.constant)
		  (:import-from :flexpr.interface.load
						:primitive-load-file
						:split))

(defvar +FILENAME+ ".flexprc")

(defvar +REFER+ 
  (list 
	 (cons "VARIABLE_PREFIX"
		   (lambda (x)
			 (setf +RENAME-PREFIX+ x)))
	 (cons "SKOLEM_CONSTANT_PREFIX"  
		   (lambda (x)
			 (setf +SKOLEM_CONST+ x)))
	 (cons "SKOLEM_FUNCTION_PREFIX"
		   (lambda (x)
			 (setf +SKOLEM_FUNC+ x)))
	 (cons "DEPTH_OF_SEARCH"
		   (lambda (x)
			 (setf +DEPTH+ (parse-integer x))))
	 (cons "NOREPL" 
		   (lambda (x)
			 (setf +SILENT+ (when (string= x "True") t))))
	 (cons "UGLY_PRINTING"
		   (lambda (x)
			 (setf +UGLY-PRINTING+ (when (string= x "True") t))))))

(defvar +DEFAULT-CONFIG+
  (format nil "~A~%~A~%~A~%~A~%~A~%~A~%"

   "VARIABLE_PREFIX = v_"
   "SKOLEM_CONSTANT_PREFIX = SC_"
   "SKOLEM_FUNCTION_PREFIX = SF_"
   "DEPTH_OF_SEARCH = 50"
   "NOREPL = False"
   "UGLY_PRINTING = True"

   ))


(defun write-default ()
  (with-open-file (out +FILENAME+ :direction :output :if-exists :supersede)
	(format out +DEFAULT-CONFIG+)))

@export
(defun loadconfig ()
  (handler-case 
	(dolist (each (primitive-load-file +FILENAME+))
	  (destructuring-bind (name val) (split each #\=)
		(let* ((fn (string-trim +CHAR-BUG+ name))
			   (fv (string-trim +CHAR-BUG+ val))
			   (ins (cdr (assoc fn +REFER+ :test #'string=))))
		  (when (null ins)
			(format t "!!! WARNING !!!~%!!! unrecognized field: ~A !!!~%!!! skipped it !!!~%~%" fn))
		  (funcall ins fv))))	
	(file-error (c)
		(declare (ignore c))
		(write-default))))

