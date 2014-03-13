

(ns:defns flexpr.interface.repl
	(:use :cl
		  :flexpr.interface.constant
		  :flexpr.system.error)
	(:import-from 
	  :flexpr.interface.load
	  :primitive-load-file)
	(:import-from 
	  :flexpr.system.parser
	  :string->lexpr)
	(:import-from 
	  :flexpr.system.infer.wrap
	  :resolution)
	(:import-from
	  :flexpr.system.dump
	  :lexpr->string
	  :term->string
	  ))


(defmacro defcompo (fname arg &rest body)
  `(defun ,fname ,(append arg '(&rest dummy)) ,@body))

(defvar +CSTART+ #\:)
(defvar *axioms* (list (list "nil" nil)))
(defvar *current* "nil")

(define-condition caught-error (error) ())




(defcompo help ()
  (format t "Command Help~%")
  (format t "~2t :load <Filename>               load a definition file~%")
  (format t "~2t :save <axiomatic system name>  save current system~%")
  (format t "~2t :desc <axiomatic system name>  describe axiomatic system~%")
  (format t "~2t :set  <New axiomatic system>   change current axiomatic system~%")
  (format t "~2t :add  <Formula>                add formula to current axiomatic system~%")
  (format t "~2t :list  enumerate the axiomatic system that are currently defined~%")
  (format t "~2t :help  show this help~%")
  (format t "~2t :exit  exit from REPL~%")
  (format t "~2t :quit  alias of :exit~%~%"))


(defun hook (proc)
  (handler-case 
	(funcall proc)
	(struct-unmatch-error (c)
		(format t "~%Internal error: struct unmatch error occurred~%")
		(format t "struct: ~A~%where: ~A~%~%" 
				(sue-val-of c) (sue-where-of c))
		(error (make-condition 'caught-error)))
	(initval-required-error (c)
		(format t "~%Internal error: initval required error occurred~%")
		(format t "where: ~A~%~%" (ire-where-of c))
		(error (make-condition 'caught-error)))
	(illformed-error (c)
		(format t "~%Runtime error: illformed error occurred~%")
		(format t "mes: ~A~%val: ~A~%where: ~A~%~%"
				(ie-mes-of c) (ie-val-of c) (ie-where-of c))
		(error (make-condition 'caught-error)))
	(illformed-parse-error (c)
		(format t "~%Runtime error: illformed parse error occurred~%")
		(format t "mes: ~A~%val: ~A~%where: ~A~%~%"
				(ipe-mes-of c) (ipe-val-of c) (ipe-where-of c))
		(error (make-condition 'caught-error)))
	(illformed-formalize-error (c)
		(format t "~%Runtime error: illformed formalize error occurred~%")	
		(format t "mes: ~A~%val: ~A~%where: ~A~%~%"
				(ife-mes-of c) (ife-val-of c) (ife-where-of c))
		(error (make-condition 'caught-error)))))


(defun unexpected (c)
  (print c)	   
  (format t "~%Runtime error: unexpected error occured~%"))


(defun credit ()
  (format t "Theorem Prover 1.0~%")
  (help)
  (force-output t))

(defcompo quit ()
  (format t "bye!~%") t)

(defun current ()
  (string-upcase *current*))

(defun prompt () 
  (format t "(~A)>>> " (current))
  (force-output *standard-output*))

(defcompo listup ()
  (if (null *axioms*)
	(format t "axiomatic system doesn't exist~%")
	(dolist (each *axioms*)
	  (format t "~A~%" (car each)))))

(defun existax? (axname)
  (find-if 
	(lambda (x)
	  (string= (car x) axname)) *axioms*))

(defun setax (axname)
  (if (null (existax? axname))
	(format t "undefined axiomatic system: ~A~%" axname)
	(progn (setf *current* axname) nil)))



(defun loadax (path)
  (handler-case 
	(hook 
	  (lambda ()
		(let ((raw (primitive-load-file path))
			  (name (pathname-name path)))
		  (push 
			(list name (mapcar #'string->lexpr raw))
			*axioms*)
		  (format t "~A load ok~%" name)
		  (setax name))))
	(caught-error (c)
		(declare (ignore c)))
	(stream-error (c)
	  (declare (ignore c))
	  (format t "%Runtime error: file error occurred~%"))
	(file-error (c)
	  (declare (ignore c))
	  (format t "~%Runtime error: file error occurred~%"))
	(error (c)
	  (unexpected c))))

(defun desc (axname)
  (let ((r (existax? (if (string= "" axname) *current* axname))))
	(if (null r)
		(format t "undefined axiomatic system: ~A~%" axname)
		(destructuring-bind (name obj) r
		  (format t "~A is consist of: ~%" name)
		  (loop for oe in obj
				do 
				(format t "~A~%" (lexpr->string oe)))
		  (format t "~%")))))



(defun adddef (line)
  (if (string= line "")
	(format t "formula required~%")
    (if (string= *current* "nil")
	   (format t "adding formula to this axiomatic system is not allowed~%")
	   (destructuring-bind (name obj) (existax? *current*)
	     (handler-case
		   (let ((new (hook (lambda () (string->lexpr line)))))
		   (setf *axioms*
			   (cons 
			     (list name (cons new obj))
			     (remove-if 
				   (lambda (x)
				   (string= name (car x))) *axioms*)))nil)
		   (caught-error (c)
		     (declare (ignore c)))
		   (error (c)
		     (unexpected c)))))))


(defun save (line)
  (let* ((target (if (string= line "") *current* line))
		(r (existax? target)))
	(if (null r)
	  (format t "undefined axiomatic system: ~A~%" target)
	  (handler-case 
		(with-open-file (out target :direction :output :if-exists :supersede)
		  (destructuring-bind (name obj) r
			(dolist (each obj)
			  (format out "~A~%" (lexpr->string each)))))
		(file-error (c)
			(declare (ignore c))
			(format t "file error occurred~%"))))))


(defun execute-resolution (line)
  (destructuring-bind (name obj) (existax? *current*)
	(let ((start (get-universal-time)))
	 (handler-case 
	  (destructuring-bind (status exist spec) 
		(hook (lambda () (resolution obj (string->lexpr line))))
		(declare (ignore exist))
		(format t "~A is ~A under the ~A~%" 
				line (if status "provable" "not provable")
				*current*)
		(unless (null spec)
		  (format t "specific term: ")
		  (dolist (each spec)
			(format t "~A " (term->string each)))
		  (format t "~%")))
	  (caught-error (c)
	    (declare (ignore c)))
	  (undeterminable-error (c)
		(declare (ignore c))
		(format t "undeterminable: ~A~%" line))
	  (error (c)
		 (unexpected c))) 
	  (format t "evaluation took ~A sec~%" (- (get-universal-time) start)))))



(defvar *case*
  (list 
	 (cons ":save"  #'save)
	 (cons ":add"   #'adddef)
	 (cons ":load"  #'loadax)
	 (cons ":set"   #'setax)
	 (cons ":list"  #'listup)
	 (cons ":desc"  #'desc)
	 (cons ":help"  #'help)
	 (cons ":quit"  #'quit)
	 (cons ":exit"  #'quit)))


(defun parse-line (s)
  (coerce 
	(loop 
	  for i from 0 below (length s)
	  for char = (char s i)
	  while (char/= char #\Space)
	  collect char)
	'string))


(defun command-proc (line)
  (let*  ((cmd (parse-line line))
		  (arg (subseq line (length cmd)))
		  (fun (cdr (assoc cmd *case* :test #'string=))))
	(if (null fun)
	  (format t "Undefined command: ~A~%" cmd)
	  (funcall fun (string-trim +CHAR-BUG+ arg)))))


(defun main ()
  (credit)
  (prompt)
  
  (loop 
	:named exit
	:do
	(let ((line (read-line t nil nil)))
	  (cond 
		((null line) 
		 (quit) (return-from exit))
		((string= "" line))
		((char= (char line 0) +CSTART+)
		 (when (command-proc line) 
		   (return-from exit)))
		(t 
		  (execute-resolution line))))
	(force-output *standard-output*)
	(prompt)))


