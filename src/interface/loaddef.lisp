
(ns:defns flexpr.interface.load
  (:use :cl
		:flexpr.interface.constant)
  (:import-from :flexpr.system.parser
				:string->lexpr))


#|
	<DEFINITION> ::= <CLASS-NAME> { <LOGICAL-EXPR>* <DEFINITION>* }



	ex)


	#
    # this is comment
    #

	%Animal {Ax.mortal(x);     Ex.foo(x);
 		Mamal{
			Ax.taisei(x);
			Human{
				Ax.greed(x);
				Ax.ugly(x);
			}
		}

	}


|#



;; コメント取り除いて行で読み込む

@export
(defun primitive-load-file (path)
  (with-open-file (in path :direction :input :if-does-not-exist :error)
	(loop for line = (read-line in nil) 
		  while line
		  for fix1 = (string-trim +CHAR-BUG+ line)
		  for fix2 = (string-trim +CHAR-BUG+ 
								 (subseq  fix1 0 (position +COMMENT-START+ fix1 :test #'char=))) 
		  if (string/= "" fix2)
		    collect fix2)))

;; 全部の行をくっつけるだけ
(defun load-def-file (path)
  (apply #'concatenate 'string 
		 (primitive-load-file path)))


(defun get-contents (str)
  (let ((start (position +BLOCK-START+ str :test #'char=)))
	(values
	  (string-trim 
	    +CHAR-BUG+ 
		(subseq str 0 start))
	  (string-trim 
		+CHAR-BUG+
		(subseq str (1+ start) (1- (length str)))))))


#|
	((%animal
	  ...
	  ...
	  ...
	  )
	  (mamal 
		...
		...
		...
		)
	  (human
		...
		...
		...
		))

	 (animal (mamal (human)) (fish) )
|#


(defun split (str delimiter)
  (assert (typep delimiter 'character))
  (let ((pos (position delimiter str :test #'char=)))
	(if (null pos) (list str)
	  (cons (subseq str 0 pos) (split (subseq str (1+ pos)) delimiter)))))


(defun get-lexprs (str)
  (print str)
  (assert (string/= str ""))
  (let ((nextblock (position +BLOCK-START+ str :test #'char=)))
	(if (null nextblock)
	  (mapcar #'string->lexpr (split str +LEXPR-DELIMITER+))
	  (labels 
		((main (str pos result count)
			(let ((p (position +LEXPR-DELIMITER+ str :test #'char=)))
			  (if (or (null p) (> (+ pos p) nextblock))
				(values result count)
				(main 
				  (subseq str (1+ p))
				  (+ p pos)
				  (cons (string-trim +CHAR-BUG+ #0=(print (subseq str 0 p))) result)
				  (+ count (length #0#) 1))))))
		(main str 0 nil 0)))))

#|
(defun parse-definition (path)
  (let ((target (load-def-file path)))
	(labels 
	  ((main (str axiomatic-sys dependent)
		(if (string= str "") 
		  (values axiomatic-sys dependent)
		  (multiple-value-bind (name con) (get-contents str)
			
			)
		  )
		))
	  (main target nil nil))))
|#
