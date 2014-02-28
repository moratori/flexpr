

(ns:defns flexpr.error
	(:use :cl))


(define-condition maximum-depth-exceeded (error)
  ((mde-val   :initarg :mde-val
		      :reader   mde-val-of)
   (mde-where :initarg :mde-where
		      :reader   mde-where-of))
  (:report 
	(lambda (c s)
	  (format s "~%Runtime error occurred.~%Maximum search depth ~A exceeded." (mde-val-of c)))))


@export
(define-condition struct-unmatch-error (error)
  ((sue-val   :initarg :sue-val
		      :reader   sue-val-of)
   (sue-where :initarg :sue-where
		      :reader   sue-where-of))
  (:report
	(lambda (c s)
	  (format s "~%Internal error occurred.~%Structure unmatched any struct pattern~%struct: ~A~%where:  ~A~%"
			  (sue-val-of c)
			  (sue-where-of c)))))


@export
(define-condition initval-required-error (error)
  ((ire-where :initarg :ire-where
			  :reader   ire-where-of))
  (:report 
	(lambda (c s)
	  (format s "~%Internal error occurred. ~%initial value required~%where: ~A~%"
			  (ire-where-of c)))))

@export
(define-condition illformed-error (error)
  ((ie-val   :initarg :ie-val
			 :reader   ie-val-of)
   (ie-where :initarg :ie-where
			 :reader   ie-where-of)
   (ie-mes   :initarg :ie-mes
			 :reader   ie-mes-of))
  (:report 
	(lambda (c s)
	  (format s "~%Runtime error occurred.~%~A~%value: ~A~%where: ~A~%"
			  (ie-mes-of c)
			  (ie-val-of c)
			  (ie-where-of c)))))


@export
(define-condition illformed-parse-error (error)
  ((ipe-val   :initarg :ipe-val
			  :reader   ipe-val-of)
   (ipe-where :initarg :ipe-where
			  :reader   ipe-where-of)
   (ipe-mes   :initarg :ipe-mes
			  :reader   ipe-mes-of))
  (:report 
	(lambda (c s)
	  (format s "~%Runtime error occurred while parsing.~%~A~%value: ~A~%where: ~A~%"
			  (ipe-mes-of c)
			  (ipe-val-of c)
			  (ipe-where-of c)))))

@export
(define-condition illformed-formalize-error (error)
  ((ife-mes   :initarg :ife-mes
			  :reader  ife-mes-of)
   (ife-val   :initarg :ife-val
			  :reader  ife-val-of)
   (ife-where :initarg :ife-where
			  :reader  ife-where-of))
  (:report 
	(lambda (c s)
	  (format s "~%Runtime error occurred while formalizing. ~%~A~%value: ~A~%where: ~A~%"
			  (ife-mes-of c)
			  (ife-val-of c)
			  (ife-where-of c)))))

