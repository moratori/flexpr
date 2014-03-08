

(ns:defns flexpr.interface.constant
	(:use :cl))


@export
(defvar +CHAR-BUG+ '(#\Space #\Tab #\Return #\Newline #\Linefeed))

@export
(defvar +LEXPR-DELIMITER+ #\;)

@export
(defvar +COMMENT-START+ #\#)

@export
(defvar +SPECIAL-BLOCK+ #\%)

@export
(defvar +BLOCK-START+ #\{)

@export
(defvar +BLOCK-END+ #\})

@export
(define-condition illformed-definition-error (error) ())
