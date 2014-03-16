

(ns:defns flexpr.system.constant
	(:use :cl))

;; defconstant でやると再定義エラーが起きてしまう -> asdファイルの書き方おかしいのかな?

@export
(defvar +IMPL+ 'impl)

@export
(defvar +NEG+  'neg)

@export
(defvar +AND+  'and)

@export
(defvar +OR+   'or)

@export
(defvar +EQ+   'eq)

@export
(defvar +FORALL+ 'forall)

@export
(defvar +EXISTS+ 'exists)


@export
(defvar +NEG-STR+ "~")

@export
(defvar +FORALL-STR+ "A")

@export
(defvar +EXISTS-STR+ "E")

@export
(defvar +DELIMITER+ ".")

@export
(defvar +ARG-DELIMITER+ ",")

@export
(defvar +OPERATOR+
			 `((,+IMPL+ ">" 3)
			   (,+NEG+  ,+NEG-STR+ 1)
			   (,+AND+  "&" 2)
			   (,+OR+   "V" 2)
			   (,+EQ+   "-" 3)))
@export
(defvar +PAREN-START+ "(")

@export
(defvar +PAREN-END+ ")")

@export
(defvar +QUANTS+ 
			 `((,+FORALL+ ,+FORALL-STR+ "∀")
			   (,+EXISTS+ ,+EXISTS-STR+ "∃")))

@export
(defvar +RENAME-PREFIX+ "v_")

@export
(defvar +SKOLEM_CONST+ "SC_")

@export
(defvar +SKOLEM_FUNC+ "SF_")

@export
(defvar +DEPTH+ 50)
