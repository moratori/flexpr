

(ns:defns constant
	(:use :cl)
	(:nicknames :const))

@export
(defconstant +IMPL+ 'impl)

@export
(defconstant +NEG+  'neg)

@export
(defconstant +AND+  'and)

@export
(defconstant +OR+   'or)

@export
(defconstant +EQ+   'eq)

@export
(defconstant +FORALL+ 'forall)

@export
(defconstant +EXISTS+ 'exists)


@export
(defconstant +NEG-STR+ "~")

@export
(defconstant +FORALL-STR+ "A")

@export
(defconstant +EXISTS-STR+ "E")

@export
(defconstant +DELIMITER+ ".")

@export
(defconstant +ARG-DELIMITER+ ",")

@export
(defconstant +OPERATOR+
			 `((,+IMPL+ ">" 3)
			   (,+NEG+  ,+NEG-STR+ 1)
			   (,+AND+  "&" 2)
			   (,+OR+   "V" 2)
			   (,+EQ+   "-" 3)))
@export
(defconstant +PAREN-START+ "(")

@export
(defconstant +PAREN-END+ ")")

@export
(defconstant +QUANTS+ 
			 `((,+FORALL+ ,+FORALL-STR+ "∀")
			   (,+EXISTS+ ,+EXISTS-STR+ "∃")))







