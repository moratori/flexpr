

(defpackage constant
  (:use :cl)
  (:export 
	:+IMPL+
	:+NEG+
	:+AND+
	:+OR+
	:+EQ+
	
	:+FORALL+
	:+EXISTS+
	:+DELIMITER+
	:+NEG-STR+
	:+FORALL-STR+
	:+EXISTS-STR+
	:+OPERATOR+
	:+PAREN-START+
	:+PAREN-END+
	:+QUANTS+
	))
(in-package constant)



(defconstant +IMPL+ 'impl)
(defconstant +NEG+  'neg)
(defconstant +AND+  'and)
(defconstant +OR+   'or)
(defconstant +EQ+   'eq)

(defconstant +FORALL+ 'forall)
(defconstant +EXISTS+ 'exists)


(defconstant +NEG-STR+ "~")
(defconstant +FORALL-STR+ "A")
(defconstant +EXISTS-STR+ "E")


(defconstant +DELIMITER+ ".")

(defconstant +OPERATOR+
			 `((,+IMPL+ ">" 3)
			   (,+NEG+  ,+NEG-STR+ 1)
			   (,+AND+  "&" 2)
			   (,+OR+   "V" 2)
			   (,+EQ+   "-" 3)))

(defconstant +PAREN-START+ "(")
(defconstant +PAREN-END+ ")")

(defconstant +QUANTS+ 
			 `((,+FORALL+ ,+FORALL-STR+ "∀")
			   (,+EXISTS+ ,+EXISTS-STR+ "∃")))







