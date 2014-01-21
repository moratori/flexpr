

(defconstant +IMPL+ 'impl)
(defconstant +NEG+  'neg)
(defconstant +AND+  'and)
(defconstant +OR+   'or)
(defconstant +EQ+   'eq)

(defconstant +FORALL+ 'forall)
(defconstant +EXISTS+ 'exists)
(defconstant +DELIMITER+ ".")


(defconstant +NEG-STR+ "~")

(defconstant +OPERATOR+
			 `((,+IMPL+ ">" 3)
			   (,+NEG+  ,+NEG-STR+ 1)
			   (,+AND+  "&" 2)
			   (,+OR+   "V" 2)
			   (,+EQ+   "-" 3)))

(defconstant +PAREN-START+ "(")
(defconstant +PAREN-END+ ")")

(defconstant +QUANTS+ 
			 `((,+FORALL+ "A" "∀")
			   (,+EXISTS+ "E" "∃")))







