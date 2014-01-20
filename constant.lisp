

(defconstant +IMPL+ 'impl)
(defconstant +NEG+  'neg)
(defconstant +AND+  'and)
(defconstant +OR+   'or)
(defconstant +EQ+   'eq)

(defconstant +FORALL+ 'forall)
(defconstant +EXISTS+ 'exists)

(defconstant +OPERATOR+
			 `((,+IMPL+ ">" 3)
			   (,+NEG+  "~" 1)
			   (,+AND+  "&" 2)
			   (,+OR+   "V" 2)
			   (,+EQ+   "-" 3)))


(defconstant +QUANTS+ 
			 `((,+FORALL+ "∀")
			   (,+EXISTS+ "∃")))


