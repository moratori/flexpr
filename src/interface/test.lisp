
(ql:quickload :flexpr)


(multiple-value-bind (a b)
  (flexpr.interface.load::get-contents 
	(flexpr.interface.load::load-def-file "definition"))
  (multiple-value-bind (c d)
	(flexpr.interface.load::get-lexprs b)
	(print c)
	(print d)
	)
  )


