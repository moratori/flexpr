

(ql:quickload :flexpr)




(print 
  (parser::string->quantsp "Ax Ay  ~~Ez")
  )

(print 
 	(parser::string->operator "&")
  )


(print 
  (parser::weak-point (parser::tokenize "P(x) > Q(x)"))
  )


(print 
  (parser::weak-point (parser::tokenize "P(x) - Q(x) > S(x)"))
  )

(print (parser::weak-point (parser::tokenize "P(x)")))
