

(ql:quickload :flexpr)




(print 
  (parser::string->quantsp "Ax Ay  ~~Ez")
  )

(print 
 	(parser::string->operator "&")
  )


(print 
  (parser::weak-point (parser::tokenize-lexpr "P(x) > Q(x)"))
  )


(print 
  (parser::weak-point (parser::tokenize-lexpr "P(x) - Q(x) > S(x)"))
  )

(print (parser::weak-point (parser::tokenize-lexpr "P(x)")))


(print (parser::tokenize-lexpr "(((((((P))))))) > Q(x)"))


(parser::split-func-format "func        (a,b,c,g(a,h(b)))         ")


(print (parser::tokenize-term "x     , y ,      z ,   f(x   ,   g(x , z))  ,   h(y,x)"))


(print (parser::string->term "f(x,g(x,Y))"))


(print (parser::string->atomic " P   (  x , f ( x ) )  "))
