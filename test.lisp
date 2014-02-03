

(ql:quickload :flexpr)




(defun ft (&rest arg)
  (apply #'struct:fterm arg))

(defun vt (&rest arg)
  (apply #'struct:vterm arg))



(print 
  (unifier:mgu 
	(ft 'f (vt 'x nil) (ft 'g (vt 'x nil)))
	(ft 'f (vt 'C t)   (vt 'y nil) )
	))
