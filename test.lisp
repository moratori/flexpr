

(ql:quickload :flexpr)




(defun ft (&rest arg)
  (apply #'struct:fterm arg))

(defun vt (&rest arg)
  (apply #'struct:vterm arg))


(print 
  (unifier:mgu 
	(ft 'f  (vt 'x nil) (ft 'C ) (ft 'g (vt 'z nil)))
	(ft 'f  (ft 'h (vt 'y nil) (vt 'w nil)) (vt 'y nil) (vt 'w nil))
	)
  )
