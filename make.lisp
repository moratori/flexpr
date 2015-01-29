(ql:quickload :flexpr)

#+sbcl (sb-ext:save-lisp-and-die 
  "binary/linux/REPL-linux-x64"
  :toplevel #'flexpr.interface.repl::main
  :executable t
  :purify t
  :compression 9)
