

(ql:quickload :flexpr)


(defun parse (str)
  (flexpr.parser::string->lexpr str))

(defun dump (lexpr)
  (flexpr.dump::lexpr->string lexpr))

(defun formal (lexpr)
  (flexpr.formalize::formalize lexpr))


(defun test (str)
  (dump (formal (parse str))))





