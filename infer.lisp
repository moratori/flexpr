

(ns:defns flexpr.infer
	(:use :cl
		  :flexpr.constant
		  :flexpr.struct)
	(:import-from :flexpr.unifier
				  :mgu))


;;;  効率的な導出を実装
;;;  導出原理つかうかタブロー使うか
;;;  他の使うか


