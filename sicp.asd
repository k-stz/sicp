(in-package :asdf-user)

(defsystem #:sicp
  :description "SICP - exercises and notes"
  :author "k-stz"
  :license "MIT"
  :serial t
  :components
  ((:module "1-chapter/"
	    :components ((:file "primality")
			 (:file "newton")))
   (:module "2-chapter/"
	    :components ((:file "line-segments")
			 (:file "sequences-as-conventional-interfaces.lisp")))))
