(in-package :asdf-user)

(defsystem #:sicp
  :description "SICP - exercises and notes"
  :author "k-stz"
  :license "MIT"
  :serial t
  :components
  ((:module "1-chapter/"
	    :components ((:file "newton")))
   (:module "2-chapter/"
	    :components ((:file "line-segment")))))
