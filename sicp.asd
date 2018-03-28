(in-package :asdf-user)

(defsystem #:sicp
  :description "SICP - exercises and notes"
  :author "k-stz"
  :license "MIT"
  ;; :depends-on (:picture-language-package)
  :serial t
  :components
  ((:file "package")
   (:module "1-chapter/"
	    :components ((:file "primality")
			 (:file "newton")))
   (:module "2-chapter/"
	    :components ((:file "line-segments")
			 (:file "closure")
			 (:file "sequences-as-conventional-interfaces")
			 ;(:file "the-picture-language")
			 (:file "symbolic-data")
			 (:file "representing-sets")
			 (:file "huffman-encoding-tree")
			 (:file "2.4-multiple-representation-for-abstract-data")))))
