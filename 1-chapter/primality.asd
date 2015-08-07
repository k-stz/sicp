(defpackage #:primality-asd
  (:use :cl :asdf))

(in-package :primality-asd)


;;;To build and run the system:
; (asdf:operate 'asdf:load-op 'primality)
; it will search for this file first and then search the directories in:
; asdf:*central-registry*

(defsystem primality ;minimal form, :name :author etc. optional
  :serial t ; means: the dependencies are liner
  :components (
	       (:file "primality") ; without the ".lisp" !
	       (:file "higher-order-procedure")
	       )
)
