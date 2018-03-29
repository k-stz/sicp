(defpackage :sicp
  (:use :cl)
  (:export :for-each ;; 2-chapter/closure.lisp
	   ))

(defpackage :2-4-sicp
  (:use :cl)
  (:export :put-op
	   :get-op
	   :get-entry
	   :get-types-table
	   :*op-table*
	   :apply-generic
	   :attach-tag
	   :type-tag
	   :contents))

(defpackage :2-5-sicp
  (:use :cl
	:2-4-sicp))

