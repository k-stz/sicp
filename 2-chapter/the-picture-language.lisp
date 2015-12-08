(defpackage :sicp-picture-language
  (:use :cl
	:sicp
	:pic-lang ;; in system: picture-language-package
	))

(in-package :sicp-picture-language)

;; Section 2.2.4 Example: the picture language


;; We consider a picture language that helps us describe an image that is transformed and
;; enclosed by a parallelogram.

;; TODO: rewrite the three properties of language with the acutall example. e.g. means
;;       of combiniation is a closure who's components are procedures, not lists as until
;;       now with CONS.

;; This language has primitives: _the painter_ we call WAVE that draws crude lines

;; Means of combination: e.g. BELOW that takes WAVEs (painter) as argument and returns a
;; new painter. It has the closure property.

;; Means of abstraction: BELOW can be abstracted by WAVE2, using a DEFUN

;; The language uses ordinary Lisp functions and hence allows us to use do everything we
;; can do with Lisp functions, for example treat them as first-class objects.



