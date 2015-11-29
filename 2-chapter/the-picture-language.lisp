(defpackage :sicp (:use :cl))
(in-package :sicp)

;; Section 2.2.4 Example: the picture language


;; We consider a picture language that helps us describe an image that is transformed and
;; enclosed by a parallelogram.

;; This language has primitives: _the painter_ we call WAVE that draws crude lines

;; Means of combination: e.g. BELOW that takes a WAVEs (painter) as argument and returns a
;; new painter. It has the closure property.

;; Means of abstraction: BELOW can be abstracted by WAVE2, using a DEFUN

;; The language uses ordinary Lisp functions and hence allows us to use do everything we
;; can do with Lisp functions, for example they're in turn first-class objects.
