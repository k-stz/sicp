(defpackage :sicp-picture-language
  (:use :cl
	:sicp
	:pic-lang ;; in system: picture-language-package
	))

(in-package :sicp-picture-language)

;; Section 2.2.4 Example: the picture language


;; We consider a picture language that helps us describe an image that is transformed and
;; enclosed by a parallelogram.

;; TODO: rewrite the three properties of a language with the actual example. e.g. means
;;       of combination is a closure who's components are procedures, not lists as until
;;       now with CONS.

;; This language has primitives: _the painter_ we call WAVE that draws crude lines

;; Means of combination: e.g. BELOW that takes WAVEs (painter) as argument and returns a
;; new painter. It has the closure property.

;; Means of abstraction: BELOW can be abstracted by WAVE2, using a DEFUN

;; The language uses ordinary Lisp functions and hence allows us to use do everything we
;; can do with Lisp functions, for example treat them as first-class objects.

;; TODO: test once picture language core is implemented

;; (defvar wave2 (beside wave (flip-vert wave)))
;; (defvar wave4 (below wave2 wave2))

;; (defun flipped-pairs (painter)
;;   (let ((painter2 (beside painter (flip-vert painter))))
;;     (below painter2 painter2)))

;; (defvar wave4 (flipped-pairs wave))

;; (defun right-split (painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (right-split painter (- n 1))))
;; 	(beside painter (below smaller smaller)))))

;; Exercise 2.44

;; (defun up-split (painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (right-split painter (- n 1))))
;; 	(below painter (beside smaller smaller)))))
