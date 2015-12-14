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


;; (defun corner-split (painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((up (up-split painter (- n 1)))
;; 	    (right (right-split painter (- n 1))))
;; 	(let ((top-left (beside up up))
;; 	      (bottom-right (below right right))
;; 	      (corner (corner-split painter (- n 1))))
;; 	  (besie (below painter top-left)
;; 		 (below bottom-right corner))))))

;; (defun square-limit (painter n)
;;   (let* ((quarter (corner-split painter n))
;; 	 (half (beside (flip-horiz quarter) quarter)))
;;     (below (flip-vert half) half)))


;; Exercise 2.44

;; (defun up-split (painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (right-split painter (- n 1))))
;; 	(below painter (beside smaller smaller)))))

;;; higher-order operations

;; (defun square-of-four (tl tr bl br)
;;   (lambda (painter)
;;     (let ((top (beside (funcall tl painter) (funcall tr painter)))
;; 	  (bottom (beside (bl funcall painter) (br funcall painter))))
;;       (below bottom top))))


;; (defun flipped-pairs (painter)
;;   (let ((combine4 (square-of-four identity flip-vert
;; 				  identity flip-vert)))
;;     ;; TODO: FUNCALL?
;;     (combine4 painter)))


;; (defun square-limit-2 (painter n)
;;   (let ((combine4 (square-of-four flip-horiz identity
;; 				  rotate180 flip-vert)))
;;     (combine4 (corner-split painter n))))


;;; Exercise 2.45

;; SPLIT higher-order procedure which can implement RIGHT-SPLIT and UP-SPLIT

;; (defun split (fn-1 fn-2)
;;   (labels ((rec (painter n)
;; 	     (if (= n 0)
;; 		 painter
;; 		 (let ((smaller (rec painter (- n 1))))
;; 		   (funcall fn-1 painter
;; 			    (funcall fn-2 smaller smaller))))))
;;     #'rec ;; to return the function slot
;;     ))



;;; Pic lang implementation (partly)


;;; Exercise 2.47

(defun make-frame (origin edge-1 edge-2)
  "Frame constructor."
  (list origin edge-1 edge-2))


;; selectors

(defun origin-frame (frame)
  (first frame))

(defun edge-1-frame (frame)
  (second frame))

(defun edge-2-frame (frame)
  (third frame))

;; alternative implementation - part of the exercise
;; (defun make-frame (origin edge-1 edge-2)
;;   "Frame constructor."
;;   (cons origin (cons edge-1 edge-2)))

;; ;; selectors
;; (defun origin-frame (frame)
;;   (first frame))

;; (defun edge-1-frame (frame)
;;   (second frame))

;; (defun edge-2-frame (frame)
;;   (rest (rest frame)))
