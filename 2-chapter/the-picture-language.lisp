(defpackage :sicp-picture-language
  (:use :cl
	:sicp
	:pic-lang ;; in system: picture-language-package
	))

(in-package :sicp-picture-language)

;; To get anything to draw on the screen, first run the sdl window:
;; (pic-lang:main)



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
;; 	  (beside (below painter top-left)
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


;; vector stuff, incidentally this is

;; Exercise 2.46

(defun make-vector (&rest components)
  "Vector constructor."
  components)


(defun xcor-vector (vector)
  (first vector))

(defun ycor-vector (vector)
  (second vector))

(defun add-vector (v1 v2)
  (map 'list #'+ v1 v2))

(defun sub-vector (v1 v2)
  (map 'list #'- v1 v2))

(defun scale-vector (scalar vector)
  (mapcar #'(lambda (component) (* component scalar))
	  vector))

;;

(defun frame-coord-map (frame)
  "Returns a function that maps a unit vector to a vector in the frame. Meaning v(0,0) is
at origin _of the frame_ and v(1,1) is the point across the diagonal."
  (lambda (vector)
    (add-vector
     (origin-frame frame)
     (add-vector (scale-vector (xcor-vector vector)
			       (edge-1-frame frame))
		 (scale-vector (ycor-vector vector)
			       (edge-2-frame frame))))))

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


;;; test painter

;; constructor
(defun make-line-segment (vector-1 vector-2)
  (list vector-1 vector-2))

;; selector
(defun line-segment-vector-1 (line-segment)
  (first line-segment))

(defun line-segment-vector-2 (line-segment)
  (second line-segment))


(defun transform-line-segments (frame line-segments)
  (let ((transform-fn (frame-coord-map frame)))
    (loop for line-segment in line-segments
       :collect
	 (list
	  ;; so we can change the implementation detail of line-segment
	  ;; else, if we knew it is just a list, we could mapcar over
	  ;; both vectors that make up the line-segment
	  (funcall transform-fn
		   (line-segment-vector-1 line-segment))
	  (funcall transform-fn 
	  	   (line-segment-vector-2 line-segment))))))

(defparameter *rectangle-line-segments*
  (list
   (make-line-segment (make-vector 0.0 0.0) (make-vector 0.0 1.0))
   (make-line-segment (make-vector 0.0 0.0) (make-vector 1.0 0.0))
   (make-line-segment (make-vector 1.0 0.0) (make-vector 1.0 1.0))
   (make-line-segment (make-vector 0.0 1.0) (make-vector 1.0 1.0))))

(defparameter *test-frame*
  (make-frame (make-vector 100 100) (make-vector 50 0) (make-vector 100 50)))

(defparameter *transformed-line-segments*
  (transform-line-segments *test-frame* *rectangle-line-segments*))


(defun draw-line-segment (line-segment)
  (let ((v1 (line-segment-vector-1 line-segment))
	(v2 (line-segment-vector-2 line-segment)))
    (apply #'pic-objects:add-line-segment 
     (list (xcor-vector v1) (ycor-vector v1)
	   (xcor-vector v2) (ycor-vector v2)))))

;; With this we can finally implement our first painter

(defun parallelogram (frame)
  "A painter that draws a parallelogram of lines."
  (mapcar #'draw-line-segment
	  (transform-line-segments frame *rectangle-line-segments*)))


;;

(defun segments->painter (segment-list)
  (lambda (frame)
    (for-each)))
