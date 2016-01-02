(defpackage :sicp-picture-language
  (:use :cl
	:sicp
	:pic-lang ;; in system: picture-language-package
	)
  ;; we only need want this particual functions to clear the screen
  (:import-from :pic-objects :clear-lines :clear-parallelograms
		:x1 :x2 :y1 :y2 :get-rectangle
		:add-line-segment
		:add-rectangle-as ;; used to draw rectangles
		:vec3 ;; needed for RECTANGLE internal data representation
		))

(in-package :sicp-picture-language)

;; To get anything to draw on the screen, first run the sdl window:
;; (pic-lang:main)

;; utils------------------------------------------------------------------------

(defun clear-screen ()
  ;; TODO: later add clear rectangles objects once needed
  (clear-lines)
  (clear-parallelograms)
  (format t "~&Screen Cleared~%"))

;;------------------------------------------------------------------------------


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

(defun vector-length (vector)
  (coerce (sqrt (+ (expt (xcor-vector vector) 2)
		   (expt (ycor-vector vector) 2)))
	  'single-float))

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

;;; Exercise - 2.48

;; constructor
(defun make-segment (start-vector end-vector)
  (list start-vector end-vector))

;; handy macro for segment creation
(defmacro defsegment (sv-x sv-y ev-x ev-y) ; sv = start-vector (...)
  "Handy macro to write segments easier. Especially when trying
to make new Painters!"
  `(make-segment (make-vector ,sv-x ,sv-y)
		 (make-vector ,ev-x ,ev-y)))

;; selector - reuse from "line-segment.lisp"
(defun start-segment (line-segment)
  (first line-segment))

(defun end-segment (line-segment)
  (second line-segment))

;; segment utils

(defun mid-point (segment)
  (let* ((start-segment (start-segment segment))
	 (end-segment (end-segment segment)))
    (make-vector
     (newton-sicp:avg (xcor-vector start-segment)
		      (xcor-vector end-segment))
     (newton-sicp:avg (ycor-vector start-segment)
		      (ycor-vector end-segment)))))

;;

(defun transform-line-segments (frame line-segments)
  (let ((transform-fn (frame-coord-map frame)))
    (loop for line-segment in line-segments
       :collect
	 (list
	  ;; so we can change the implementation detail of line-segment
	  ;; else, if we knew it is just a list, we could mapcar over
	  ;; both vectors that make up the line-segment
	  (funcall transform-fn
		   (start-segment line-segment))
	  (funcall transform-fn 
	  	   (end-segment line-segment))))))

(defparameter *rectangle-line-segments*
  (list
   (make-segment (make-vector 0.0 0.0) (make-vector 0.0 1.0))
   (make-segment (make-vector 0.0 0.0) (make-vector 1.0 0.0))
   (make-segment (make-vector 1.0 0.0) (make-vector 1.0 1.0))
   (make-segment (make-vector 0.0 1.0) (make-vector 1.0 1.0))))

(defparameter *test-frame*
  (make-frame (make-vector 100 100) (make-vector 50 0) (make-vector 100 50)))

(defun draw-line-segment (line-segment)
  (let ((v1 (start-segment line-segment))
	(v2 (end-segment line-segment)))
    (apply #'add-line-segment 
     (list (xcor-vector v1) (ycor-vector v1)
	   (xcor-vector v2) (ycor-vector v2)))))

;; With this we can finally implement our first painter

;; see below for a better implementation
;; (defun parallelogram-1 (frame)
;;   "A painter that draws a parallelogram of lines."
;;   (mapcar #'draw-line-segment
;; 	  (transform-line-segments frame *rectangle-line-segments*)))


;;

(defun segments->painter (segment-list)
  (lambda (frame)
    ;; exporting every symbol that might is used in later exercises or chapters 
    (for-each
     (lambda (segment)
       (draw-line-segment ;; we draw here, because FOR-EACH doesn't return a meaningful
	                  ;; value it is supposed to be used to perform an action 
	(make-segment
	 (funcall (frame-coord-map frame) (start-segment segment))
	 (funcall (frame-coord-map frame) (end-segment segment)))))
     segment-list)))

(defparameter *nyo-verts*
  (list
   (make-vector 0.0 0.0) (make-vector 1.0 0.0)   ;; x1 x2
   (make-vector 0.0 1.0) (make-vector 1.0 1.0))) ;; y1 y2

(defun nyo (frame)
  "A Painter. Draws Nyo on the screen. Not quite as fancy as Rogers."
  (let* ((coord
	  (mapcar #'(lambda (vector)
		      ;; here we translate the (1.0 1.0) pairs into
		      ;; #(1.0 1.0 0.0) vec3, which is the expected type
		      ;; the rectangle drawing function wants. Just an implementation detail
		      (apply #'vec3 vector))
		  (mapcar (frame-coord-map frame) *nyo-verts*)))
	 (trans-x1 (elt coord 0))
	 (trans-x2 (elt coord 1))
	 (trans-y1 (elt coord 2))
	 (trans-y2 (elt coord 3))
	 (nyo (pic-objects:make-rectangle)))
    (with-slots (x1 x2 y1 y2) nyo
      (setf x1 trans-x1
	    x2 trans-x2
	    y1 trans-y1
	    y2 trans-y2))
    (add-rectangle-as (gensym) nyo)))

;; for painter creating tests

(defparameter *rect-frame* (make-frame (make-vector 250.0 250.0)
				       (make-vector 100.0 0.0)
				       (make-vector 0.0 100.0)))

;;; Exercise - 2.49

;; a) Draws the outline of the designated frame
(defun parallelogram (frame)
  "A Painter. Draws the outline of the designated frame."
  (funcall
   (segments->painter *rectangle-line-segments*)
   frame))

;; b)
(defun cross (frame)
  "A Painter. Draws a cross."
  (funcall
   (segments->painter
    (list
     (make-segment (make-vector 0.0 0.0)
			(make-vector 1.0 1.0))
     (make-segment (make-vector 1.0 0.0)
			(make-vector 0.0 1.0))))
   frame))

;; c)
;; (defun diamond (frame)
;;   "A Painter. Draws a diamond shape."
;;   (let ((mid-points (mapcar #'mid-point *rectangle-line-segments*)))
;;     (funcall
;;      (segments->painter
;;       (list
;;        ;; well not quite elegant TODO: find elegant solution
;;        ;; to iterating through list taking two arguments
;;        (make-segment (first mid-points)
;; 		     (second mid-points))
;;        (make-segment (second mid-points)
;; 		     (third mid-points))
;;        (make-segment (third mid-points)
;; 		     (fourth mid-points))
;;        (make-segment (fourth mid-points)
;; 		     (first mid-points))))
;;      frame)))

;; d)
(defun wave (frame)
  "A Painter. Draws the SICP wave shape."
  (funcall
   (segments->painter
    (list
     ;; head
     (defsegment 0.4 1.0 0.35 0.85)
     (defsegment 0.6 1.0 0.65 0.85)
     (defsegment 0.35 0.85 0.4 0.65)
     (defsegment 0.65 0.85 0.6 0.65)
     (defsegment 0.4 0.65 0.35 0.65)
     (defsegment 0.6 0.65 0.7 0.65)
     ;; left arm
     (defsegment 0.35 0.65 0.2 0.60)
     (defsegment 0.2 0.60 0.0 0.85)
     (defsegment 0.0 0.70 0.2 0.45)
     (defsegment 0.35 0.6 0.2 0.45)
     (defsegment 0.35 0.6 0.4 0.55)
     ;; left leg
     (defsegment 0.4 0.55 0.25 0.0)
     (defsegment 0.38 0.0 0.5 0.33)
     ;; right leg
     (defsegment 0.5 0.33 0.62 0.0)
     (defsegment 0.73 0.0 0.60 0.5)
     ;; right arm
     (defsegment 0.6 0.5 1.0 0.2)
     (defsegment 0.7 0.65 1.0 0.4)))
   frame))



;; Transforming a combining painters

(defun transform-painter (painter origin corner-1 corner-2)
  "Returns a Painter that will be transformed given the unit-square representation
^as arguments: origin, coner-1, corner-2. This unit-square transformation is in
terms of the frame eventually passed to the PAINTER upon invokation!"
  (lambda (frame)
    (let* ((trans-fn (funcall #'frame-coord-map frame))
	   (new-origin (funcall trans-fn origin)))
      (funcall painter
	       (make-frame new-origin
			   ;; vector from new-origin to the transformed corner-1
			   (sub-vector (funcall trans-fn corner-1) new-origin)
   			   (sub-vector (funcall trans-fn corner-2) new-origin))))))


(defun flip-vert (painter)
  "Flip the PAINTER resulting drawing upside down."
  (transform-painter painter
		     (make-vector 0.0 1.0) ;; new origin
		     (make-vector 1.0 1.0) ;; new end of edge-1!
		     (make-vector 0.0 0.0) ;; new end of edge-2!
		     ))

(defun shrink-to-upper-right (painter)
  "PAINTER trasnformation."
  (transform-painter painter
		     (make-vector 0.5 0.5)
		     (make-vector 1.0 0.5)
		     (make-vector 0.5 1.0)))
