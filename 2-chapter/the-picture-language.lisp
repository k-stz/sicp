(defpackage :sicp-picture-language
  (:use :cl
	:sicp
	:pic-lang ;; in system: picture-language-package
	)
  ;; to clear the screen
  (:import-from :pic-objects :clear-lines :clear-parallelograms
		;; modify the NYO-painter rectangle
		:x1 :x2 :y1 :y2 :get-rectangle
		;; draw lines
		:add-line-segment
		;; draw NYO - the painter substitute for SICP's Roger
		:add-rectangle-as
		:vec3 ;; needed for NYO's internal data representation
		))

(in-package :sicp-picture-language)

;; To get anything to draw on the screen, first run the sdl window:
;; (pic-lang:main)

;; utils------------------------------------------------------------------------

(defun clear-screen ()
  "Clears the all object from the screen."
  (clear-lines)
  (clear-parallelograms)
  (format t "~&Screen Cleared~%"))

;;------------------------------------------------------------------------------


;; Section 2.2.4 Example: the picture language


;; We consider a picture language that let's us describe a picture that is enclosed and
;; transformed by a parallelogram (the "frame"), to fit them. Then we draw more intricate
;; pictures by dividing the frame in sub-frames and draw further pictures within them.



;; The properties of a language, according to SICP

;; 1. primitives: _the painter_ such as WAVE, NYO, DIAMOND, CROSS etc.  that draw crude
;; lines, or a picture, like NYO, inside a given frame.


;; 2. means of combination: with the powerful PAINTER at its core we have procedures that
;; compose new painters from other painters that satisfy the closure property such as
;; BELOW, BESIDE, FLIP-VERT.

;; 3. means of abstraction: First we have data abstraction. See the implementation of
;; frames with their constructor and selectors MAKE-FRAME and ORIGIN-FRAME. The
;; implementation of vectors with MAKE-VECTOR and their selector XCOR-VECTOR AND
;; YCOR-VECTOR.
;; Since the language was implemented on top of Lisp, we have all the abstractions
;; available that the language provides, such as defining functions and higher-order
;; functions.  Procedure abstraction is what actualised the implementation of PAINTERs and
;; which masked a great deal of complexity by creating new line-drawing painters in the
;; form of SEGMENTS->PAINTER, and the core function, FRAME-COORD-MAP, which, after
;; implementing TRANSFORM-PAINTER with it, allowed to easily implement the core feature of
;; our picture language, the drawing of nested pictures.



;;; The picture language implementation:


;; vector stuff, incidentally, this is

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

;; alternative implementation - part of the exercise:

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

;; A distorted --non-rectangular-- frame
(defvar *test-frame*
  (make-frame (make-vector 100 100) (make-vector 50 0) (make-vector 100 50)))

(defun draw-line-segment (line-segment)
  (let ((v1 (start-segment line-segment))
	(v2 (end-segment line-segment)))
    (apply #'add-line-segment 
     (list (xcor-vector v1) (ycor-vector v1)
	   (xcor-vector v2) (ycor-vector v2)))))

;;

(defun segments->painter (segment-list)
  "Takes a bunch of segments, see MAKE-SEGMENT, and returns a PAINTER that will draw
them. Expects the line to be within a unit-frame, meaning coordinate values are in range
[0,1]."
  (lambda (frame)
    ;; exporting every symbol that might be used in later exercises or chapters 
    (for-each
     (lambda (segment)
       (draw-line-segment ;; we draw here, because FOR-EACH doesn't return a meaningful
	                  ;; value, it is supposed to be used to perform an action 
	(make-segment
	 (funcall (frame-coord-map frame) (start-segment segment))
	 (funcall (frame-coord-map frame) (end-segment segment)))))
     segment-list)))

(defvar *nyo-verts*
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
    ;; changing NYO drawing to just display the "front" animation
    (pic-objects::change-animation-state nyo :walk :left 0 :nyo)
    (pic-objects::apply-animation-state nyo)
    ;; /changing animation done
    (with-slots (x1 x2 y1 y2) nyo
      (setf x1 trans-x1
	    x2 trans-x2
	    y1 trans-y1
	    y2 trans-y2))
    (add-rectangle-as (gensym) nyo)))

;;; Exercise - 2.49

(defvar *rectangle-line-segments*
  (list
   (make-segment (make-vector 0.0 0.0) (make-vector 0.0 1.0))
   (make-segment (make-vector 0.0 0.0) (make-vector 1.0 0.0))
   (make-segment (make-vector 1.0 0.0) (make-vector 1.0 1.0))
   (make-segment (make-vector 0.0 1.0) (make-vector 1.0 1.0)))
  "Data for drawing a rectangle.")

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
(defun diamond (frame)
  "A Painter. Draws a diamond shape."
  (funcall
   (segments->painter
    (list
     (defsegment 0.5 0.0 1.0 0.5)
     (defsegment 1.0 0.5 0.5 1.0)
     (defsegment 0.5 1.0 0.0 0.5)
     (defsegment 0.0 0.5 0.5 0.0)))
   frame))

;; d)
(defun wave (frame)
  "A Painter. Draws the SICP wave shape."
  (funcall
   (segments->painter
    (list
     ;; smile
     (defsegment 0.45 0.7 0.5 0.67)
     (defsegment 0.5 0.67 0.55 0.7)
     
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
     (defsegment 0.4 0.0 0.5 0.33)
     ;; right leg
     (defsegment 0.5 0.33 0.60 0.0)
     (defsegment 0.73 0.0 0.60 0.5)
     ;; right arm
     (defsegment 0.6 0.5 1.0 0.15)
     (defsegment 0.7 0.65 1.0 0.3)))
   frame))



;; Transforming a combining painters


;; Frame transformation is the key to defining means of combination for two or more
;; painters

(defun transform-painter (painter origin corner-1 corner-2)
  "Returns a Painter that will be transformed given the unit-square representation
as arguments: origin, corner-1, corner-2. This unit-square transformation is in
terms of the frame eventually passed to the PAINTER upon invocation!"
  (lambda (frame)
    (let* ((trans-fn (funcall #'frame-coord-map frame))
	   (new-origin (funcall trans-fn origin)))
      (funcall painter
	       (make-frame new-origin
			   ;; vector from new-origin to the transformed corner-1
			   (sub-vector (funcall trans-fn corner-1) new-origin)
   			   (sub-vector (funcall trans-fn corner-2) new-origin))))))


;; some painter operation that satisfy the closure property

(defun flip-vert (painter)
  "Flip the PAINTER resulting drawing upside down."
  (transform-painter painter
		     (make-vector 0.0 1.0) ;; new origin
		     (make-vector 1.0 1.0) ;; new end of edge-1!
		     (make-vector 0.0 0.0) ;; new end of edge-2!
		     ))

(defun shrink-to-upper-right (painter)
  "PAINTER transformation. Ditto."
  (transform-painter painter
		     (make-vector 0.5 0.5)
		     (make-vector 1.0 0.5)
		     (make-vector 0.5 1.0)))


(defun rotate-90 (painter)
  "PAINTER transformation. Rotate image 90-degree counterclockwise."
  (transform-painter painter
		     (make-vector 0.0 1.0)
		     (make-vector 0.0 0.0)
		     (make-vector 1.0 1.0)))

(defun squash-inwards (painter)
  (transform-painter painter
		     (make-vector 0.0 0.0)
		     (make-vector 0.65 0.35)
		     (make-vector 0.35 0.65)))

(defun beside (painter-1 painter-2)
  "Returns a PAINTER that'll draw two pictures next to each other."
  (let ((paint-left
	 (transform-painter painter-1
			    (make-vector 0.0 0.0)
			    (make-vector 0.5 0.0)
			    (make-vector 0.0 1.0)))
	(paint-right
	 (transform-painter painter-2
			    (make-vector 0.5 0.0)
			    (make-vector 1.0 0.0)
			    (make-vector 0.5 1.0))))
    (lambda (frame)
      (funcall paint-left frame)
      (funcall paint-right frame))))


;;; Exercise - 2.50, FLIP-HORIZ and 180- and 270-degree rotation painter

(defun flip-horiz (painter)
  "Make Painter that will flip the image horizontally."
  (transform-painter painter
		     (make-vector 1.0 0.0)
		     (make-vector 0.0 0.0)
		     (make-vector 1.0 1.0)))


(defun rotate-180 (painter)
  (rotate-90 (rotate-90 painter)))

(defun rotate-270 (painter)
  (rotate-90 (rotate-180 painter)))


;;; Exercise - 2.51, BELOW implementation

(defun below (painter-1 painter-2)
  (let ((painter-up
	 (transform-painter painter-1
			    (make-vector 0.0 0.0)
			    (make-vector 1.0 0.0)
			    (make-vector 0.0 0.5)))
	(painter-down
	 (transform-painter painter-2
			    (make-vector 0.0 0.5)
			    (make-vector 1.0 0.5)
			    (make-vector 0.0 1.0))))
    (lambda (frame)
      (funcall painter-down frame)
      (funcall painter-up frame))))

;; BELOW in terms of rotation and BESIDE!
;; (defun below-1 (painter-1 painter-2)
;;   (rotate-90
;;    (beside (rotate-270 painter-1)
;; 	   (rotate-270 painter-2))))


;; now we can finally use some of the previously introduced painters:

(defun wave2 (frame)
  (funcall
   (beside #'wave (flip-vert #'wave))
   frame))

(defun wave4 (frame)
  (funcall
   (below #'wave2 #'wave2)
   frame))



;; The rest of the previously implemented function before the core of the
;; language was there to test them

(defun flipped-pairs (painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(defun right-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

;; Exercise 2.44

(defun up-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))


(defun corner-split (painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let (;; change due to exercise 2.52
	      ;; (top-left (beside up up))
	      (top-left up)
	      ;; (bottom-right (below right right))
	      (bottom-right right)
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		 (below bottom-right corner))))))

;;

(defun square-limit (painter n)
  (let* ((quarter (corner-split painter n))
	 (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))



;;; higher-order operations


;; see further below for examples FLIPPED-PAIRS and SQUARE-LIMIT-2
(defun square-of-four (tl tr bl br)
  "Takes four one-argument painter operations and will arrange the given PAINTER
in a square-of-four. The images transformed then get assigned to one of the four square
namely, for example, tl to the _t_op _l_eft one.
Now you only need to pass it a PAINTER to return a PAINTER!"
  (lambda (painter)
    (let ((top (beside (funcall tl painter) (funcall tr painter)))
	  (bottom (beside (funcall bl painter) (funcall br painter))))
      (below bottom top))))


(defun flipped-pairs (painter)
  (let ((combine4 (square-of-four #'identity #'flip-vert
				  #'identity #'flip-vert)))
    (funcall combine4 painter)))


;; (defun square-limit-2 (painter n)
;;   (let ((combine4 (square-of-four #'flip-horiz #'identity
;; 				  #'rotate-180 #'flip-vert)))
;;     (funcall combine4 (corner-split painter n))))


;;; Exercise 2.45

;; SPLIT, a higher-order procedure, which can implement RIGHT-SPLIT and UP-SPLIT

(defun split (fn-1 fn-2)
  "General painter splitting operation. Must be given two two-argument painter
transformation operations."
  (labels ((splitter (painter n)
	     (if (= n 0)
		 painter
		 (let ((smaller (splitter painter (- n 1))))
		   (funcall fn-1 painter
			    (funcall fn-2 smaller smaller))))))
    #'splitter ;; to return the function slot content
    ))

;; example of SPLIT use; implementing UP-SPLIT in terms of SPLIT
;; (defun splitter-up-split (painter n)
;;   (funcall (split #'below #'beside)
;; 	   painter
;; 	   n))


;; Convenient for tests

(defvar *big-frame*
  (make-frame (make-vector 150.0 100.0)
	      (make-vector 400.0 0.0)
	      (make-vector 0.0 400.0)))

(defun test-painter (painter)
  "Draws the painter given on a big, square frame. For quick tests."
  (clear-screen)
  (funcall painter *big-frame*))



;; Showcasing the stratified design in action, that is
;; Exercise 2.51

;; a) added some lines, to form a smile, to the WAVE picture

;; b) CORNER-SPLIT modified

;; c) modify SQUARE-LIMIT, so that NYO looks outside

(defun m-square-limit (painter n)
  ;; the FLIP-HORIZ here takes care of Nyo looking the other way
  (let ((quarter (corner-split (flip-horiz painter) n)))
    (funcall
     ;; here we abstracted away the (half (beside ...) (below (flip-vert half) half)
     (square-of-four #'flip-horiz #'identity
		     #'(lambda (painter)
			 (flip-vert (flip-horiz painter))) #'flip-vert)
     quarter)))

