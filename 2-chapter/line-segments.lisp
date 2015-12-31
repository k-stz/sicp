(defpackage :sicp
  (:use :cl))

(in-package :sicp)

;;; exercise 2.2

(defun make-point (x y)
  (cons x y))

(defun x-point (point)
  (car point))

(defun y-point (point)
  (cdr point))

(defun make-segment (start-point end-point)
  (cons start-point end-point))

(defun start-segment (segment)
  (car segment))

(defun end-segment (segment)
  (cdr segment))

(defun print-point (point)
  (format t "~&(~a,~a)~%"
	  (x-point point)
	  (y-point point )))

(defun mid-point (segment)
  (let* ((start-segment (start-segment segment))
	 (end-segment (end-segment segment)))
    (make-point
     (newton-sicp:avg (x-point start-segment)
		      (x-point end-segment))
     (newton-sicp:avg (y-point start-segment)
		      (y-point end-segment)))))


(defun segment-length (segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    ;; √a²+b² = c
    (sqrt (+ (expt (abs (- (x-point start)
			   (x-point end))) 2)
	     (expt (abs (- (y-point start)
			   (y-point end))) 2)))))

;; exercise 2.3

;; rectangle perimeter, area

(defun make-rectangle (origin-point top-right-point)
  (cons origin-point top-right-point))

(defun rec-origin (rectangle)
  (car rectangle))

(defun rec-top-right (rectangle)
  (cdr rectangle))

(defun rectangle-area (rectangle)
  (let* ((origin (rec-origin rectangle))
	 (top-right (rec-top-right rectangle))
	 (segment-origin-to-up (make-segment origin
					 (make-point (x-point origin)
						     (y-point top-right))))
	 (segment-origin-up-to-top-right
	  (make-segment (end-segment segment-origin-to-up)
			top-right))
	 (length-horizontal (segment-length segment-origin-to-up))
	 (length-vertical (segment-length segment-origin-up-to-top-right)))
    (* length-horizontal
       length-vertical)))

(defun rectangle-perimeter (rectangle)
  (let* ((origin (rec-origin rectangle))
	 (top-right (rec-top-right rectangle))
	 (segment-origin-to-up (make-segment origin
					 (make-point (x-point origin)
						     (y-point top-right))))
	 (segment-origin-up-to-top-right
	  (make-segment (end-segment segment-origin-to-up)
			top-right))
	 (length-horizontal (segment-length segment-origin-to-up))
	 (length-vertical (segment-length segment-origin-up-to-top-right)))
    (+ (* 2 length-horizontal)
       (* 2 length-vertical))))


(defun make-rectangle-2 (segment-ab segment-ac)
  (cons segment-ab segment-ac))

;; if we replace the selectors above with these rectangle-perimeter and rectangle-area work the same
(defun rec-origin-2 (rectangle)
  (end-segment (cdr rectangle)))

(defun rec-top-right-2 (rectangle)
  (end-segment (car rectangle)))

