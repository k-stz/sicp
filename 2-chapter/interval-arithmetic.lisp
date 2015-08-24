(defpackage :sicp (:use :cl))

(defun rp (r1 r2)
  "Computes the parallel equivalent resistance Rp of resistors r1 and r2"
  (/ 1
     (+ (/ r1) (/ r2))))


;; intervals--------------------------------------------------------------------

(defun make-interval (lower-bound upper-bound)
  (cons lower-bound upper-bound))

;; Exercise 2.7

(defun lower-bound (interval)
  (car interval))

(defun upper-bound (interval)
  (cdr interval))


(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))


(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (lower-bound y)))
	(p3 (* (upper-bound x) (upper-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound x)))))


;; Exercise 2.8

(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))
