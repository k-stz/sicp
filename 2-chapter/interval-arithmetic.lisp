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
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))


;; Exercise 2.8

(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))


;; Exercise 2.9

(defun interval-width (interval)
  (/
   (abs (- (lower-bound interval)
	   (upper-bound interval)))
   2))


;; Does the width of an interval combination through addition or substraction
;; yield the same result as using each intervalls width for the same combination?
;; (width (add-interval interval-x interval-y)) = (+ (width interval-x) (width interval-y)?)

(defun width-test (x y)
  (print (interval-width (add-interval x y)))
  (print (+ (interval-width x) (interval-width y))))

;; This relation holds true as long as (<= lower-bound upper-bound)
;; The combination width is directly derived from the addition, negation of it's
;; components bounds. Given a width from one interval we get the combination width by
;; changin the bound of of the interval by exactly the magnitude of the other
;; bound. Therefore the combination bound changes by the same magnitude.

;; This doesn't work for multiplication: negating the bounds (muliplying a negative and
;; positive bound), doesn't change the width of one bounds with the same magnitude as
;; the combination. Just imagine two identical intervals |-2 -1|, with 0.5 width they
;; combine into |1 4| with width 1.5.
;; Division is prone to same property when dividing positive and negative numbers.


;; Exercise 2.10

(defun div-interval-safely (x y)
  "Divide intervals x and y. Signals division by zero errors explicitly."
  (let ((y-lower-bound (lower-bound y))
	(y-upper-bound (upper-bound y)))
    ;; if y-lower-bound is negative, or zero, and the upper-bound positive or zero
    ;; it spans zero. Mutliplying the two yielding a negative number, or zero indicates
    ;; an interval containing 0!
    (if (<= (* y-upper-bound y-lower-bound) 0)
	(error "Division by zero: Interval ~a spans zero." y)
	;; else
	(mul-interval x
		      (make-interval (/ 1.0 y-upper-bound)
				     (/ 1.0 y-lower-bound))))))

;; Exercise 2.11

(defun spans-zero? (interval)
  (<= (* (lower-bound interval)
	 (upper-bound interval))
      0))

(defun mul-interval-bitdiddly (x y)
  (let ((lx (lower-bound x))
	(ly (lower-bound y))
	(ux (upper-bound x))
	(uy (upper-bound y)))
    (flet ((++? (interval)
	     (and (>= (lower-bound interval) 0)
		  (>= (upper-bound interval) 0)))
	   (--? (interval)
	     (and (< (lower-bound interval) 0)
		  (< (upper-bound interval) 0)))
	   (-+? (interval)
	     (and (< (lower-bound interval) 0)
		  (>= (upper-bound interval) 0)))
	   ;;+-? is not possible due to intervals total
	   ;; order.
	   ;;Which means we have 3 states of 2 combinations
	   ;; 3² = 9
	   )
      (cond ((and (++? x) (++? y))
	     (make-interval (* lx ly) (* ux uy)))
	    ((and (--? x) (--? y))
	     (make-interval (* ux uy) (* lx ly)))
	    ((and (++? x) (--? y))
	     (make-interval (* ux ly) (* lx uy)))
	    ((and (--? x) (++? y))
	     (make-interval (* uy lx) (* ly ux)))
	    ((and (++? x) (-+? y))
	     (make-interval (* ux ly) (* ux uy)))
	    ((and (-+? x) (++? y))
	     (make-interval (* lx uy) (* ux uy)))
	    ((and (--? x) (-+? y))
	     (make-interval (* lx uy) (* lx ly)))
	    ((and (-+? x) (--? y))
	     (make-interval (* ux ly) (* lx ly)))
	    ((and (-+? x) (-+? x))
	     ;; the special case, since we don't know if
	     ;; the negative numbers are bigger than the positive
	     ;; ones, we need to perform all the multiplications
	     (let ((p1 (* lx ly))
		   (p2 (* lx uy))
		   (p3 (* ux ly))
		   (p4 (* ux uy)))
	       (make-interval (min p1 p2 p3 p4)
			      (max p1 p2 p3 p4))))))))

;;

(defun make-center-width (interval-center width+-)
  (make-interval (- interval-center width+-)
		 (+ interval-center width+-)))

(defun center (interval)
  (/ (+ (lower-bound interval)
	(upper-bound interval))
     2.0))

(defun width (interval)
  (- (upper-bound interval)
     (center interval)))

;; Exercise 2.12

(defun make-center-percent (center percent)
  (let ((width+- (* center (/ percent 100.0))))
    (make-center-width center width+-)))

;; for style reasons why simply undo the width+- calculation above
;; mathematically
(defun percent (interval)
  (/ (* (width interval)
	100.0)
     (center interval)))


;; Exercise 2.13

;; (make-center-percent 20 15) ==> (17.0 . 23.0)
;; (make-center-percent 10 10) ==> (9.0 . 11.0)
;; (percent (mul-interval * **)) ==> 24.630543
;;
;; Assumption: sum of percentages of the factors is target interval's
;;              approximate percentage

;;

;; ':' means "such that"

;; center-points x and y , or "the factors" in the exercise
;; x : x >= 0
;; y : y >= 0

;; numer : numer ∈ {0, 1 ... 100} aka small percentages
;; p = numer/100

;; z : x or y
;; z*p >= 0  (simplification provided by the exercise)
;; upper-bound = z+z*p : upper-bound >= 0
;; lower-bound = z-z*p : lower-bound >= 0

;; if i and i' are two intervals, and their upper-bound and lower-bound
;; are positive, as the exercise suggest then their multiplication will
;; also be positive. From this also follows

;; upper-bound(i) > lower-bound(i)
;; upper-bound(i')*upper-bound(i) > any other combination involving the
;; values for upper-bound and lower-bound

;; hence if
;; t = i*i'
;; then from the above follows
;; upper-bound(t) = upper-bound(i)*upper-bound(i'), similiarly for
;; lower-bound(t).

;; corollary if x,y are centers of i and i' then
;; (x+x*p) * (y+y*p') ≅ upper-bound(t) ;; similarly for lower-bound(t)
;;  = (x+x*p) * (y+y*p')
;;  = x(1+p) * y(1 +p')
;;  = y*x(1+p)(1+p') 
;;  = y*x(1+p'+p+p*p')
;; The exercise gives us the freedom to ignore small percentages, which
;; is what we get if we take the percentage of a percentage (p*p'), hence:
;;  upper-bound (t) ≅ y*x(1+p'+p)

;; similarly for lower-bound, which shows that out assumption of adding
;; the percentage is approximately the percentage of the product intervals


;; Lem E. Tweakit parallel resistor functions

 (defun par1 (r1 r2)
   (div-interval (mul-interval r1 r2)
		 (add-interval r1 r2)))

(defun par2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;; Lem is right, the above return different results for intervals r1:(1 1) r2:(1 2)

;; lets focus on the use of division, expexcially on the inverse (/ 1 r1)
;; usually: (1*x)/x = 1

(defvar *one* (make-interval 1 1))

(defun inv-interval (interval)
  (div-interval *one* interval))

;; (let ((test-interval (make-interval 1 2)))
;;   (mul-interval
;;    (inv-interval test-interval)
;;    test-interval)) ==> (0.5 . 2.0)

;; this proves that the following inverse multiplication identity does _not_ hold
;; true for intervals

;; (1*r)/r ≠ 1  : r is an interval

;; examining the inverse operations shows that the lower-bound will take the
;; smallest value of all the inversions on its range and the upper-bound the
;; biggest. If we now multiply the pre-inversion interval with the inverse one
;; it will build the endpoints in terms of these extremes in stead of mediating
;; for identity.

;; the same is true of division identities
;; (div-interval (make-interval 1 2) (make-interval 1 2)) will not return *one*
;; but rather will assign the endpoint to the extremes, namely the smallest value
;; from the division 1/2 and the biggest 2/1 yielding interval (0.5 2) instead of
;; (1 2)
