;;;;exercise 1.5 of SICP
(defpackage sicp-1.15 (:use :cl))
(in-package :sicp-1.15)

(defparameter *n* 0)

(defun cube (x)
  (* x x x))

(defun p (x)
  (- (* 3 x)
     (* 4 (cube x))))

(defun sine (angle)
  (incf *n* 1)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;;wow: steps needed: CL-USER> (ceiling (/ (log (/ a 0.1)) (log 3))) ;note that
;;common lisp's LOG defaults to base eulers-number NOT 10

;;using n, we know that (SINE 12.5) is called 6-times, so we get a chain of deferred P
;;but on the last call to SINE, we exit the IF-statement with true, so we don't
;;call the <else> branch so there is no 6th P added to the chain
;;-> a) The procedure p is applied 5-times.
;;  y
;; ^
;; . |-----------------------------------------------------| 3*a
;; . |------------------| a
;; . |-----|
;; . |-|
;; . |
;;....................................................................> x
;; .   x=size of the problem
;; .   y=steps needed
;; doubling, or in this case rather tripling only adds one more step to solve
;;the function, so we have with double the size of the problem a constant
;;increase in resources needed, hence: logarithmic order of growth!


;; -----------------------------------------------------------------------------

;; Alternative explanation:
;; On each invocation of angle we divide the input by three (sin (/ angle 3.0))
;; order of growth for time:

;; the deferred chain grows until the input after being divided by 3 is smaller than
;; 0.1. In math "how many times do you need to divide n with x to get y" can also be
;; expressed as "how many times do you need to multiply n with the inverse of x to get y"

;; We have n, it is the input, here 12.5, we have x which is the inverse of 3:

;; (/ 3) ==> 1/3

;; remember instead of repeatedly dividing with 3, we expressed it in terms of repeatedly
;; multiplying - which would be simply multiplying with its inverse.

;; n * 1/3^steps = 0.1  ; /n
;; x^steps = 0.1/n

;; to get the exponent, steps, we need the logarithm:
;; (log 0.1/n 1/3) ==> steps

;; let's test this with the example above: input= 12.5 inverse=1/3
;; (log (/ 0.1 12.5) 1/3) ==> 4.3949203
;; and indeed
;; (* 12.5 (expt 1/3 4.3949203)) = 0.10000003
;; is equal to 0.1, so for it to be smaller than 0.1 we just round it up
;; (ceiling 4.3949203) ==> 5		

;; That means this is our time-needed formula:
;; (log 0.1/n 1/3)

;; which, by removing the constants, simply:
;; O(log n)

;; -----------------------------------------------------------------------------
;; How about space?
;; On each step we get a deferred function call (p ..), which only gets
;; resolved when the input satisfies < 0.1, which is as shown above:
;; (log 0.1/n 1/3) steps which means they're both tightly related. That's why
;; the order of growth is again:
;; O(log n)
