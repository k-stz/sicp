(defpackage :sicp (:use :cl))
(in-package :sicp)


;; Data is just some collection of constructors and selectors some conditions
;; they must fulfill to be a valid representation

;; a pair, here represented by a closure, a function closing over the input that
;; will depending on its input retrieve the closed over data. We say the data is
;; has a /procedural representation/.

;; Message passing,
;; furthermore the style employed here is called /message passing/ this means we
;; pass some information to an object or process, here the m-cons returned precedure
;; (a function object), and the object decides what sub-routine to run.
;; This is to be contrasted by simply calling the routine directly. Basically whneever
;; a method on an object is invoked we use /message passing/ as the object decides with
;; the argument passed "the message" which of its method to invoke.


(defun m-cons (x y)
  (lambda (i)
    (cond ((= i 0) x)
	  ((= i 1) y)
	  (t (error "Argument not 0 or 1 -- CONS")))))

(defun m-car (m-cons)
  (funcall m-cons 0))

(defun m-cdr (m-cons)
  (funcall m-cons 1))


;; exercise 2.4

(defun 2-4-cons (x y)
  (lambda (m) (funcall m x y)))

(defun 2-4-car (2-4-cons)
  (funcall 2-4-cons (lambda (p q) (declare (ignore q))
			    p)))

;; since 2-4-cons lambda applies "m" to two arguments x and y, then we need to pass it a
;; lambda that takes two arguments. Those arguments, again, will be x and y in the lambda
;; body, so we just need to return either of those to emulate a CAR or CDR respectively

(defun 2-4-cdr (2-4-cons)
  (funcall 2-4-cons (lambda (head tail) (declare (ignore head))
			    tail)))



;; Exercise 2.5
;; show that we can represent a pair of non-negative numbers using:
;; (2^a)*(3^b)
;; First of how'd we go about retrieving a and b from that term:
;; 2^a = (3^b)/(2^a), and finally (log 2^a 2) => a
;; Which means we need to store 2^a, 3^a and the corresponding logairthm bases
;; 2 and 3 in the functional object

(defun 2-5-cons (x y)
  (lambda (i)
    (let* ((divisor (ecase i
		     (0 (expt 3 y))
		     (1 (expt 2 x))))
	  (base-a
	   (/ (* (expt 2 x)
		 (expt 3 y))
	      divisor)))
      (ecase i
	(0 (log base-a 2))
	(1 (log base-a 3))))))

(defun 2-5-car (2-5-cons)
  (funcall 2-5-cons 0))

(defun 2-5-cdr (2-5-cons)
  (funcall 2-5-cons 1))

;; The numbers 2 and 3 are both primes, according to the _fundamental theorem of
;; arithmetic_ every positive integer can be represented as the product of primes.
;; Also any such product is unique up to the order of factors.
;; This menas that (* 2^a 3^b) yields a unique numbers. Hence we know that if we
;; remove either factor (2 or 3) we will yield _integers_ but whenever a factor
;; is missing and we remove it we will with absolute certainty get a non integer
;; because we will then divide a prime by _a different_ prime, which is always a
;; non-integer by definition.
;; Hence what we need is a procedure that tests how many times a division of
;; a number still yields an integer - a modulo test:

(defun remainder-zero-n-times (dividend divisor)
  "Returns the number of times the divisor can be divided by the divident while at
each division yielding a remainder of zero -- an integer"
  (labels ((rec (quotient n)
	     (if (= (mod quotient divisor) 0)
		 (rec (/ quotient divisor) (1+ n))
		 n)))
    (rec dividend 0)))

(defun new-2-5-cons (x y)
  ;; (* 2^a 2^b)
  (* (expt 2 x)
     (expt 3 y)))

(defun new-2-5-car (new-2-5-cons)
  (remainder-zero-n-times
   new-2-5-cons
   2))

(defun new-2-5-cdr (new-2-5-cons)
  (remainder-zero-n-times
   new-2-5-cons
   3))
