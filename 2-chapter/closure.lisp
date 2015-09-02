(defpackage :sicp (:use :cl)) (in-package :sicp)

;; Closure,
;; in abstract algebra refers to an operation that given a specific set elements
;; as input returns elements of the set. We say "the elements are closed an operation to
;; elements of the set produces an element that is again an element of the set".
;; Which is the case for the CONS operation, which always returns an object that
;; can be fed back into CONS.
;; Important distinction:
;; The lisp community, and by extension other programming languages by now, refer
;; to procedure with free variables as a closure:

;; (let ((x 2) (y 3))  ;; free variables
;;   (lambda (z)  
;;     (+ z (incf x) (decf y)))) ;; returns a closure

;; SICP will not refer to the word "closure" in this sense.

;; Furthermore on the topic of "means of combination" one of the three key properties
;; of any programming languge is always expected to satisfy the closure property.
;; Some langauges do not provide this property, such as Basic (some version at least)
;; forces one to assemble elements into arrays, but the array is not allowed to contain
;; arrays themselves.


(defun list-ref (n list)
  "CL:NTH"
  (if (= n 0)
      (car list)
      (list-ref (1- n) (cdr list))))


;; Exercise 2.17

(defun last-pair (list)
  "CL:LAST"
  (if (null (cdr list))
      list
      (last-pair (cdr list))))

;; Exercise 2.18

(defun m-reverse (list)
  "CL:REVERSE"
  (labels ((rec (reverse-list list)
	     (if (null list)
		 reverse-list
		 (rec (cons (car list) reverse-list)
		      (cdr list)))))
    (rec () list)))


;; Exercise 2.19 - cc with list input

(defparameter *us-coins* (list 50 25 10 5 1))
(defparameter *uk-coins* (list 100 50 20 10 5 2 1 0.5))
(defparameter *eu-coins* (list 200 100 50 20 10 5 2 1))

(defun cc (amount coin-list)
  "Counting unique changes of <amount> with coins in <coin-list>."
  (labels ((rec (n coin-list)
	     (cond ((null coin-list) 0)
		   ((< n 0) 0)
		   ((= n 0) 1)
		   (t ;; else:
		    (+
		     ;; Try all coins in order given.
		     ;; preconditions eliminate this branch with (< n 0) and (= n 0)
		     (rec (- n (car coin-list)) coin-list)
		     ;; try all but the first kind of coin.
		     ;; the preconditions eliminate this branch with (null coin-list)
		       (rec n (cdr coin-list)))))))
    (if (<= amount 0)
	0
	(rec amount coin-list))))
