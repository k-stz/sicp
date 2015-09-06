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

(defun first-denomination (coins)
  (car coins))

(defun except-first-denomination (coins)
  (cdr coins))

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
		     (rec (- n (first-denomination coin-list)) coin-list)
		     ;; try all but the first kind of coin.
		     ;; the preconditions eliminate this branch with (null coin-list)
		       (rec n (except-first-denomination coin-list)))))))
    (if (<= amount 0)
	0
	(rec amount coin-list))))

;; The order of the coins doesn't affect the result. Our setup creates every combination
;; possible because we either branch of applying a denominiation or we branch of exempting
;; a denomination. Further branches then either operate on but one less denimination AND
;; on all denomination available. Or: the order in which we substract the coins doesn't matter
;; as long as we try all the coins. Example: Given coins 10 and 1 for amount 11 we either try
;; 10 then exempt 10 and then 1 yielding combiniation (10 1) for 11, or we first try 1 then
;; exempt 1 and then 10 also yielding combiniation (1 10) for 11. The order doesn't matter.


;; Exercise 2.20

;; Scheme: (define (foo x . lst) ...)
;; CL    : (defun  foo (x &rest lst) ...)
;;
;; Same is true for lambda function object definition
;; Scheme: ((lambda lst lst) 1 2 3) ==> (1 2 3)
;; CL    : ((lambda (&rest lst) lst) 1 2 3) ==> (1 2 3)
;;
;; Scheme: ((lambda (x . lst) (list 77 3 2 1))) ==> (77 (3 2 1))
;; CL    : ((lambda (x &rest lst) (list x lst)) 77 1 2 3) ==> (77 (1 2 3))

;; "parity" property of intergers to be either even or odd, "the parity of 2 is even" and
;; "n+1 has always the opposite parity of n if n is an integer"

(defun same-parity (&rest list)
  "Return a list of numbers that have the same parity as the first argument"
  (flet ((collect-odds ()
	   (loop for i in list
	      :when (oddp i)
	      :collect i))
	 (collect-evens ()
	   (loop for i in list
	      :when (evenp i)
	      :collect i)))
    (if (oddp (first list))
	(collect-odds)
	(collect-evens))))


;; no need to build the list from cdr to car and then m-reverse..
(defun 1-scale-list (list factor)
  "(cl:mapcar (lambda (x) (* x factor)) list)"
  (labels ((rec (cdr-list output-list)
	      (if (null cdr-list)
		  (m-reverse output-list)
		  (rec
		   (cdr cdr-list)
		   (cons (* factor (car cdr-list))
			 output-list)))))
    (let ((init-list '()))
      (rec list init-list))))


;; we can build the list from car to cdr recursively!
(defun 2-scale-list (list factor)
  "(cl:mapcar (lambda (x) (* x factor)) list)"
  (if (null list)
      '()
      (cons (* (car list) factor)
	    (2-scale-list (cdr list) factor))))


(defun m-map (fn list)
  "(cl:mapcar #'fn list)"
  (if (null list)
      '()
      (cons (funcall fn (car list))
	    (m-map fn (cdr list)))))

;; (m-map #'abs (list -10 2.5 -11.6 17)) ==> (10 2.5 11.6 17)

;; using M-MAP for the implementation
(defun 3-scale-list (list factor)
  (m-map (lambda (x) (* x factor))
	 list))


;; Exercise 2.21

(defun square (x) (* x x))

(defun 1-square-list (list)
  (if (null list)
      nil
      (cons (square (car list))
	    (1-square-list (cdr list)))))

(defun 2-square-list (list)
  (m-map #'square
	 list))


;; Exercise 2.22

;; An expression like
;;  (cons (square (car things))
;;        answer)
;; adds a new head to the list like so
;; (cons 'first nil) => (cons '2nd (cons 'first nil)) => (2nd first)

;; Interchanging the arguments:
;; (cons 'answer
;;       (cons 'square-etc))

;; puts the accumulator <answer> in the head and "replaces" the tail with the current
;; square.  Thereby violating the list property of a nil-terminator

;; (defun bad-square-list (items)
;;   (labels ((iter (things answer)
;; 		  (if (null things)
;; 		      answer
;; 		      (iter (cdr things)
;; 			    (cons answer
;; 				  (square (car things)))))))
;;     (iter items nil)))
   
;; (bad-square-list (list 1 2 3 4)) ==> ((((NIL . 1) . 4) . 9) . 16)


;; Exercise 2.23 - FOR-EACH

(defun for-each (fn list)
  (if (null list)
      nil
      (progn (funcall fn (car list))
	     (for-each fn (cdr list)))))


;;------------------------------------------------------------------------------

;; Trees and tree-recursiveness

(defun m-length (list)
  "CL:LENGTH"
  (labels ((rec (length list)
	     (if (null list)
		 length
		 (rec (1+ length)
		      (cdr list)))))
    (rec 0 list)))


(defun count-leaves (list)
  "Returns number of leaves in input list (tree"
  (cond ((null list)
	 0)
	((atom list) 1) ;; ATOM is equivalent to (not (consp x))
	(t ;; else
	 (+ (count-leaves (car list))
	    (count-leaves (cdr list))))))
