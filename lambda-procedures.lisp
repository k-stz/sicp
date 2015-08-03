(defpackage :sicp (:use :cl))
(in-package :sicp)

;;; scheme's (define foo (lambda (x) (+ x 4)))
;;; in CL:
;;; (defun foo () (lambda (x) (+ 4 x)))
;;; usage: (funcall (foo) 2) ==> 6
;;; OR: (defparameter foo (lambda (x) (+ 4 x)))
;;; (funcall foo 4) ==> 8

;;; Scheme: ((lambda (x y z) (+ x y z)) 1 2 3)
;;; CL
;;; (funcall (lambda (x y z) (+ x y z)) 1 2 3) ==> 6
;;; or
;;; (apply (lambda (x y z) (+ x y z)) '(1 2 3)) ==> 6

;;; SICP explains LET as follows:
;;; (let ((var1 exp1) ... (var-n exp-n)) <body)
;;; is a alternative syntax for:
;;; (funcall (lambda (var1 ... var-n) <body>)
;;;          (exp-1
;;;           ...
;;;           exp-n))

;;; i.e. the LET expression is syntactic sugar for the underlying
;;;lambda-expression.
;;;
;;; Note the implication of this:
;;; (defparameter x 3):
;;;> (let ((x 3)
;;;	       (y (+ x 2)))
;;;	   (* y x))
;;;==>12
;;;> (funcall (lambda (x y) (* y x)) 3 (+ x 2))
;;;==>12                                  ^ doesn't depend on the "3" bound to
;;;                                         LAMBDA'S x
;;;
;;;Note the LET*
;;; > (let* ((x 3)
;;; 	       (y (+ x 2)))
;;; 	   (* y x))
;;; ==>15

;;;;exercise 1.34
;;;; 
(defun f (g) (funcall g 2))
;;;; doing (f #'f) -> (funcall #'f #'f) -> (funcall #'f 2) -> (funcall 2 2)
;;;; -> type-error, 2 is not a function!


;;;; "zero of a function" is any value of x "such that" f(x)=0
;;;; "a fixpoint of a function" is any value of x "such that" f(x)=x!!! EASY!

;;;; "Procedures as general methods" -> If you think you understand what a
;;;; procedure is, and what a process is, than try to fit the word "Method" into
;;;; there.
;;;; Goal: What is a "general method"???
;;;; Some math:
;;;; fix point = Fix Punkt  x such that f(x)=x
;;;; zero or root of a function = Nullstelle  x such that f(x)=0

;;;; continuous function = stetige Function
;;;; Small changes in the input (parameter) cause small changes in the output 



;;;Half-interval method for finding zeros in a function
(defun f (x)
  "Test function to be used in our zero finding function"
  (flet ((^ (exp power) ;;check out this cool function name
	   (expt exp power)))
  (- (^ x 2) -1.5)))

;;;;The following functions where a semantically flawed attempt at searching for
;; zero's as it utilized f(x)=x^2, which didn't satisfy the premise of a,b such
;; that there exists f(a) < 0 < f(b) at any time, as x^2 can never be negative
;; and even pulling it below the x-axis (x^2 - 1) created left and right of the
;; zero intervals of negative points, which fooled my basic find-a find-b
;; functions already.
;; (defun average (x y)
;;   (/ (+ x y) 2.0))
;;;
;; (defun find-zero (function)
;;   (labels ((find-a (x)
;; 	     (if (< 0 (funcall function x))
;; 		 x
;; 		 (find-a (1- x))))
;; 	   (find-b (x)
;; 	     (if (> 0 (funcall function x))
;; 		 x
;; 		 (find-b (1+ x))))
;; 	   (good-enoughp (x)
;; 	     (< (abs (funcall #'f x)) 0.1))
;; 	   (improve (a b)
;; 	     (let ((avg (average a b)))
;; 	       (print-a-b-avg a b avg)
;; 	       (if (good-enoughp avg)
;; 		   avg			;  f(avg)= 0 (about
;; 		   (if (> (funcall #'f avg) 0)
;; 		       (improve a avg)
;; 		       (improve avg b)))))
;; 	   (print-a-b-avg (a b avg)
;; 	     (format t "a:~A b:~A avg:~A~%" a b avg)))
;;     ;;f(a) < 0; f(b) > 0
;;     (let* ((a (find-a 0))
;; 	   (b (find-b (1+ a)))
;; 	   (zero))
;;       (setf zero (improve a b))
;;       (format t "zero-x: ~A actual zero: ~A" zero (funcall #'f zero)))))

;;;;find fixed points of a function using the f(x), f(f(x)), approximation
;;;;method

;;; This only works on _some_ functions!
(defun fix-point (function guess)
  (flet ((good-enoughp (function guess)
	   (< (abs (- (funcall function guess) guess))
	      0.0001)))
    ;;FIX-POINT:
    (if (good-enoughp function guess)
	guess
	(progn (format t "~A " guess)
	       (fix-point function
			  (funcall function guess))))))

;;;y^2=x -> y= x/y -> the fixpoint of f(y)=x/y would be the sqrt(x)!
; (defun sqrt (x)
;   (fix-point #'(lambda (y) (/ x y))
;              1.0))
;;unfortunately we get an infinite loop because, e.g. x is 2.0:
;; 2/y1 = y2, y3= 2/(2/y1) <- reciprocate so 2/(2/y1) always yields y1!
;; Hence, it will jump betwen y1 and y2:
;; again, y1=1 y2=2 because x=2 and x/y1 -> 2/1=2 (y2)

;; 2/1=2 -> 2/2=1 -> 2/1=2 infinite loop
;; 2/1=2 -> 2/(2/1)-> 1 -> 2/(2/(2/1))  -> 2

;;;But there is a way out, and it is so very novel way of looking at mathematic
;;;equations to me, y->x/y causes way to big changes, but slightly modifying the
;;;equations yields: y->(1/2)(y + x/y)
(defun sqrt-m (x)
  (fix-point #'(lambda (y) (* 1/2 (+ y (/ x y))))
	     1.0))

;;exercise 1.35
;;;phi^2=phi+1 -> *divide by phi* -> phi = 1 + 1/phi 
(defun phi ()
  (fix-point #'(lambda (x) (+ 1 (/ 1 x)))
	     1.0))

;;; exercise 1.36
;;; Solution to x^x=1000
;;; x^x=1000 |
;;; SOLUTION: Log's use is not very intimate as one might assume, it's just a
;;; the advantageous rules: log(x^x) = x*log(x) !! hence x = log(1000)/log(x)!!!


(defun e-approx (precision) ;works with "10000" but greater than that fails
  (expt (+ 1 (/ 1.0 precision))
	precision))

;; 10kg of radiactive material decaying at 100% a year, how much is left after 3
;; years? (* 10 (exp (* 3 (- 1)))) ==> 0.49787068 kg!!! 10*e^-1*3
;; (-1 = 100% decay!), 3 is for 3 years, and 10 are the 10 kg

;;how long does it take to grow double the amount? Easy: (* amount (exp (log 2)))!
;;; x -> log(1000)/log(x)
(defun f-1 (x)
  (/ (log 1000) (log x)))

;;; f-1 using average damping! -- WORKS
(defun f-avg-damping (x)
  (* 1/2 (+ x (f-1 x))))
