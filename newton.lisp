(defpackage :newton-sicp (:use :cl))
(in-package :newton-sicp)


(defun avg (x y)
  "Return the average of x and y."
  (/ (+ x y) 2.0))

(defun square (x) (* x x))

(defun newton-special-sqrt (radicand)
  "Use a special case of Newton's method to calculate square roots."
  (let ((initial-guess 1.0))
    (flet ((good-enough? (guess)
	     (< (abs (- (square guess)
			radicand))
		0.001))
	   (improve-guess (guess)
		 (avg guess (/ radicand guess))))
      (labels ((rec (guess)
		 (if (good-enough? guess)
		     guess
		     (rec (improve-guess guess)))))
	(rec initial-guess)))))


;; Some fixed points can be found by simply applying the function repeatedly on its own
;; output. And since we operate on a function we are introduced to higher-order use as
;; means of abstraction:

(defun close-enough? (x y)
  (< (abs (- x y)) 0.001))

(defun fixpoint (fn &optional (guess 1.0))
  (let ((f-of-x (funcall fn guess)))
    (if (close-enough? f-of-x guess)
	f-of-x
	(fixpoint fn f-of-x))))

;; then we learn that we can reformulate the square root search for a fixpoint search:
;; x² = y  ; /y
;; x = y/x ;
;; f(x) = y/x  <- to remove the second variable, we can construct the function

(defun oscillating-fixpoint-sqrt (number)
  (let ((fn (lambda (x)
	      ;; we capture the number from input, effectively building a function
	      ;; suitable for a fixpoint-search
	      (/ number x))))
    (fixpoint fn)))

;; But the above function is an instance where the primitive fixpoint search doesn't work
;; by simple repeated application. The inputs and outputs will will oscillate over the
;; sqrt due to the nature of the function (we say: the function does not _converge_):
;; f (x) = y/x
;; repeated application
;; f (f (x)) ==> y/y/x  ==> y*x/y ==> x
;; adding another call just resets it:
;; f (*) ==> y/x
;; even deeper nesting:
;; y/y/y/y/x => y/y/y*x/y ==> y/y/x ==> y*x/y ==> x

;; We can solve this problem by a neat math trick called average-damping. Average damping
;; averages the input with the result (just like the first newton sqrt procedure did to
;; yield a improve-guess!). We can build an average damp version of any function:

(defun average-damp (fn)
  "Returns an average-damped version of the input function. Expects a single input."
  (lambda (x) (avg x (funcall fn x))))

;; now we can try it again:

(defun fixpoint-sqrt (number)
  (let ((fn (lambda (x)
	      ;; we capture the number from input, effectively building a function
	      ;; suitable for a fixpoint-search (meaning: it will _converge_!)
	      (/ number x))))
    (fixpoint (average-damp fn))))


;; quick excursion on the golden ratio: Φ²= 1 + Φ or by dividing by Φ we get:  Φ = 1/Φ + 1
;; which is suitable for a fixpoint serach that yields the golden ratio:

(defun fixpoint-golden-ratio ()
  "Returns the golden ratio Φ."
  (fixpoint (lambda (x) (+ (/ 1 x) 1))))


;; back to average damping: This is a higher-order function that even returns an operator.
;; Due to cl being a lisp-2, we can't use the return value in function position:
;; Scheme ((average-damp) 10) ==> 55.0
;; CL (funcall (average-damp) 10) ==> 55.0

;; At this point we're supposed to reflect on the first newton method exposed to us: a
;; special case of the newton method (the general case can find roots for any
;; equations). We can immediately see a striking difference: before we averaged y with x/y
;; successively (1) which is just average-damping and the repeated application is (2) just
;; the way the fixpoint seach works!
;; In other words we abstracted a general procedure of averaging input and output values
;; of function (average-damp) and a general procedure for repeated application, with
;; the specialized goal of converging to a fixpoint (fixpoint-search)
;; (fixpoint (average-damp (lambda (x) (/ 2.0 x)))) ==> 1.4142135

;; finally abstracting these procedures out and exposing them as seperate entities allows
;; for simple reuse: we now can easily compute cube-roots:

(defun cube-root  (number)
  "Fixpoint search x = y / x²"
  (fixpoint (average-damp
	     (lambda (x) (/ number (square x))))))



;; Now we shall use Newton's general method, which involves derivatives - again, our higher-order
;; abstraction comes in handy, because calculating a derivative needs a procedure that takes
;; a function as input and outputs a function such that x->x^3  => x->3x²

;; Computing the derivative of function g(x) where dx is "a small number":
;; Dg(x) = g(x + dx) - g(x)
;;        -----------------
;;                dx

;; The small number dx shall be
(defparameter *dx* 0.00001)

(defun deriv (g)
  "Returns the derivate of input function g."
  (lambda (x) (/ (- (funcall g (+ x *dx*))
		    (funcall g x))
		 *dx*)))


;; (deriv (lambda (x) (expt x 3))) ==> 3x²
;; (funcall * 2) ==> 12.016296, indeed 3*2² = 12!


(defun cube (x) (* x x x))

;; (funcall (deriv #'cube) 5) ==> 75.531006

;; Newton's method
;; if g(x) is a differentiable then a solution to the equation g(x) = 0 is a fixed
;; point of the function f(x) where
;; f(x) = x - g(x)/Dg(x)

;; we can formulate this function easily now:

(defun newton-transform (g)
  "Returns a function whose fixpoint is the solution is zero/root of g. Such that:
(fixpoint (newton-transform g)) satisfies g(x) = 0 "
  (lambda (x) (- x (/ (funcall g x)
		      (funcall (deriv g) x)))))

;; So if we now find the fixpoint of the above returned function, we get the solution
;; to g(x)=0. So let's use this to formulate the sqrt. First to make the g(x)=0 solution
;; equal to the square root:
;; x² = y     ; -x²
;; 0 = y - x²  <- this be our g(x), where y = the sqrt radicand

;; (newton-transform (lambda (x) (- number (square x)))) ==> f (x)
;; hence (fixpoint f(x)) ==> will be our solution to g(x) = 0, ergo: sqrt(y)!!

(defun newton-general-sqrt (number)
  "Use Newton's method to compute square roots!"
  (fixpoint (newton-transform
	     (lambda (x) (- number (square x))))))


;; Now we have two fixpoint searches that compute the sqrt. A general procedure can
;; abstract them both:

(defun fixpoint-of-transform (g transform &optional (guess 1.0))
  (fixpoint (funcall transform g) guess))

(defun fixpoint-avg-damp-sqrt (number)
  (fixpoint-of-transform (lambda (x) (/ number x))
			 #'average-damp
			 1.0))

(defun fixpoint-newton-transform-sqrt (number)
  (fixpoint-of-transform (lambda (x) (- number (square x)))
			 #'newton-transform
			 1.0))
