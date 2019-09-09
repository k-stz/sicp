
;; exercise 3.1
(defun make-accumulator (start-amount)
  (lambda (x)
    (incf start-amount x)))
;; usage:
;; (defparameter *a* (make-accumulator 10))
;; (funcall *a* 30)


;; Monte Carlo Method:
;;
;; By choosing a sample of experiments at random from a larger set of experiments, we can
;; use the estimates to make deductions about the whole set.


;; For example:
;;
;; approximating π by using the fact that the probability that two integers chosen at
;; at random would will have no factors in common is 6/π².
;; meaning that (gcd (random int-x1) (random int-x2)) will be false 6/π² percent of the
;; time


(defun estimate-pi (n-trials)
  "Using Monthe Carlo method, by performing an experiment `n-trials' of times to estiamte
Pi."
  (let ((no-common-factors 0)
	(6-pi-squared))
    (setf 6-pi-squared
	  (/
	   (loop for i from 0 upto n-trials
	      :when (= (gcd (random 1000000) (random 1000000))
		       1)
	      :counting no-common-factors)
	   n-trials))
    ;; extracting pi
    (sqrt (/ 6 6-pi-squared))))
