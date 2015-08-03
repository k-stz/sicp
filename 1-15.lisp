;;;;exercise 1.5 of SICP
(defparameter n 0)

(defun cube (x)
  (* x x x))

(defun p (x) (- (* 3 x)
		(* 4 (cube x))))

(defun sine (angle)
  (incf n 1)
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
;;increse in resources needed, hence: logarithmic order of growth!
