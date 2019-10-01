
(defun square (x)
  (* x x))

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

(defun monte-carlo (trials experiment-fn)
  "doc"
  (labels ((iter (trials-remaining trials-passed)
	     (cond ((= trials-remaining 0)
		    (/ trials-passed trials))
		   ((funcall experiment-fn)
		    (iter (1- trials-remaining) (1+ trials-passed)))
		   (t
		    (iter (1- trials-remaining) trials-passed)))))
    (iter trials 0)))

(defun cesaro-test ()
  (= (gcd (random 1000000) (random 1000000)) 1))

(defun estimate-pi (n-trials)
  (sqrt
   (/ 6
      (monte-carlo n-trials #'cesaro-test))))


;; Exercise 3.5 - Monte Carlo integration
;;
;; estimating definite integrals by means of Monte Carlo simulation
;;
;; let P(x,y) for point (x,y) that is in a region, else false
;;
;; e.g. the circle of radius 3 at (5, 7) where P(x,y) would be
;; (x-5)²+(y-7)²≤3²
;; To estimate the area of the region using the predicate P we can use
;; a rectangle that contains partially the region. 
;;
;; As an example we choose a rectangle with the diagonally opposite
;; corners (2,4) and (8,10), where "diagonally opposite" means
;; the two points are diagonally opposing corners of the rectangle.
;;
;; Now we want to compute the integral of the area of the triangle
;; that lies in the circle.
;;
;; Now to the monte carlo trick: We can pick random points (x,y
;; _that lie in the rectangle_ and tsting P(x,y) for each poit to
;; determine whether the points lies in the region.
;;
;; If we try this many times then the fraction of the points that fall
;; in the region should give the proportion of the rectangle that
;; lies in the region. Hence multiplying this with the area of the
;; entire rectangle should yield the integral.
;;
;; Given this description calculate the are of a unit-circle to
;; estimate π!

;; First lets get the random range from a rectangle
;; For this we need to choose values from x-axis constrains and y-axis costrains.
;; So first we define for 1 dimension:

(defun random-in-range (low high)
  (let ((range (- high low)))
    (+ low (random (float range)))))

(defun circle-area (radius)
  ;; for sanity checks
  (* pi (expt radius 2)))

(defun rectangle-area (rec-x1 rec-x2 rec-y1 rec-y2)
  (* (abs (- rec-x1 rec-x2))
     (abs (- rec-y1 rec-y2))))

(defun gen-circle-predicate-fn (circle-x circle-y radius)
  (lambda (x y)
    (<= (+ (square (- x circle-x))
	   (square (- y circle-y)))
	(square radius))))

(defun estimate-integral (predicate-fn rec-x1 rec-x2 rec-y1 rec-y2 n-trials)
  (let ((rectangle-area (rectangle-area rec-x1 rec-x2 rec-y1 rec-y2 )))
    (labels ((experiment-fn ()
	       (funcall predicate-fn
			(random-in-range rec-x1 rec-x2)
			(random-in-range rec-y1 rec-y2))))
      (float
       (*
	rectangle-area
	(monte-carlo n-trials
		     #'experiment-fn))))))


;; now because the area of a rectangle is π*r² given r=1, a unit-circle
;; has the are of exactly π. Now we choose any rectangle that contains
;; a unit circle and we get pi

(defun estimate-pi-integral (n-trials)
  ;; we triavally describe a unit-circle that is definitely fully contained by
  ;; a rectangle
  (estimate-integral
   (gen-circle-predicate-fn 0 0 1) ;; unit-circle at origin
   -2 2 -2 2			   ;; rectangle diagonally opposite points
   n-trials))			   ;; trials
   

;; exercise 3.8
;; depending on order of evaluation the expression (+ (f 0) (f 1))
;; shall return:
;; 0 for left-to-right
;; 1 for right-to-left

(defparameter *f-global* -1)

(defun f (n)
  (if (= *f-global* -1)
      (progn (setf *f-global* n)
	     0)
      *f-global*))

