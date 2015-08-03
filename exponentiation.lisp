;;recursive process
(defun expt-rec (b n)
  (if (= n 0)
      1
      (* b (expt-rec b (1- n)))))

;;iterative process
(defun expt-iterative (b n product)
  (if (= n 0)
      product
      (expt-iterative b (1- n) (* product b))))

(defun expt-iter (b n)
  (expt-iterative b n 1))

;;exponentiation with theta(log n) order of growth
;;beats above functions in this test: (time (* (expt-log 1000000000 100000) 0))
;;but primitive function EXPT takes 0 seconds, and 0 bytes consed...
(defun expt-log (b n)
  (cond
    ((= n 0) 1)
    ((= n 1) b)				;necessary?
    ((evenp n) (expt-log (* b b) (/ n 2)))
    (t (* b (expt-log b (1- n))))))

;;theta(log n) but generates an iterative process
;;This iterative algorithm is ancient! It appears in the /Chandah-sutra/ by
;;"Acharya Pingala", written before 200 B.C.!! See Knuth 1981, section
;;4.6.3. for a full discussion and analysis of this and other methods of
;;exponentiation!!
(defun expt-log-iter (b n a)
  (cond ((= n 0)
	 a) ;initial value 1
	((evenp n)
	 (expt-log-iter (* b b) (/ n 2) a))
	(t
	 (expt-log-iter b (1- n) (* a b)))))

(defun expt-iter-proc (b n)
  (expt-log-iter b n 1))

;;;logarithmic number of step multiplication procedure
;;sicp example
(defun *new (a b)
  (if (= b 0)
      0
      (+ a (*new a (- b 1)))))

(defun m-double (a)
  (+ a a))

(defun halve (a)
  (/ a 2))

(defparameter *x* 0) ;just to test the number of steps *-NEW is called

;;my solution: recursive process
(defun *-new (a b)
  (incf *x* 1)
  (cond ((= a 0) 0)
	((evenp a)
	 (*-new (halve a) (m-double b)))
	(T ;a is not even
	 (+ b (*-new (1- a) b)))))
	 

;;1.18 iterative process
(defun *-new-iter (a b accu)
  (incf *x* 1)
  (cond ((= a 0) accu)
	((evenp a)
	 (*-new-iter (halve a) (m-double b) accu))
	(T
	 (*-new-iter (1- a) b (+ b accu)))))
	 
(defun *-iter (a b)
  (*-new-iter a b 0))

;;;for exercise 1.9 quick fib-iter reimplementation:
(defun fib-iter (a b n)
  (if (= n 0)
      a
      (fib-iter b (+ a b) (1- n))))
;;state transformations:
; a <- b
; b <- a + b
(defun fib (n)
  (fib-iter 0 1 n))


;;;exercise 1.19
(defun fib-iter-1.19 (a b p q count)
  (cond ((= count 0) b)
	((evenp count)
	 (fib-iter-1.19 a
			b
			(+ (* q q) (* p p))        ; p 
			(+ (* 2 (* p q)) (* q q))  ; q
			(/ count 2)))
	(T ;else
	 (fib-iter-1.19 (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))


(defun fib-1.19 (n)
  (fib-iter-1.19 1 0 0 1 n))
