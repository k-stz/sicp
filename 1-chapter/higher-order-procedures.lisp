(defun sum (a b term next) 
  (if (> a b)
      0
      (+ (funcall term a)
	 (sum
	  (funcall next a)
	  b term next))))


(defun integral (a b f dx)
  (labels ((add-dx (x)
	     (+ dx x)))
    (* dx
       (sum (+ a (/ dx 2.0))
	    b
	    f
	    #'add-dx))))

(defun integral-iter (a b f dx accu)
  (if (> a b) ;has to terminate one step sooner
      accu
      (integral-iter (+ a dx)
		      b f dx ;constants
		      (+ (funcall f a) accu))))

;;generating an iterative process, works fine now!
(defun integral-i (a b f dx)
  (let ((init-a (+ a (/ dx 2.0))))
    (* dx
       (integral-iter init-a
		      b f dx ; constants
		      (funcall f init-a)))))

(defun cube (x)
  (* x x x))


;;exercise 1.29 Simpson's Rule (a more accurate numerical integration than used
;;in the functions above)
;;;The following was the first attempt but it doesn't _apply_ higher-order
;;procedures and therefore it shouldn't be considered the right solution!
(defun simpson-integration-1 (a b f n)	;first failed attempt
  (let ((h (/ (- b a) n)))
    (* (/ h 3)
       (+ 
	(loop for i from 1 to (- n 1) summing
	   ;;most blatant bug ever encountered:
	   ;;was:       f(* if(2,4) (a + k*h))
	   ;;should be:    if(2,4) * f(a+k*h) 
	     (*
	      (funcall f (+ a (* i h)))
	      (if (evenp i) 2 4)))
	;;adding the k=0 and k=n cases:
	;;k=0
	(funcall f a)			;(a+0*h)
	;;k=n
	(funcall f (+ a (* n h)))))))

;;utilizing the higher order procedure SUM
(defun simpson-integration (a b f n)
  (let ((h (/ (- b a) n)))
    (labels ((term (k)
	       (cond ((or (= k 0) (= k n)) ;TODO switch with 2nd clause-> more
					;efficient?
		      (funcall f (+ a (* k h))))
		     ((oddp k)
		      (* 4
			 (funcall f (+ a (* k h)))))
		     ((evenp k)
		      (* 2
			 (funcall f (+ a (* k h))))))))
      (* (/ h 3)
	 (sum 0 n #'term #'1+)))))
	    
       

;CL-USER> (simpson-integration 0 1 #'cube 3) ==>    47/243
;CL-USER> (simpson-integration-1 0 1 #'cube 3) ==> 155/243
;yk = f(a + kh)

;;exercise 1.30
(defun sum-i (a b term next)
  (labels  ((iter (a result)
    (if (> a b)
        result
        (iter (funcall next a)
	      (+ result
		 (funcall term a))))))
  (iter a 0)))

;;exercise 1.31 -- defining product (as in capital pi notation)
(defun product (a b f next)
  (labels ((iter (a result)
	     (if (> a b)
		 result
		 (iter (funcall next a)
		       (* result (funcall f a))))))
    (iter a 1)))

(defun factorial (n)
  (product 0 n #'identity #'1+))

;;auxiliary function for pi-approx, exposed for testing (sicp exercise 1.31)
(defun next (x) (+ x 2))

;;; DOWN and UP constitute some hacks to implement PI-APPROX without the need of
;;; an auxiliary variable!
(defun up (x)
  (labels ((init-x-1 (x)
	     (if (evenp x)
		 (+ x 2)
		 (1+ x)))
	   (init-x-2 (x)
	     (if (evenp x)
		 x
		 (1+ x))))
    (* 2
       (product 4 (init-x-1 x) #'identity #'next)
       (product 4 (init-x-2 x) #'identity #'next))))

(defun down (x)
  (labels ((init-x-1 (x)
	     (if (evenp x) 
		 (1+ x)
		 (+ x 2)))
	   (init-x-2 (x)
	     (if (evenp x)
		 (1+ x)
		 x)))
    (* (product 3 (init-x-1 x) #'identity #'next)
       (product 3 (init-x-2 x) #'identity #'next))))

;;exercise 1.31
(defun pi-approx (precision)
  (* 4.0
     (/ (up precision)
	(down precision))))

;;exercise 1.31 writing PRODUCT generating a recursive process
(defun prod-rec (a b f next)
  (if (> a b)
      1
      (* (funcall f a)
	 (prod-rec (funcall next a)
		   b f next))))
    
;(2 4 4 6 6)
;(3 3 5 5 7)
;other solutions: mapping the index to the numbers (2 4 4 6 6) by testing (evenp
;x) if even => x+2, not even => x+1    e.g.: x=5 => not even -> x+1 -> 5+1 => 6
;(2 4 4 6 6)
;         ^ this is the 5th index!, so it works!

;; (define (pi n)
;;   (define (pi-term k)
;;     (/ (* (- k 1) (+ k 1)) (square k)))
;;   (define (pi-next k)
;;     (+ k 2))
;;   (* 4 (product pi-term 3 pi-next n)))

(defun square (x) (* x x))

;implementation by "sergeykhenkin"
(defun pi-m (n)
  (labels ((pi-term (k)
	     (/ (* (- k 1) (+ k 1)) (square k)))
	   (pi-next (k)
	     (+ k 2)))
	   (* 4.0 (product 3 n #'pi-term #'pi-next))))

;( 2 4 4 6 6 8)
;( 3 3 5 5 7 7) 
;;   ^ 3-1 * 3+1 =>  2*4
;; devided by 3^2 => 3*3 ... fits perfectly, more efficient than my solution!

;;exercise 1.32
;;(accumulate combiner null-value term a next b),
;;;generates a recursive process
(defun accumulate-r (combiner null-value a b term next)
  (if (> a b)
      null-value
      (funcall combiner
	       (funcall term a)
	       (accumulate combiner null-value
			   (funcall next a)
			   b term next))))

;;ACCUMULATE function generating an iterative process
(defun accumulate-i (combiner null-value a b term next)
  (labels ((iter (a result)
	     (if (> a b)
		 result
		 (iter (funcall next a) ;step through a
		       (funcall combiner
				result
				(funcall term a))))))
    (iter a null-value)))

;;;exercise 1.33, because we need PRIMEP and possible GCD, which are defined in
;;;other *.lisp files
;; TODO: ASDF unify these over a package:
;; (load "gcd.lisp")
;; (load "/primality.lisp")

(defun filtered-accumulate (combiner null-value a b term next test)
  "Accumulate the values yielded through term from a to b by (next a) through
combining them using combine and only doing so if they pass test."
  (labels ((iter (a result)
	     (if (> a b)
		 result
		 (iter (funcall next a)
		       (if (funcall test a)
			 (funcall combiner
				  result
				  (funcall term a))
			 result)))))
    (iter a null-value)))
;;succesful tests:
;> (loop for i in (search-for-primes 1 100) summing i) ==> 23593
;> (filtered-accumulate #'+ 0 1 523 #'identity #'1+ #'primep) ==> 23593

;;now for the sum of all integers less than n that are relatively prime to n,
;;i.e. gcd(i,n) = 1, we need to write a special test function therefore:
(defun relative-primep (i n)
  (if (= (gcd i n) 1)
      T
      NIL))

;;TODO, solve the NEXT SQUARE collisions in the files used by defining a package
;;probably

;> (apply #'+ (loop for i from 1 to 100 when (relative-primep i 100) collect i))
;==> 2000

;;check out the formulation of the LAMBDA!!
;> (filtered-accumulate #'+ 0 1 100 #'identity #'1+ (lambda (x)
;						     (relative-primep x 100)))
;==> 2000
