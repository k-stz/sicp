(in-package :sicp)

;;primality = Primzahl-Eigenschaft
;;composite = the opposite of prime numbers, that is a number that has a
;;            positive divisor other than one or itself. 
(defun dividesp (a b)
  (= (mod b a) 0)) ;note the switched b a, so that we can ask "does a divide b?"
                   ;does 4 divide 2 (dividesp 4 2)=>NIL but (dividesp 2 4)=>T
(defun square (x)
  (* x x))

;;exercise 1.23
(defun next (test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
	 ((dividesp test-divisor n) test-divisor)
	 (T
	  ;(find-divisor n (1+ test-divisor)) ;test every even number-> inefficient
	  (find-divisor n (next test-divisor)) ;improved
	  )))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun primep (n)
    (= n (smallest-divisor n)))

;; Why is it that if we haven't found an integer divisor smaller or equal to the
;; sqrt(n), that we won't find one bigger than sqrt(n)?
;; It is because sqrt(n) fits n-times into n! Any number bigger than sqrt(n) can
;; only fit less times then sqrt(n) into n. Corollary, there must be a divisor
;; smaller than sqrt(n) if we find a divisor bigger than sqrt(n).
;; The above concerns the realm of natural numbers, we're asking why the
;; end-test (> (square test-divisor) n) is legit! It's a valuable observation
;; that yields us Theta(sqrt(n)) Order of growth!!!
;;The Carmichael numbers fail the Farmat test, those are extremly rare numbers
;; that are not prime but pass the test anyway example numbers: 561, 1105
;;;The FERMAT-TEST procedure effectively implement exercise 1.27 as well!
(defun fermat-test (prime) ;example primes: 149,107,61,83
   (when (not (primep prime))
     (format t "~a is not a prime number" prime))
   (loop for i from 1 below prime ; i < prime!
	collecting (= i   ;(mod i prime) well i < prime so (mod i prime) => i
		      (mod (expt i prime) prime))))

;(fermat-test 561) => 561 is a Carmichael Number!!!

;; Fermat's Little Therom is quite remarkable, if we take any integer smaller
;; than a prime and raise it to the power of the prime, then modulo it by the
;; prime, we end up with the integer we've taken to begin with!  Example: Prime:
;; 17 We choose: 7
;; 7^17 = 232630513987207
;; And that number modulo our prime 17:
;; 232630513987207 % 17= 7 <- yiels the number!!!

;; a < n, n is prime then a^n % n => a

;; For non prime numbers, this relation rarely holds true and extremly rarly holds
;; true for every integer below the prime. Those exception, around 255 below
;; 100.000.000, are called the Carmichael numbers!!!

;;  number-smaller-then-prime^prime modulo prime= number-smallen-then-prime


;;; SICP's EXPMOD implementation:
;; The MOD in the EVENP clause, is an efficiency born from the fact that (mod z m)
;; yields the same result for z = x*y -> (* (mod x m) (mod y m)) ==> RESULT
;; (= (mod RESULT m) (mod z m)) ==> T
;; That's why it also works for the ELSE clause, T, where (mod (* base (expmod ..)) m).
;; Because  2*2^7-1 = z --> ((2 % m) * (2^7-1 % m)) % m = z % m!!!
;; TEST PROOF:   m = 5           maps:
;; > (expt 2 (- 7 1)) ==> 64     x
;; > (mod 64 5) ==> 4            x % m
;; > (mod 2 5) ==> 2             y % m
;; > (* 4 2) ==> 8               (x % m)*(y % m)
;; > (mod 8 5) ==> 3             ((x % m)*(y % m)) % m -,
;; > (expt 2 7) ==> 128          z			|-> same result [] Q.E.D. lol
;; > (mod 128 5) ==> 3           z % m		       -'
(defun expmod (base exp mod-arg)
  (cond ((= exp 0) 1)
	((evenp exp)
	 (mod (square (expmod base (/ exp 2) mod-arg)) ;this MOD is efficient
	      mod-arg))
        (T
	 (mod (* base (expmod base (1- exp) mod-arg))
		 mod-arg))))

(defun fermat-test-2 (prime)
  (loop for i from 1 below prime collecting
       (= (expmod i prime prime)
	  i)))

(defun fermat-test-m (n)
  (flet ((try-it (a)
	   (= (expmod a n n)
	      a)))
  (try-it (random n))))

(defun fast-primep (n times)
  "fastest primality test in this file"
  (cond ((= times 0) T)
	((fermat-test-m n)
	 (fast-primep n (1- times)))
	(T ;else false
	 NIL)))
	   
;;exercise 1.21
;> (smallest-divisor 199) ==> 199 ;prime number
;> (smallest-divisor 1999) ==> 1999 ;prime number
;> (smallest-divisor 19999) ==> 7 

;;exercise 1.22:
;find the first three smallest primes larger than 1k 10k 100k 1000k, write a
;function to measure the time and see to it that theata(sqrt(n)) is met->finding
;10k as opposed to 1k should take sqrt(1k) sqrt(10k) time difference=> roughly
;3-tim(es longer
(defun search-for-primes (from find-this-many)
  (let ((primes '()))
    (labels ((find-primes (n find-this-many)
	     (if (= find-this-many 0)
		 primes
		 (progn (when (primep n)
			  (push n primes)
			  (decf find-this-many))
			(find-primes (1+ n) find-this-many)))))
      (find-primes from find-this-many))
    primes))

		   
    
;;;find the first 3 primes: a thousand times (modern computer's are too fast)
;; 1k   : 0.010 sec
;; 10k  : 0.040 sec  4 times more
;; 100k : 0.095 sec  2.3 times more
;; 1kk  : 0.210 sec  2.2 times more
;; 10kk : 0.866 sec  4.1 times

;;Should actually be close to theta(sqrt(n)) but atleast it approaches it quite
;;nicely as is the essence of order of growth

;;            Factor 10:                                ratio/expected ratio
;; 0.065 for  1000000000000    sqrt: 1000000.0          
;; 0.188 for  10000000000000   sqrt: 3162277.8          2.8923078/3.162277
;; 0.589 for  100000000000000  sqrt: 10000000           3.1329787/3.1622775
;; 2.735 for  1000000000000000 sqrt: 3.1622776e7        4.6434636/3.1622777
;; 9.565                                                3.4972577
;; 18.572                                               1.9416624
;well good enough except the last but the discrepancy is
;compiler/implementatnion/machine dependent large may behave differently from
;small ones
;;It is vital to observe that as we increse the input by a factor of 10, the
;; time needed increases only be a factor of roughly sqrt(10) as is the essence
;; of an order of growth theta(sqrt(n)) !!!!!!!!!!!

;;exercise 1.24
;modify the search-for-primes to use the Fermat method FAST-PRIMEP to search for
;prime, the probablistic algorithm with order of growth theta(log n) to search
;for the closest primes to 1k 10k 100k 1kk, trying to predict the time needed.
;Prediction: As theta(sqrt(n)) having the factor of sqrt(10) between the values
;for 1k -> sqrt(1k) to 10k = 1k*1k or sqrt(10)*sqrt(10)
;;But here comes the great property of the logarithm, a log(10) to log(100),
;raises the R(n) required by a constant factor namely: log(100) = log(10) +
;log(10)!!!
;==> 1kk will only need four times the resources needed by 1k!!!
(defun primes-de-fermat (from find-this-many)
  (let ((primes '()))
    (labels ((find-primes (n find-this-many)
	       (if (= find-this-many 0)
		   primes
		   (progn (when (fast-primep n 4) ;<- hardcoded "times" fermat-test!
			    (push n primes)
			    (decf find-this-many))
			  ;; (1+ n) should be (+ n 2) along the uneven numbers!
			  (find-primes (1+ n) find-this-many)))))
    (find-primes from find-this-many))
  primes))
;;OMG this algorithm kicks ass big time, it can find primes in a fraction of a
;second where the theta(sqrt(n)) order of growth needs hours!!!


;;exercise 1.25
;implementing the expmod using the exponentiation first and then just modulo the
;result is less efficient then using the (mod (* (mod x m) (mod y m)) m) trick
;to keep the numbers small. All explanation point to the fact that applying the
;MOD function to a huge number yielded by exponentiation can be very inefficient
;if we deal with numbers that can't fit into the machine registers and are thus
;computed using arbitrary-precision arithmetic (e.g. bignum arithmetic) which is
;much more inefficient that resolving a chain of deferred procedures 
; (mod (square (mod (base (mod square ...)))))
; Arbitrary-precision-arithmetic is implemented in software while the numbers
;that fit entirely in a processors register are implemented in hardware
;arithmetic which makes the difference.


;;exercise 1.26
;; (square 2^4)
;; (square (square 2^2))
;; (square (square (square 2)))

;; 2^8
;; (* 2^4 2^4)
;; (* (* 2^2 2^2) (* 2^2 2^2))
;; (* (* (* 2 2) (* 2 2)) (* (* 2 2) (* 2 2)))
;In Louis Reasoner's EXPMOD instead of just halving the exponent creating a
;chain of deferred SQUARE procedures, we half the exponent _and_ doulbe the
;amount of numbers we're concerned with at every step, yielding the same amount
;of numbers as the exponent N. We apply the (MOD (SQUARE ...) m) nesting N times
;8 -> 4 4 -> 2 2 2 2 -> 1 1 1 1 1 1 1 1
;Whereas in the theta(log n) EXPMOD, we half the exponent and just apply the
;(MOD (SQUARE ..)) nesting on the result:
;8 -> (MOD (SQUARE 4) m) -> (MOD (SQUARE (MOD (SQUARE 2) m)) m)
;Or to use the knowledge of SICP: Louis' EXPMOD generates a tree-recursive
;process instead of a linear-recursive
;;halving the exponent but doubling the numbers concerned ~> log 2^n ~> theta(n)

;;exercise 1.28 miller-rabin test implementation
;if n is prime then a an ineger is a<n and a^(n-1)st power is congruent 1 % n.

(defun square-check (x n-1)
  (if (and (not ;beginners trap (or (not ) (not )) != (not (or .. ..))
	        ;and OR in programming is _NOT "or"_ as used colloquially
	    (or (= x 1)  
		(= x n-1)))
	   (= (mod (* x x) (1+ n-1)) 1))
      0
      (* x x)))

(defun expmod-m (base exp mod-arg)
  (cond ((= exp 0) 1)
	((evenp exp)
	 (mod (square-check (expmod-m base (/ exp 2) mod-arg) (1- mod-arg))
	      mod-arg))
	(T
	 (mod (* base (expmod-m base (1- exp) mod-arg))
	      mod-arg))))

(defun miller-rabin (n)
  "Primality test using the Miller-Rabin test."
    (loop for a from 1 below n always   ; a < n, 'ALWAYS' keyword very useful also 'NEVER'!!
	 (= (expmod-m a (1- n) n) (mod 1 n))))
