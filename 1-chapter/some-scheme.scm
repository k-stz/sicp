 (define (abs x)
   (if (< x 0)
       (- x)
       x))

(define drei 3)
(define (trueif1 x)
  (if (= x 1)
      (t)
      f))
(define aux 1)
(define (testloop x)
  (if (< x 10)
      (display (testloop (+ x 1)))
      (display "loop terminated")))

(define (exercise1.3 a b c)
   (cond ((and (< a b) (< a c)) (+ (* b b) (* c c))) 
	 ((and (< b a) (< b c)) (+ (* a a) (* c c))) 
	 ((and (< c b) (< c a)) (+ (* b b) (* a a)))))

; give radicand and guess
; check if (
(define (sqrt-newton radicand guess)
  (if (good-enough? radicand guess)
      (* 1.0 guess) ; make it decimal
      (sqrt-newton radicand (better-guess radicand guess))))

(define (good-enough? radicand guess)
  (< (abs (- (square guess) radicand)) (* 0.001 guess)))

(define (better-guess radicand guess) 
  (average guess (/ radicand guess)))

(define (average x y) (/ (+ x y) 2.0))

;exercise 1.6
(define (sqrt-with-cond radicand guess) 
  (cond ((good-enough? radicand guess) (* 1.0 guess)) 
	(else (sqrt-with-cond radicand (better-guess radicand guess)))))

(define (sqrt-new-if radicand guess) 
  (new-if (good-enough? radicand guess) 
	  (* 1.0 guess) 
	  (sqrt-new-if radicand (better-guess radicand guess))))

;;TODO EXERCISE 1.6!

(define (sqrt-end-test radicand guess)
  (if (good-enough? radicand guess)
      (* 1.0 guess)
      (if (< (abs (- (better-guess radicand guess) guess)) (/ guess 1000000))
	  (display "(better-guess - guess) < (guess / 1000000)")
	  (sqrt-end-test radicand (better-guess radicand guess)))))
;exercise 1.8
(define (cube-root radicand guess)
  (if (good-enough-cube? radicand guess)
      (* 1.0 guess)
      (if (< (abs (- (better-cube-guess radicand guess) guess)) (* guess 0.001))
	  (display "(- better-guess-cube guess) < (* guess 0.00001)")
	  (cube-root radicand (better-cube-guess radicand guess)))))

(define (good-enough-cube? radicand guess)
  (< (abs (- (* guess guess guess) radicand)) (* 0.001 guess)))

(define (better-cube-guess radicand guess)
  (/ (+ (/ radicand (square guess)) (* 2 guess))
     3))
; Block Structure test
(define (sqrt-with-blockstructure x guess)
  (define (improve guess) (average guess (/ x guess)))
  (define (average a b) (/ (+ a b) 2))
  (define (good-enough? guess) (< (abs (- (square guess) x)) (* guess 0.00001)))
  (if (good-enough? guess)
      (* 1.0 guess)
      (sqrt-with-blockstructure x (improve guess))))

; factorial
(define (factorial x)
  (define (factorial-iter x n)
  ;(define n 0)
  (if (= x 0)
      1
      (cond ((= x 1) (* n x))
	    (else (factorial-iter (- x 1) (* n (- x 1)))))))
  ;factorial body
  (factorial-iter x x))

(define (factorial-sicp n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;sum funciton, test recursion as iteration implementation
(define (sum start end)
  (define (function start)
    start)
  (define (sum-iter start)
    (if (> start end)
	0
	(+ (function start) (sum-iter (+ start 1)))))
  ; sum body
  (sum-iter start))
;alternativ factorial
(define (alt-factorial n)
  (define (factorial x)
    (if (= x n)
	x
	(* x (factorial (+ x 1)))))
  (factorial 1)) 
;power function
(define (pow base exponent)
  (define (pow-iter i)
    (if (= i exponent)
	base
	(* base (pow-iter (+ i 1)))))
  (if (= exponent 0); pow body
      0 ; x^0 = 1
      (if (= exponent 1)
	  base ; x^1 = x
	  (pow-iter 1))))

;Ackermann function, exercise 1.10
(define (A x y)
  (cond ((= y 0) 0); this doesn't matter for outer-loop-> applicative-order
	((= x 0) (* 2 y))
	((= y 1) 2)
	 (else (A (- x 1); outer-loop
		  (A x (- y 1)))))); inner-loop

; inc dec procedure, exercise 1.09
(define (inc n)
  (+ n 1))
(define (dec n)
  (- n 1))
(define (plus-rec a b)
  (if (= a 0)
      b
      (inc (plus (dec a) b))))
    ;      ^^^^^^^^^^^^^ this function returns b eventually
    ; and calls (plus a b) a-times! Corollary a deferred (inc (inc... chain
    ; of operation ensues... a-times! (inc (inc (inc (inc b)))) ; for a = 4
(define (plus-iter a b)
  (if (= a 0)
      b
      (plus-iter (dec a) (inc b))))
 ; b is being increased a-times because the recursive call depends on 
 ; a reaching 0 (if (= a 0) returns b, the b will be the current (inc b)!
;;; NOTE: (plus-rec -1 1) -> maximum recursion depth exceeded, recursive process
;;;WHEREAS(plus-iter -1 1) -> doesnt return anything, because of the constant
;;; space used via the tail-recursive implementation of scheme! This won't work
;;; in other programming languages with a recursive procedure generating a 
;;; iterative process because each procedure call let's memory grow for the  
;;; iterative process is not recognized by the interpreter. This must be solved
;;; in other languages with special loop constructs such as: while do for (...)

;TODO: brute force, write down: tabulation memoization as optimizations, use the
;SICP ackermann function as an example. Translate the recursive procedure
;count-change - which generates a tree-recursive process - into a iterative
;process through the use of a stack. "

;"The simplest/most general way to eliminate recursion, in general, is to use an
;auxiliary stack -- instead of making the recursive calls, you push their
;arguments into the stack, and iterate. When you need the result of the
;recursive call in order to proceed, again in the general case, that's a tad
;more complicated because you're also going to have to be able to push a "
;continutation request" (that will come off the auxiliary stack when the results
;are known); however, in this case, since all you're doing with all the
;recursive call results is a summation, it's enough to keep an accumulator and,
;every time you get a number result instead of a need to do more call, add it to
;the accumulator.

;;brute force count-change
;solved @galatea !

;1.12 pascal's triangle, compute an arbitrary element through a recursive
;process 
(define phi (/ (+ 1 (sqrt 5)) 2))
(define psi (/ (- 1 (sqrt 5)) 2))