;SICP Excercise 1.10
(define (A x y)
  "Ackermann function"
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
;; What are the value of the following experession?
;; (A 1 10)    ;  (A 2 4) = (A 3 3)
;; (A 2 4)     ;
;; (A 3 3)     ;

;;consider the following procedures, where A is the procedure defined above:
(define (f n) (A 0 n)) ; 0 2 4 6 8 10 12        f(n) = 2*n
(define (g n) (A 1 n)) ; 0 2 4 8 16 32 64 128   g(n) = 2^n
(define (h n) (A 2 n)) ; 0 2 4 16 65536 2^65536 h(n) = 2↑n "tetration - exponentiating exponentiation"
;;            (A 3 n)  ; 0 2 4 65536 "pentation - tetration tetration"
(define (k n) (* 5 n n)); 5n^2
;; give concise mathematical definitions for the functions computed by the procedrure
;; f, g and h for positive integer balues of n.

;; The ackermann function is in effect an abstraction of _hyperoperation_ the base is always 2
;; the first argument the "power" of the operation, while the second argument is the hyperoperation
;; that is going to be used: 0 = doubling, 1 = exponentiation, 2 = tetration, 3 = pentation, 4 = ...
