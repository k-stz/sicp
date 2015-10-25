(defpackage :sicp (:use :cl))
(in-package :sicp)

;; /Conventional Interfaces/----------------------------------------------------

(defun sum-odd-squares (tree)
  (cond ((null tree) 0)
	((atom tree)
	 (if (oddp tree)
	     (square tree)
	     0))
	(t ;;else
	 (+ (sum-odd-squares (car tree))
	    (sum-odd-squares (cdr tree))))))


;; /Sequence Operations/--------------------------------------------------------

;; (filter #'oddp (list 1 2 3 4)) ==> (1 3)
(defun filter (predicate sequence)
  "Return sequence of elements satisfying the predicate."
  (let ((head (car sequence))
	(rest (cdr sequence)))
    (cond ((null (car sequence)) nil)
	  ((funcall predicate head)
	   (cons head (filter predicate rest)))
	  (t ;; else
	   (filter predicate rest)))))

;; (accumulate #'+ 0 (list 1 2 3)) ==> 6
(defun accumulate (op initial sequence)
  "Accumulate elements of sequence using a binary operation (op)."
  (if (null sequence)
	initial
	(funcall op (car sequence)
		 (accumulate op initial (rest sequence)))))

(defun enumerate-interval (low high)
  "Return list of integers from <low> to <high>"
  (if (> low high)
      nil
      (cons low (enumerate-interval (1+ low)
				    high))))

(defun enumerate-tree (tree)
  (cond ((null tree) nil)
	((atom tree) (list tree))
	(t ; else
	 (append (enumerate-tree (car tree))
		 (enumerate-tree (cdr tree))))))


;;; sequence operation examples

(defun 2-sum-odd-squares (tree)
  (accumulate #'+ 0 ;; sum
	      (map 'list #'square ;; squares
		   (filter #'oddp ;; oddly
			   (enumerate-tree tree)))))


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

(defun even-fibs (n)
  (accumulate #'+ 0
	      (filter #'evenp
		      (map 'list #'fib
			   (enumerate-interval 0 n)))))


;; Exercise 2.33

;; implement map in terms of accumulate
(defun accu-map (fn sequence)
  "Imlementation of MAP using ACCUMULATE"
  (accumulate
   (lambda (x y)
     (cons (funcall fn x)
	   y))
   nil
   sequence))


(defun accu-append (seq1 seq2)
  "Implementation of APPEND using ACCUMULATE"
  (accumulate #'cons seq2 seq1))

(defun accu-length (sequence)
  "Implementation of LENGTH using ACCUMULATE"
  (accumulate
   (lambda (x y)
     (declare (ignore x))
     ;; the second argument is building the deferred chain of (+ 1 <here>)
     ;; which terminates in a "0" - the initial element. Hence we add
     ;; the branches containing a (+ 1 ..) effectively returning the
     ;; the length of the list
     (+ 1 y))
   0 sequence))


;; Exercise 2.34

(defun horner-eval (x coefficient-sequence)
  "Evaluate a polynomial in x at a given value in x. Example: f(x)= 1 + 3x + 5x³ + x^5 at
x=2 would be (horner-eval 2 (list 1 3 0 5 0 1).  

This function implements Horner's Rule: It can be proven that any algorithm that computes
arbitrary polynomials must at least use as man additions and multiplications as Horner's
rule, thus Horner's Rule is an optimal algorithm for polynomial evaluation!

This was proved (for the number additions) by A.M. Otrowski in 1954 paper that essentially
founded the modern study of optional algorithms. While V.Y.Pan 1966 proved it for
muliplications."
  (accumulate (lambda (this-coeff higher-terms) ;;??;;
		;; it helps to imagnine it "backwards" starting at the initial=0 then, as
		;; according to Horner's rule "we multiply by x" (* x 0) "then add a.n-1
		;; (* this-coeff (* x 0)) since we're at the end of the deferred chain
		;; "this-coeff" is indeed the last value in the coefficient-sequence. Now
		;; it should be possible to see that the rest of the chain will follow the
		;; order: (... (a.nx + a.n-1)x + ... +a1)x + a0
		(+ this-coeff
		   (* higher-terms x)))
	      0
	      coefficient-sequence ))
