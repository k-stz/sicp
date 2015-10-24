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
  (fringe tree))


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

