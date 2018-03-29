(in-package 2-5-sicp)

;; exercise 2.77

;; tracing through all the calls when issuing (magnitude z)

;; 1. apply-generic 'magnitude is called on z, which will dispatch on
;; the the first tag to z, the type 'complex.

;; 2. magnitude is called. But _where_ does it come from?  Its index
;; with operation 'magnitude and type 'complex, returning the
;; operation, of the same name, 'magnitude.
;; This magnitude operation isn't the selector yet, its the complex-arithmetic
;; selector (see Ben's and Alyssa's implementation in Section 2.4.3).
;; Those call an apply-generic internally:

;; 3. apply-generic again, this time dispatching on the next tag, originally the second,
;; which is 'rectangular. And this finally returns the actual 'magnitude
;; function in the rectangular-package

;; 4. magnitude selector is called in the rectangular-package, it in turn only calls
;; its internal selectors, and the function sqrt, square.


;;; generic arithmetic package:



;; generic arith package
(defun install-cl-number-package ()
  (labels ((tag (x)
	     (attach-tag 'cl-number x)))    
    (put-op 'add '(cl-number cl-number)
	    (lambda (x y) (tag (+ x y))))
    (put-op 'sub '(cl-number cl-number)
	    (lambda (x y) (tag (- x y))))
    (put-op 'mul '(cl-number cl-number)
	    (lambda (x y) (tag (* x y))))
    (put-op 'div '(cl-number cl-number)
	    (lambda (x y) (tag (/ x y))))
    (put-op 'make 'cl-number
	    (lambda (x) (tag x))))
  'done)

;; rational package
(defun install-rational-package ()
  ;; internal procedures
    (labels ((numer (x) (car x))
	     (denom (x) (cdr x))
	     (make-rat (n d)
	       (let ((g (gcd n d)))
		 (cons (/ n g) (/ d g))))
	     (add-rat (x y)
	       (make-rat (+ (* (numer x) (denom y))
			    (* (numer y) (denom x)))
			 (* (denom x) (denom y))))
	     (sub-rat (x y)
	       (make-rat (- (* (numer x) (denom y))
			    (* (numer y) (denom x)))
			 (* (denom x) (denom y))))
	     (mul-rat (x y)
	       (make-rat (* (numer x) (numer y))
			 (* (denom x) (denom y))))
	     (div-rat (x y)
	       (make-rat (* (numer x) (denom y))
			 (* (denom x) (numer y))))
	     ;; interface to rest of the system
	     (tag (x) (attach-tag 'rational x)))
      (put-op 'add '(rational rational)
	      (lambda (x y) (tag (add-rat x y))))
      (put-op 'sub '(rational rational)
	      (lambda (x y) (tag (sub-rat x y))))
      (put-op 'mul '(rational rational)
	      (lambda (x y) (tag (mul-rat x y))))
      (put-op 'div '(rational rational)
	      (lambda (x y) (tag (div-rat x y))))

      (put-op 'make 'rational
	      (lambda (n d) (tag (make-rat n d)))))
  'done)

(defun make-rational (n d)
    (funcall (get-op 'make 'rational) n d))
