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

;; ordinary numbers

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

;; complex - rectangle
(defun install-rectangular-package ()
  ;; internal procedures
  (labels ((real-part (z) (car z))
	   (imag-part (z) (cdr z))
	   (make-from-real-imag (x y) (cons x y))
	   (magnitude (z)
	     (sqrt (+ (sicp::square (real-part z))
		      (sicp::square (imag-part z)))))
	   (angle (z)
	     (atan (imag-part z) (real-part z)))
	   (make-from-mag-ang (r a) 
	     (cons (* r (cos a)) (* r (sin a))))
	   ;; interface to the rest of the system
	   (tag (x) (attach-tag 'rectangular x)))
    (put 'real-part '(rectangular) #'real-part)
    (put 'imag-part '(rectangular) #'imag-part)
    (put 'magnitude '(rectangular) #'magnitude)
    (put 'angle '(rectangular) #'angle)
    (put 'make-from-real-imag 'rectangular 
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular 
	 (lambda (r a) (tag (make-from-mag-ang r a)))))
  'done)
;; complex - polar

(defun install-polar-package ()
  ;; internal procedures
  (labels ((magnitude (z) (car z))
	   (angle (z) (cdr z))
	   (make-from-mag-ang (r a) (cons r a))
	   (real-part (z)
	     (* (magnitude z) (cos (angle z))))
	   (imag-part (z)
	     (* (magnitude z) (sin (angle z))))
	   (make-from-real-imag (x y) 
	     (cons (sqrt (+ (sicp::square x) (sicp::square y)))
		   (atan y x)))
	   ;; interface to the rest of the system
	   (tag (x) (attach-tag 'polar x)))
    (put-op 'real-part '(polar) #'real-part)
    (put-op 'imag-part '(polar) #'imag-part)
    (put-op 'magnitude '(polar) #'magnitude)
    (put-op 'angle '(polar) #'angle)
    (put 'make-from-real-imag 'polar
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar 
	 (lambda (r a) (tag (make-from-mag-ang r a)))))
  'done)

(defun real-part (z) (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle (z) (apply-generic 'angle z))


(defun install-complex-package ()
  ;; imported procedures from rectangular and polar packages
  (labels
      ((make-from-real-imag (x y)
	 (funcall (get-op 'make-from-real-imag 'rectangular) x y))
       (make-from-mag-ang (r a)
	 (funcall (get-op 'make-from-mag-ang 'polar) r a))
       ;; internal procedures
       (add-complex (z1 z2)
	 (make-from-real-imag (+ (real-part z1) (real-part z2))
			      (+ (imag-part z1) (imag-part z2))))
       (sub-complex (z1 z2)
	 (make-from-real-imag (- (real-part z1) (real-part z2))
			      (- (imag-part z1) (imag-part z2))))
       (mul-complex (z1 z2)
	 (make-from-mag-ang (* (magnitude z1) (magnitude z2))
			    (+ (angle z1) (angle z2))))
       (div-complex (z1 z2)
	 (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
			    (- (angle z1) (angle z2))))
       ;; interface to rest of the system
       (tag (z) (attach-tag 'complex z)))
    (put 'add '(complex complex)
	 (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
	 (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
	 (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
	 (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
	 (lambda (r a) (tag (make-from-mag-ang r a)))))
  'done)

(defun make-complex-from-real-imag (x y)
  (funcall (get-op 'make-from-real-imag 'complex) x y))
(defun make-complex-from-mag-ang (r a)
  (funcall (get-op 'make-from-mag-ang 'complex) r a))

(defun install-generic-arithmetic-package ()
  (install-cl-number-package)
  (install-rational-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package))


;; NEXT TODO add the apply-generic from the exercises, and onther one somewhere in chapter 2.5
