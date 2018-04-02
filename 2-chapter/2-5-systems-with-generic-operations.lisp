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
	     (attach-tag :cl-number x))
	   (cl-number->complex (n)
	     (make-complex-from-real-imag n 0)))    
    (put-op 'add '(:cl-number :cl-number)
	    (lambda (x y) (tag (+ x y))))
    (put-op 'sub '(:cl-number :cl-number)
	    (lambda (x y) (tag (- x y))))
    (put-op 'mul '(:cl-number :cl-number)
	    (lambda (x y) (tag (* x y))))
    (put-op 'div '(:cl-number :cl-number)
	    (lambda (x y) (tag (/ x y))))
    (put-op 'equ? '(:cl-number :cl-number)
	    (lambda (x y) (= x y)))
    (put-op '=zero? '(:cl-number)
	    (lambda (x) (= x 0)))
    (put-coercion :cl-number 'complex #'cl-number->complex)
    (put-op 'raise '(:cl-number) (get-coercion :cl-number 'complex))
    (put-op 'make :cl-number
	    (lambda (x) (tag x)))
    (put-op 'project '(:cl-number)
	    (lambda (x) (make-real x))))
  'done)

(defun make-cl-number (n)
  (funcall (get-op 'make :cl-number) n))


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
	     (rational->real (n)
	       (make-real (/ (numer n)
			     (denom n))))
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
      (put-op 'numer '(rational)
	      (lambda (x) (numer x)))
      (put-op 'denom '(rational)
	      (lambda (x) (denom x)))
      (put-op 'equ? '(rational rational)
	      (lambda (x y) (and (= (numer x) (numer y))
				 (= (denom x) (denom y)))))
      (put-op '=zero? '(rational)
	      (lambda (x) (= (numer x) 0)))
      (put-op 'make 'rational
	      (lambda (n d) (tag (make-rat n d))))
      ;; we put the coercion here, because its easier
      ;; for the package implementor to know how to translate 
      ;; the type to another, and also the raise operation
      ;; on rationals will be here
      (put-coercion 'rational 'real #'rational->real)
      (put-op 'raise '(rational) (get-coercion 'rational 'real))
      (put-op 'project '(rational)
	      (lambda (n)
		(make-integer (truncate (numer n) (denom n))))))
    'done)

(defun numer (z) (apply-generic 'numer z))
(defun denom (z) (apply-generic 'denom z))


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
    (put-op 'real-part '(rectangular) #'real-part)
    (put-op 'imag-part '(rectangular) #'imag-part)
    (put-op 'magnitude '(rectangular) #'magnitude)
    (put-op 'angle '(rectangular) #'angle)
    (put-op 'equ? '(rectangular rectangular)
	    (lambda (x y) (and (= (real-part x) (real-part y))
			       (= (imag-part x) (imag-part y)))))
    (put-op 'make-from-real-imag 'rectangular 
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put-op 'make-from-mag-ang 'rectangular 
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
    (put-op 'make-from-real-imag 'polar
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put-op 'make-from-mag-ang 'polar 
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
    (put-op 'add '(complex complex)
	 (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put-op 'sub '(complex complex)
	 (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put-op 'mul '(complex complex)
	 (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put-op 'div '(complex complex)
	    (lambda (z1 z2) (tag (div-complex z1 z2))))
    ;; implementing equ? in here suffices, also we choose the rectangluar
    ;; representation to make the comparison because the polar form has
    ;; some peculiarities where a certain modulo of an angle is considered
    ;; the same
    (put-op 'equ? '(complex complex)
	    (lambda (x y)
	      (and (= (apply-generic 'real-part x) (apply-generic 'real-part y))
		   (= (apply-generic 'imag-part x) (apply-generic 'imag-part y)))))
    (put-op '=zero? '(complex)
	    (lambda (x) (= 0 (apply-generic 'real-part x) (apply-generic 'imag-part x))))
    (put-op 'make-from-real-imag 'complex
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put-op 'make-from-mag-ang 'complex
	 (lambda (r a) (tag (make-from-mag-ang r a))))
    ;; from exercise 2.77
    (put-op 'real-part '(complex) #'real-part)
    (put-op 'imag-part '(complex) #'imag-part)
    (put-op 'magnitude '(complex) #'magnitude)
    (put-op 'angle '(complex) #'angle)
    (put-op 'project '(complex) 
	    ;; project complex number to :cl-number
	    (lambda (n)
	      (make-cl-number (real-part n)))))
  'done)

(defun make-complex-from-real-imag (x y)
  (funcall (get-op 'make-from-real-imag 'complex) x y))
(defun make-complex-from-mag-ang (r a)
  (funcall (get-op 'make-from-mag-ang 'complex) r a))

(defun install-generic-arithmetic-package ()
  (install-cl-number-package)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package))


;; exercise 2.78
;; Using Common Lisp native numbers and their type checks with numberp (sicp::number?),
;; without using our taging system. Implementation was realized by checking for the
;; type with sicp::number? and then either not add a tag (attach-tag ..) or return
;; the type :cl-number with (type-tag ...)
;; finally in the cl-number-package (tag x) doesn't do anything to the number, as
;; it may call (attach-to :cl-number on-number), but since attach-to simply returns
;; the number unaltered when testing for sicp::number? no tag gets attached.

;; exercise 2.79
;; adding `equ?' and equality operator to the generic-arithmetic package, done by
;; adding the functions to the packages and exporting them with put-op to the *table-op*

;; exercise 2.80
;; adding =zero?, that tests if its argument is zero to every arithmetic package

;; coercion facility
;; to work with coercion
(defvar *coercion-table* '())
(defmacro get-coercion-types-table (op)
  `(rest (assoc ,op *coercion-table*)))

(defun get-coercion-entry (from-type to-type)
  (let ((types-table (get-coercion-types-table from-type)))
    (loop for entry in types-table
       ;; equal is true for lists of equal symbols
       ;; (equal (cons 'a 'b) (cons 'a 'b)) ==> t
       :when
	 (equal (first entry)
		to-type)
       :return entry)))

(defun get-coercion (from-type to-type)
  (let ((entry (second (get-coercion-entry from-type to-type))))
    (if entry
	entry
	nil)))

(defun put-coercion (from-type to-type item)
  (let ((new-entry (list to-type item)))
    (cond ((null (get-coercion-types-table from-type))
	   ;; new operation entry
	   (setf
	    *coercion-table*
	    (cons (list from-type new-entry) *coercion-table*)))
	  ((null (get-coercion-entry from-type to-type))
	   ;; new type entry under operation
	   (push
	    new-entry (get-coercion-types-table from-type)))
	  (t  
	   (warn "overwriting coercion ~a -> ~a" from-type to-type)
	   (setf (second (get-coercion-entry from-type to-type))
		 item)))
    *coercion-table*))

(defvar *type-tower* '(integer rational real :cl-number complex)
  "List of the types aranged in a tower, where integer `complex' is the highest, and supertype of
all the other types.")

;; part of exercise 2.84 used for the raising types strategy in apply-generic
(defun <type? (type1 type2)
  "Returns true if type1 is lower in the type-tower than type2. The
type-tower is defined in the variable `*type-tower*'"
  ;; memq returns the part of the *type-tower* list where the found element
  ;; is the head. Thus the longer the list, the further at the beginning and thus
  ;; lower the type.
  ;; That's why the higher offset (length of the list) is the lowest type in the tower.
  ;; Finally if the type is not part of the *type-tower*, memq returns an empty list,
  ;; that's where we raise an error signal that the type doesn't exist.
  (let ((type1-offset (length (sicp::memq type1 *type-tower*)))
	(type2-offset (length (sicp::memq type2 *type-tower*))))
    (cond ((= type1-offset 0) (error "type: ~a is not defined in the type-tower!" type1))
	  ((= type2-offset 0) (error "type: ~a is not defined in the type-tower!" type2))
	  ((> type1-offset type2-offset)
	   t)
	  (t
	   nil))))

(defun apply-generic (op &rest args)
  (let* ((type-tags (mapcar #'type-tag args))
	 (proc (get-op op type-tags)))
    (if proc
	(apply proc (mapcar #'contents args))
	(if (= (length args) 2)
	    (let ((type1 (car type-tags))
		  (type2 (cadr type-tags))
		  (a1 (car args))
		  (a2 (cadr args)))
	      (cond ((eq type1 type2)
		     (error "No method for these types ~a ~a. Avoiding raising same types!" type1 type2))
		    ((<type? type1 type2)
		     (apply-generic op (apply-generic 'raise a1) a2))
		    (t
		     (apply-generic op a1 (apply-generic 'raise a2)))))
	    (error "No method for these types ~a"
		   (list op type-tags))))))

;; exercise 2.85
(defun apply-generic-drop (op &rest args)
  (let ((result (apply #'apply-generic (append (list op) args))))
    (print result)
    (if (and (type-tag result)
	     ;; don't drop if raise or projection is used, even if the result is a number
	     ;; because it is always possible to drop a raise, while we don't want to
	     ;; drop a projection because we first want to test if it is raises to back to be the same number
	     ;; precisely to see if it is dropable
	     (case op
	       (raise nil)
	       (project nil)
	       (t t)))
	(drop result)
	result)))

;; exercise 2.81
;; a) If an operation isn't defined for a type, apply-generic will search for coercions.
;; To coerce from one type to the other. But If an coercion of a type to itself dosn't exist
;; (that is t1->t2 and t2->t1 will be empty, we get to the error that there is no method.
;; By adding a same type coercion will simply call apply-generic again, but with two
;; same types.
;; But that's where the problem starts, because in the case of the 'exp operation, there
;; is no operaation for '(complex complex) so that apply-generic will try to coerce again,
;; complex->complex, which Louis implemented, and hence call apply-generic again..
;; causing a recursive endless loop.
;; From this we learn that coercion to the same type breaks our apply-generic, and this
;; base case should be implemented as a dispatched upon function in the package.
;; So to handle (exp '(complex complex) ...) we have to put it in the complex-package.

;; b) So Louis is right about apply-generic trying to coerce argument of the same type,
;; but only if we acutlly put such an entry in the *coercion-table*. Our coercion approach
;; builds on trying to coerce one argument to the others type but in the hope that the
;; operation we consider to apply works on a pair of the type we coerce to.
;; We should thus for safty: forbid coercion of same types.
;; We can't simply cull out the case when both types are already the same, because
;; there can be cases where we are ok of applying the operation on the arguments super
;; types.

;; c) regardless we must implement what I just tried to argue against in b). The change
;; was made to apply-generic above


;; exercise 2.82
;; apply-generic with 3 arguments. Consider a strategy where the 2nd and 3rd argument get
;; coerced to the first argument, then to the second and so on, what could be the problem?
;;
;; Answer: If the first argument is a subtype of the remaining argument then a coercion
;; won't exist in that way such as in (apply-generic 'op cl-number complex complex), this
;; strategy also fails for two argument functions. Then on when we'd try to coerce a
;; number to the 2nd arguments type we'd only have to change the cl-number to a complex.
;; but what we lose here are the types in the tower hierarchy between cl-number and complex.
;; Because we jump over them we try (apply-generic 'op complex complex complex) but never
;; for the types between cl-number and complex (real and rational). Because for those
;; mixed type we'd never dispatch on operations that might exist for mixed types.
;; 
;; Solution: Given a type hierarchy (like the tower: complex, real, rational and
;; cl-number) we determine lowest type highest in the hierarchy and then raise it 
;; to the next higher type. If there is still no dispatch found we keep on raising
;; the lowerst types till we get 3 equal types.
;;
;; Now given a type hierarchy in the form of a tree like with the polygon example, the
;; solution would search for a 'highest type' in the argument which doesn't always make
;; sense when one argument is a triangle, while the other is in the right branch of the
;; polygon tree like a parallelogram.  At this point a solution would be to keep on
;; raising the types step by step, till we reach the most common ancestor or a procedure
;; that applies is found. (in this example 'polygon')


;; exercise 2.83
;; implementing a generic `raise' operation on a tower of types, such as: integer, rational,
;; real, complex. Right after we added packages for the types "integer" and "real"

(defun install-integer-package ()
  (labels
      ((tag (x)
	 (attach-tag 'integer x))
       (integer->rational (n)
	 (make-rational n 1)))
    (put-op 'add '(integer integer) (lambda (x y) (tag (+ x y))))
    (put-op 'sub '(integer integer) (lambda (x y) (tag (- x y))))
    (put-op 'mul '(integer integer) (lambda (x y) (tag (* x y))))
    (put-op 'div '(integer integer) (lambda (x y)
				      (let ((rational (make-rational x y )))
					(if (= 1 (denom rational)) ;; is integer?
					    (denom rational)
					    rational))))
    (put-op 'equ? '(integer integer) (lambda (x y) (= x y)))
    (put-op '=zero? '(integer) (lambda (x) (= x 0)))
    (put-op 'make '(integer) (lambda (x) (tag x)))
    (put-coercion 'integer 'rational #'integer->rational)
    (put-op 'raise '(integer) (get-coercion 'integer 'rational)))
  'done)

(defun make-integer (n)
  (funcall (get-op 'make '(integer)) n))

(defun install-real-package ()
  (labels
      ((tag (x)
	 (attach-tag 'real x))
       (real->cl-number (n)
	 (make-cl-number n)))
    (put-op 'add '(real real) (lambda (x y) (tag (+ x y))))
    (put-op 'sub '(real real) (lambda (x y) (tag (- x y))))
    (put-op 'mul '(real real) (lambda (x y) (tag (* x y))))
    (put-op 'div '(real real) (lambda (x y) (tag (/ x y))))
    (put-op 'equ? '(real real) (lambda (x y) (= x y)))
    (put-op '=zero? '(real) (lambda (x) (= x 0)))
    (put-op 'make '(real) (lambda (x) (tag x)))
    (put-coercion 'real :cl-number #'real->cl-number)
    (put-op 'raise '(real) (get-coercion 'real :cl-number))
    (put-op 'project '(real)
	    (lambda (n)
	      ;; no fancy tricks, just putting it in the numer, making
	      ;; sure its truncated to an integer
	      (make-rational (truncate n 1) 1))))
  'done)

(defun make-real (n)
  (funcall (get-op 'make '(real)) n))

;; exercise 2.83
;; modifying apply-generic to use successive raising instead of direct
;; coercion to one another. apply-generic changed accordingly

;; exercise 2.85
;; we add `project' to every package which returns the number coerced to a lower type, then
;; we raise the number to see if it is still equal. If so we can drop it, without losing
;; data.
;;
;; The aim is to have apply-generic return the simplest type representation, after each
;; operation. Such that the operation (3+4i)+(7-4i) will return the integer 10 instead of
;; (10+0i)
;; TODO-NEXT: figure out how to integrate this with apply-generic
(defun drop (number)
  (if (not (eq 'integer (type-tag number)))
      (let ((projection (apply-generic 'project number)))
	(if
	 (apply-generic 'equ?
			(apply-generic 'raise projection)
			number)
	 (drop projection)
	 number))
      number))
