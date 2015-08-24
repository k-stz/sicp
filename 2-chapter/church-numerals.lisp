(in-package :sicp)


;; exercise 2.6 -- Church numerals ---------------------------------------------

;; this one is weird, the church numerals are representation of numbers using nothing but
;; procedures. The numeral zero would be:

(defun cn-zero (f)
  "Church numeral 0."
  (declare (ignore f))
   (lambda (x) x))

;; and this is the operation of adding one
(defun cn-add-1 (n)
  "Adds one to the Church numeral n."
  (lambda (f)
    (lambda (x)
      (funcall f
	       (funcall (funcall n f)
			x)))))

;; from this we can infer what "1" would be like:
;; (add-1 #'zero)

;; innermost (funcall n f) returns the IDENTITY function
;; (funcall #'identity x) x is returned
;; (lambda (f) (funcall (x) (funcall f x))) THIS is "1"

(defun cn-one (f)
  "Church numeral 1."
  (lambda (x)
    (funcall f x)))

;; (add-1 #'one) expanded:
;; (lambda (f)
;;   (lambda (x)
;;     (funcall f
;; 	     (funcall (funcall
;; 		       (lambda (f)
;; 			 (lambda (x)
;; 			   (funcall f x))) f) x))))
;; this part      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; can be reduced, to the innermost: (funcall f x), hence

(defun cn-two (f)
  "Church numeral 1."
  (lambda (x)
    (funcall f
	     (funcall f x))))


;; This means numbers are represented as:
;; zero: (f (λx. x))
;; one : (f (λx. (f x)))
;; two : (f (λx. (f (f x))))


;; First lets make sure how to add more nested (f x) in there
;; in add-1 one of the innermost calls is

;; (funcall n f)

;; if we pass our church numeral in there (one):
;; (funcall (lambda (f)
;; 	    (lambda (x)
;; 	      (funcall f x))) f)
;; ==> (lambda (x)
;;       (funcall f x))

;; we should be able to see now that whatever the Church numeral is
;; (funcall n f) will always reduce to (λ (f (λ..x)) nesting passed but what
;; about the additional (funcall <what-we-discussed> x) around there?
;; well _that x_ is being passed in the reduced lambda as shown above:

;; (funcall
;;  (lambda (x)
;;    (funcall f x))
;;  x) ;;<- _that x_
;; ==> (funcall f x) ;; while note (f x) could be an arbitrary nesting indicating a greater
                  ;; church numeral
;; In effect both these funcalls effectively expose the x, (f x), (f (f x)) "numeral"
;; part of the procedure!
;; We could imagine the preceding (f (λ ..)) of every church numeral to be the "gatekeeper part"
;; then the above funcalls passed give you admission and papers of how to deal with the
;; insides of the church numeral representation.

;; Now that we know how to enter the insides of a church numeral we can try to surgically
;; plant another church numeral in there... addition!

;; Given a church numeral: (λf.λx.  f (f (f ... (f x))))
;; in the following this   ^^^^^^^  ^^^^^^^^^^^^^^^^^
;; is the "gatekeeper part" and     |||||||||||||||||
;; is the "numeral part"

(defun cn-add (n m)
  "Add Church numerals n and m"
  (lambda (f)     ;; the "gatekeeper" part of our resulting
    (lambda (x)   ;; church numeral!
      (funcall
       (funcall m f) ;; <- this will return m's (λx. (f (f .... x) - numeral part-, so we
		     ;; can pass in x, into the numeral part of n, like so:
                     ;;opening the n church numeral's belly, this will return the
                     ;;numeral-part of n:
       (funcall (funcall n f) x)))))

;; WORKS: (funcall (funcall (cn-add #'cn-two #'cn-one) #'1+) 2) ==> 5
;; 2 + 1 = 3, we apply #'1+ three times: (1+ (1+ (1+ 2))) ==> 5 !!
