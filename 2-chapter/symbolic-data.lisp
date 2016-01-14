(defpackage :sicp (:use :cl))
(in-package :sicp)

(defun memq (item list)
  (cond ((null list) NIL)
	((eq item (first list)) list)
	(t
	 (memq item (rest list)))))


;; exercise 2.53

;; (list 'a 'b 'c) ;; ==> (a b c)
;; (list (list 'george)) ;; ==> ((george))
;; (cdr '((x1 x2) (y1 y2))) ;; ==> ((y1 y2))
;; because '((x1 x2) (y1 y2)) is logically equivallent to:
;; (cons (cons 'x1 (cons 'x2 nil)) #|2nd:|# (cons (cons 'y1 (cons 'y2 nil)) nil))
;; so that the CDR is: (cons                            nil)
;;      list within list ->  (cons 'y1 (cons 'y2 nil))


;; CONSP, is the cl equivalent to scheme's PAIR?
;; (consp (car '(a short list))) ;; ==> NIL, because 'a alone is not a CONS 

;; (memq 'red '((red shoes) (blue socks))) ;; ==> NIL, understanding this is crucial
;; as the elements of the list are the literal data: '(red shoes) and '(blue socks),
;; which are not equal to 'red !

;; (memq 'red '(red shoes blue socks)) ;; ==> (RED SHOES BLUE SOCKS)


;; exercise 2.54

(defun equal? (list-1 list-2)
  "Returns true if the two lists contain EQ elements and in the same order."
  (cond ((and (eq list-1 nil)
	      (eq list-2 nil)) t)
	((eq (first list-1) (first list-2))
	 (equal? (rest list-1) (rest list-2)))
	(t ;; else:
	 nil)))



;; exercise 2.55

;; (car ''abracadabra) ;; ==> QUOTE
;; because 'a is a shorthand for (quote a), ergo ''a is (quote (quote a))
;; and that's why (car (quote (quote a))) returns QUOTE
;; (quote (quote a)) returns a quoted list containing (quote a)
;; so that (car ''abracadabra) is equivallent to (car '(quote abracadabra))
;; ==> QUOTE



;; Example: Symbolic Differentiation

;; To illustrate the power of symbolic manipulation we will implement a procedure
;; that will differentiate an algebraic expression. The procedure shall take
;; as agrument the expression and a variable and return the derivative with
;; respect to the variable.

;; "Symbolic differentiation is of historical significance in Lisp. It was one of
;; the motivating examples behind the development of a computer language for
;; symbolic manipulation."

;; Thanks to the notion of data abstraction we can approach the problem of differentiation
;; abstractly by first considering how to deal with "sums", "products" and "variables"
;; without worring about how they're represented.


;; Lets simplify further by only considering expression build up with only addition and
;; muliplication operations. Now we can formulate these four reduction rules:

;; dc/dx = 0, for c, a constant or a variable different from x 

;; dx/dx = 1

;; d(u+v)    du      dv          Notice that the last two reduction rules are recursive:
;; ------ = ----  + ----         to get the derivative of a sum we first find the derivative
;;   dx      dx      dx          of the terms. The summands now could either be of the form
;;                               of the latter two rules, or of the first two, and hence
;; d(uv)       dv        du      evaluate to a constant 0 or 1. Quite handy as 0 and 1 are
;; ------ = u(----) + v(----)    the identity elements of addition and multiplication operation.
;;   dx        dx        dx


;; careful: SICP seems to call the first term of an addition the "augend" and the second
;; the "addend" literature suggests these terms to be used the other way around



;; wishful thinking implementation:

;; (variable? e)         -- is e a variable?
;; (sum? e)              -- is e a sum?
;; (addend e) (augend e) -- sum expression selectors
;; (make-sum a1 a2)      -- makes a sum
;; (product? e)          -- is e a product?
;; (multiplier e)        -- multiplier of e
;; (muliplicand e)       -- multiplicant of e


(defun deriv (exp var)
  (cond (;; first rule
	 (number? exp) 0)
	(;; second rule
	 (variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 ;; third rule. see the recursive nature?
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 ;; fourth rule
	 (make-sum
	  (make-product (mulitplier exp)
			(deriv (multiplicand exp) var)))
	 ;; TODO: why are the arguments reversed here?
	 (make-product (deriv (multiplier exp) var)
		       (multiplicand exp)))
	(t ;; else
	 (error "Expression ~a is of unknown type" exp)))))

;; DERIV implements the complete differntiation algorithm. Since it is expressed in terms
;; of abstract data, our wishful thinking tools, it will work no matter how we choose
;; to represent algebraic expressions, as long as we design a proper set of selectors
;; and constructors.
