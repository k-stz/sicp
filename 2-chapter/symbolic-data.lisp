(in-package :sicp)

(defun memq (item list)
  "Searches for <item> in <list>. If the item isn't part of the list it returns NIL,
else it will return the rest of the list lead by the item in question."
  (cond ((null list) NIL)
	((eq item (first list)) list)
	(t
	 (memq item (rest list)))))

;; TODO: was implemented previously?

(defun number? (x)
  (numberp x))

;; utils

(defun =number? (exp num)
  "Checks whether an expression is equal to a given number."
  (and (number? exp) (= exp num)))

;; exercise 2.53

;; (list 'a 'b 'c) ;; ==> (a b c)
;; (list (list 'george)) ;; ==> ((george))
;; (cdr '((x1 x2) (y1 y2))) ;; ==> ((y1 y2))
;; because '((x1 x2) (y1 y2)) is logically equivalent to:
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
;; so that (car ''abracadabra) is equivalent to (car '(quote abracadabra))
;; ==> QUOTE



;; Example: Symbolic Differentiation

;; To illustrate the power of symbolic manipulation we will implement a procedure
;; that will differentiate an algebraic expression. The procedure shall take
;; as argument the expression and a variable and return the derivative with
;; respect to the variable.

;; "Symbolic differentiation is of historical significance in Lisp. It was one of
;; the motivating examples behind the development of a computer language for
;; symbolic manipulation."

;; Thanks to the notion of data abstraction we can approach the problem of differentiation
;; abstractly by first considering how to deal with "sums", "products" and "variables"
;; without worrying about how they're represented.


;; Lets simplify further by only considering expression build up with only addition and
;; multiplication operations. Now we can formulate these four reduction rules:

;; 1. dc/dx = 0, for c, a constant or a variable different from x 

;; 2. dx/dx = 1

;; 3. d(u+v)    du      dv          Notice that the last two reduction rules are recursive:
;;    ------ = ----  + ----         to get the derivative of a sum we first find the derivative
;;      dx      dx      dx          of the terms. The summands now could either be of the form
;;                                  of the latter two rules, or of the first two, and hence
;; 4. d(uv)       dv        du      evaluate to a constant 0 or 1. Quite handy as 0 and 1 are
;;    ------ = u(----) + v(----)    the identity elements of addition and multiplication operation.
;;      dx        dx        dx

;; exercise 2.56, the exponentiation rule:
;; 
;; 5. d(u^n)              du
;;    ------ = nu^(n-1) (----)
;;      dx                dx

;; careful: SICP seems to call the first term of an addition the "augend" and the second
;; the "addend" literature suggests these terms to be used the other way around



;; wishful thinking implementation:

;; (variable? e)         -- is e a variable?
;; (sum? e)              -- is e a sum?
;; (addend e) (augend e) -- sum expression selectors
;; (make-sum a1 a2)      -- makes a sum
;; (product? e)          -- is e a product?
;; (multiplier e)        -- multiplier of e
;; (multiplicand e)      -- multiplicand of e


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
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))	 
	;; exercise 2.56 - exponentiation rule
	((exponentiation? exp)
	 (make-product
	  (make-exponentionation (make-product (exponent exp) (base exp))
				 (make-sum (exponent exp) -1))
	  (deriv (base exp) var)))
	(t ;; else
	 (error "Expression ~a is of unknown type" exp))))

;; DERIV implements the complete differentiation algorithm. Since it is expressed in terms
;; of abstract data, our wishful thinking tools, it will work no matter how we choose
;; to represent algebraic expressions, as long as we design a proper set of selectors
;; and constructors.


;; Representing algebraic expression
;; we will represent them as lisp forms (Lisp combinations)
;; I.e. an expression like ax + b will be represented as (+ (* a x) b)
;; this approach has many advantages. The most prominent one is that we already have a 
;; parser for such forms build into our language, Lisp.

(defun variable? (x)
  (symbolp x))

(defun same-variable? (var-1 var-2)
  (and (variable? var-1) (variable? var-2) (eq var-1 var-2)))


;; implementation for a two-pair only sums:
;; (defun make-sum (a1 a2)
;;   (cond ((=number? a1 0) a2)
;; 	((=number? a2 0) a1)
;; 	((and (number? a1)
;; 	      (number? a2))
;; 	 (+ a1 a2))
;; 	(t ;;else
;; 	 `(+ ,a1 ,a2))))


;; implementation for a two-pair only product:
;; (defun make-product (m1 m2)
;;   (cond ((or (=number? m1 0)
;;       	     (=number? m2 0)) 0)
;;       	((=number? m1 1) m2)
;;       	((=number? m2 1) m1)
;;       	((and (number? m1) (number? m2)) (* m1 m2))
;;       	(t ;;else
;;       	 `(* ,m1 ,m2))))


;;; exercise 2.57 - modifying MAKE-SUM and MAKE-PRODUCT to work with multiple arguments

;; (defun make-sum (&rest summands)
;;   "Make an algebraic expression sum of the summands given."
;;   (if (<= (length summands) 1)
;;       (error "Need at least two summands to make a sum. Summands passed: ~a" summands)
;;       (let* ((numbers-sum (apply #'+ (filter #'(lambda (x) (number? x)) summands)))
;; 	     (non-numbers (filter #'(lambda (x) (not (number? x))) summands)))
	
;; 	(if (= numbers-sum 0)
;; 	    (if (= (length non-numbers) 1)
;; 		(first non-numbers)
;; 		`(+ ,@non-numbers))
;; 	    (if (null non-numbers)
;; 		numbers-sum
;; 		`(+ ,numbers-sum ,@non-numbers))))))

;; (defun make-product (&rest factors)
;;   "Make an algebraic expression product of the factors given ."
;;   (if (<= (length factors) 1)
;;       (error "Need at least two factors to make a sum. Factors passed: ~a" factors)

;;       ;; filter out all the numbers and multiply them, supersedes filtering out 1 and 0
      
;;       (let* ((numbers (filter #'(lambda (x) (number? x)) factors))
;; 	     (non-numbers (filter #'(lambda (x) (not (number? x))) factors))
;; 	     (numbers-product (apply #'* numbers)))

;;         (cond ((null numbers) `(* ,@non-numbers))
;; 	      ((= numbers-product 0) 0)
;; 	      ((and (= numbers-product 1) (null non-numbers)) 1)
;; 	      ((and (= numbers-product 1 (length non-numbers))) (first non-numbers))
;; 	      ((and (= numbers-product 1) (> (length non-numbers) 1) `(* ,@non-numbers)))
;; 	      (t ;; else:
;; 	       `(* ,numbers-product ,@non-numbers))))))


;; sum constructor, selectors, predicates:
;; here we see the advantage of using Lisp forms to represent expressions.
;; To tell if an algebraic expression is a sum, we just need to see if the
;; first element in the list is a +! (sum? '(+ 1 1)),
;; (first '(+ 1 1)) ==> +

;; (defun sum? (expression)
;;   "Returns true if the algebraic expression is a sum."
;;   (eq (first expression) '+))

;; (defun addend (sum)
;;   (second sum))

;; (defun augend (sum)
;;   (if (<= (length sum) 3)
;;       (third sum) ;;'(+ x2 x3)
;;       (apply #'make-sum (cddr sum))))


;; product contructor, selectors, predicates

;; (defun product? (expression)
;;   (eq (first expression) '*))

;; (defun multiplier (product)
;;   (second product))

;; (defun multiplicand (product)
;;   (if (<= (length product) 3)
;;       (third product)
;;       (apply #'make-product (cddr product))))


;; exercise 2.56 - adding exponentiation to the differntiation algorithm
;;                 expression of the form (** x 2) denote x²

(defun exponentiation? (expression)
  (eq (first expression) '**))

(defun base (power-expression)
  "Return the base of a power-expression. base^exponent=power."
  (second power-expression))

(defun exponent (power-expression)
  "Return the exponent of a power-expression. base^exponent=power"
  (third power-expression))

(defun make-exponentionation (base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent))
	 (expt base exponent))
	(t ;; else
	 `(** ,base ,exponent))))


;; exercise 2.58 - infix algebraic expression.

;; a) constructors, selectors and predicates for infix algebraic expressions of fully parenthesised,
;;    two argument expressions:


;;; sums (infix constructor, selector, predicates)

(defun make-sum (a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1)
	      (number? a2))
	 (+ a1 a2))
	(t ;;else
	 `(,a1 + ,a2))))


;; (defun sum? (expression)
;;   "Infix sum predicate. Returns true if the alebraic expression is a sum."
;;   (eq (second expression) '+))

;; (defun addend (sum)
;;   (first sum))

;; (defun augend (sum)
;;   (third sum))


;;; products (infix constructor, selector, predicates)

(defun make-product (m1 m2)
    (cond ((or (=number? m1 0)
      	     (=number? m2 0)) 0)
      	((=number? m1 1) m2)
      	((=number? m2 1) m1)
      	((and (number? m1) (number? m2)) (* m1 m2))
      	(t ;;else
      	 `(,m1 * ,m2))))

;; (defun product? (expression)
;;   (eq (second expression) '*))

;; (defun multiplier (product)
;;   (first product))

;; (defun multiplicand (product)
;;   (third product))


;; b) like a) but with multiple arguments, that is, not only parenthesised two arguments

;; example for a): (deriv '(x + (3 * (x + (y + 2)))) 'x) ==> 4

;; example for b): (deriv '(x + 3 * (x + y + 2)))

;; Since we need to understand that a '*' takes priority when occuring "in the wild" that is
;; together with unparenthesiszed addends, I suggest, we reduce the problem back to how it
;; appears in a). Which means we will find the multiplications in the wild and parenthesise
;; the expressions in such a way that a)-like problems will be formed, for which we have a
;; solution.
;; First we need to understand that unnecessary parenthesis have been dropped:
;; (x + y + 2) = (x + (y + 2)).

;; scratch that, lets form lists using tokens

(defun token-split (list token)
  "Splits list into sublists between the token given. Lists within lists are not recursed
into, this means that (token-split '(1 * (2 3)) '*) =returns=> ((1) (2 3))"
  (labels ((rec (list new-sublist)
	     (cond ((eq (first list) token)
		    ;; we found a token, lets build a new sublist..
		    (cons (nreverse new-sublist)
			  (rec (rest list) ;; ..starting right _after_ the token (REST)
			       nil)))
		   ((null list) (list new-sublist)) ;; total end of the list reached, return the build
		   ;; sublist since the last token (or base case, begining of
		   ;; list containing no occurence of the token)
		    (t ;; else
		     (rec (rest list)
			  ;; here add up the elements prior to the next token into the
			  ;; new sublist
			  (cons (first list) new-sublist))))))
	   (rec list '())))

(defun contains? (exp symbol)
  "Returns true if the symbol is part of the top-level of the expression. That
is (contains? '(x + 2) 'x) is true but (contain? '(y + (x + 2) 'x)) is NIL."
  (if (not (null
	    (filter (lambda (x) (eq x symbol))
		    exp)))
      t
      nil))

;; We want to enforce the multiplication priority and at the same time reduce the problem back to an "a)-like"
;; one, this is how we'll do it:
;; (3 + x * y + 2)
;; (3 + (x * y) + 2)
;; (3 + ((x * y) + 2))
;; GROUP-AROUD will help us achieve that goal.

(defun group-around (list symbol)
  "Parenthesise elements in a list around a symbol given. Ensures that at most two elements are
grouped around the symbol. 
Example: (group-around '(x * y * z) '*) ==> (((X * Y) * Z))"
  (when (or (eq (first list) symbol) (eq (last list) symbol))
    (warn 
     "First or last datum in list is the symbol to be grouped around, function not
	specified for this case."))
  (labels ((rec (list grouped-list)
	     (let ((head (first list))
		   (next (cadr list)))
	       (cond ((null list) (nreverse grouped-list))
		     ((eq symbol next)
		      (rec (cdddr list) ; skip the two ones ahead being grouped
			   ;; actuall grouping (parenthesising) takes place here:
			   (cons (list head next (third list))
				 grouped-list)))
		     (t
		      (rec (rest list)
			   (cons head grouped-list)))))))
    (if (> (length list) 3)
	(loop :for new-list = (rec list nil) :then (rec new-list nil)
	   :while (and (contains? new-list symbol) (> (length new-list) 3))
	   :finally (return new-list))
	list)))


(defun operator-priority-group (expression)
  "Applies GROUP-AROUND on the expression given with operators in order of evaluation
priority."
  (let ((transformed-expression expression))
    (loop :for operator :in '(* ** +) ; priority list
       :do
       (setf transformed-expression
	     (group-around transformed-expression operator)))
    transformed-expression))

;; now we can implement the predicates and selectors

(defun product? (expression)
  (contains? (operator-priority-group expression)
	     '*))

(defun sum? (expression)
  (let ((simplified-expression (operator-priority-group expression)))
    (and (not (product? simplified-expression))
	 (not (contains? simplified-expression '**))
	 (contains? simplified-expression '+))))

(defun addend (sum)
  (first (operator-priority-group sum)))

(defun augend (sum)
  (third (operator-priority-group sum)))

(defun multiplier (product)
  (first (operator-priority-group product)))

(defun multiplicand (product)
  (third (operator-priority-group product)))


;; /exercise 2.58 b) done
