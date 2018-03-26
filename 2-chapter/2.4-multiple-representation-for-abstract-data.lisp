(in-package :2-4-sicp)

;; sometimes we want to use more than one underlying representation for particular
;; data. For example for Complex numbers a rectangle and a polar representation.
;; Or when writing a huge system with many programmers it might not be possible
;; to agree early on what representation to use, or even what kind of data to
;; combine or keep seperate. That's why different parts of a program have to
;; deal with different representation.

;; This can be implemented using type tags and generic operation
;; (think data types and clos generic functions?)

;; /generic procedures/ can operate on data that can be represented in more than
;; one way. Think like in the huffmann tree example where `symbols' could operate
;; on leafs and partial trees/branches, where it would check the type using
;; the "leaf" element as a "type tag", dispatch the appropirate code.

;; /dispatching on type/, is what we speak of when we first check the type of
;; an object and then decide what procedure to call on it. So just like with
;; CLOS and generic functions where depending on the argument types a particular
;; defined method is called, or rather dispatched!

;; What are weaknesses of the generic procedure interface?
;; (1) It is not /additive/ - whenever we want to add a new data representation
;; we, the implementors, must change all the generic procedures that use
;; the data, and we need to know all the generic functions in a large-scale
;; system in order to modify all the generic functions involved. A source
;; of inconvenience and error.
;; (2) The people interface with the individual representation must also look out not to
;; run into naming conflicts by using our code.

;; Solution to the the two problems:
;; (1) the dispatch mechanism gets removed from the generic procedure into a table
;; with a operation argument-type(s) as index and appropriate method as entry.
;; Now using a (apply-generic 'operation-name typed-argument) will lookup the
;; method to be called, and dispatch them.
;; (2) We create packages.

;; exercise 2.73 - derivative example

;; a) Why can't we use data driven-dispatch for the `deriv' of cases `number?' and `same-variable?'
;; Answer: Because the implementation of operator and operands uses list operations car and cdr,
;;         to get the data. But number and a single variable are atoms. Also we use the objects
;;         as keys to index the operator table. Now we can't dispatch on any variable name or
;;         a particular number.

;; b) deriv table implementation and adding rules for sums and product

(defparameter *op-table*
  '((deriv (+ deriv-sum))))

(defmacro get-entry (op type)
  `(assoc ,type (get-types-table ,op)))
(defmacro get-types-table (op)
  `(rest (assoc ,op *op-table*)))

(defun get-op (op type)
  (second (get-entry op type)))

(defun put-op (op type item)
  (let ((new-entry (list type item)))
    (cond ((null (get-types-table op))
	   ;; new operation entry
	   (setf
	    *op-table*
	    (cons (list op new-entry) *op-table*)))
	  ((null (get-entry op type))
	   ;; new type entry under operation
	   (push
	    new-entry (get-types-table op)))
	  (t  
	   (warn "overwriting op:~a type:~a entry" op type)
	   (setf (second (get-entry op type))
		 item)))
    *op-table*))

(defun attach-tag (type datum)
  (cons type datum))

(defun type-tag (datum)
  (if (consp datum)
      (car datum)
      (error "~a : Bad tagged datum -- TYPE-TAG " datum)))

(defun contents (datum)
  (if (consp datum)
      (cdr datum)
      (error "~a : Bad tagged datum -- CONTENTS " datum)))

(defun apply-generic (op &rest args)
  (let* ((type-tags (mapcar #'type-tag args))
	 (proc (get-op op type-tags)))
    (if proc
	(apply proc (mapcar #'contents args))
	(error "No method for these types -- APPLY-GENERIC ~a"
	       (list op type-tags)))))

;; We will use sicp::deriv expects infix notation (arg-1 op arg-2)
;; the operations are slightly altered
(defun operator (exp) (second exp))

(defun operands (exp) (list (car exp) (caddr exp)))

(defun deriv (exp var)
  (cond ((sicp::number? exp) 0)
	((sicp::variable? exp) (if (sicp::same-variable? exp var) 1 0))
	(t
	 (funcall (get-op 'deriv (operator exp))
		  (operands exp)
		  var))))

(defun install-deriv-package ()
  (labels (;; "internal procedures"
	   ;; addition
      	   (tag+ (x) (attach-tag '+ x))
	   (addend (exp) (car exp))
	   (augend (exp) (cadr exp))
	   (make-sum (a1 a2)
	     (cond ((sicp::=number? a1 0) a1)
		   ((sicp::=number? a2 0) a2)
		   ((and (numberp a1)
			 (numberp a2))
		    (+ a1 a2))
		   (t
		    (tag+ (list a1 a2)))))
	   (deriv-sum (exp var)
	     (make-sum (deriv (addend exp) var)
		       (deriv (augend exp) var)))
	   ;; multiplication
	   (tag* (x) (attach-tag '* x))
	   (multiplier (exp) (car exp))
	   (multiplicand (exp) (cadr exp))
	   (make-product (m1 m2)
	     (cond ((or (sicp::=number? m1 0)
			(sicp::=number? m2 0)) 0)
		   ((sicp::=number? m1 1) m2)
		   ((sicp::=number? m2 1) m1)
		   ((and (sicp::number? m1) (sicp::number? m2)) (* m1 m2))
		   (t ;;else
		    (tag* (list m1 m2)))))
	   (deriv-product (exp var)
	     (make-sum
	      (make-product (multiplier exp)
			    (deriv (multiplicand exp) var))
	      (make-product (deriv (multiplier exp) var)
			    (multiplicand exp)))))
    ;; interface to the rest of the system
    (put-op 'deriv '+ #'deriv-sum)
    (put-op 'deriv '* #'deriv-product))
  'done)
