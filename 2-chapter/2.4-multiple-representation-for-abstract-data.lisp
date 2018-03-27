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
  (let ((entry (second (get-entry op type))))
    (if entry
	entry
	(error "No op: ~a of type: ~a in *op-table*!" op type))))

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

;; this ultimately fills *op-table* with the deriv-sum, deriv-product function
(defun install-deriv-package ()
  (labels (;; "internal procedures"
	   ;; addition
      	   (tag+ (x) (attach-tag '+ x))
	   (addend (exp) (car exp))
	   (augend (exp) (cadr exp))
	   (make-sum (a1 a2)
	     (cond ((sicp::=number? a1 0) a2)
		   ((sicp::=number? a2 0) a1)
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
			    (multiplicand exp))))
	   ;;c) adding exponentiation
	   (tag** (x) (attach-tag '** x))
	   (base (exp) (car exp))
	   (exponent (exp) (cadr exp))
	   (make-exponentiation (base exponent)
	     (cond ((sicp::=number? exponent 0) 1)
		   ((sicp::=number? exponent 1) base)
		   ((and (sicp::number? base) (sicp::number? exponent))
		    (expt base exponent))
		   (t ;; else
		    (tag** (list base exponent)))))
	   (deriv-expt (exp var)
	     (make-product
	      (make-exponentiation (make-product (exponent exp) (base exp))
				     (make-sum (exponent exp) -1))
	      (deriv (base exp) var))))
    ;; interface to the rest of the system
    (put-op 'deriv '+ #'deriv-sum)
    (put-op 'deriv '* #'deriv-product)
    (put-op 'deriv '** #'deriv-expt))
  'done)

(defun operator (exp) (car exp))

(defun operands (exp) (cdr exp))

;; after calling (install-deriv-package) the following function works!
;; (deriv '(* (** x 44) 2) 'x) ==> (* (** (* 44 X) 43) 2)
(defun deriv (exp var)
  (cond ((sicp::number? exp) 0)
	((sicp::variable? exp) (if (sicp::same-variable? exp var) 1 0))
	(t
	 ;; (apply-generic 'deriv)
	 (funcall (get-op 'deriv (operator exp))
	 	  (operands exp)
	 	  var))))

;; d) What would we have to change if instead of using the operator of an expression as
;; the index, we'd use the opposite (get-op (operator exp) 'deriv)?

;; Answer: Since we just switched the operation and the type when we index what operation to
;; use in the *op-table*, all we have to do is also put the entries in that form in the package:
;; (put-op '+ 'deriv #'deriv-sum) ..

;; To explain what is happening: Previously we called the 'derivative' function of the
;; type '+ Now we call the '+ function of the type 'derivative'. It's sematics, how we
;; view the problem, but I'd argue the former seems more intuitive. After all do we want
;; to say that the derivative of a sum is just a generic '+ procedure?
;; Or that the derivative of a sum is a generic derivative procedure, that dispatches the
;; proper specialized derivative procedure for sums?


;; exercise 2.74

;; Answer: The divisions need to implement a package with public interface ("interface to
;; the rest of the system") for a selector for a file, employee (with name-key as
;; argument) and, arguably, others that operate on the employee data such as get-salary etc.

;; The records datastrucutre of a division must have a type attached to it (for example
;; the division name).  The generic function that fetches the record then dispatches on
;; its type (which was tagged the record by the division) the approriate selector that
;; the devisions have installed in a central *table*. Inside the body of a
;; `install-<division>-package' a call like: (put 'generic-get-record 'division-name
;; division-get-record) is to be found.

;; `get-record' would then not simply select the record but translate it to a common
;; representation that can be shared among all division. Arguably the translation to a
;; common representation might not be wanted, that's when we'd like to have public
;; interface selectors for the other elements like salaray etc.

;; subsequent generic procedure selectors might then either not be necessary, because the
;; record can be dealt with a "common representation" selectors (that operate on the
;; translated record representation, generated from the division's `get-record' method), but
;; internally selectors for the specific division should still be needed to build the
;; `get-record' form.

;; a)
;; (defun get-record (division file employee-name)
;;   (funcall (get-op 'get-record division) file employee-name))

;; b) Selector like get-record, and a way to iterate over all entries. For example all
;; the records could be organized in a list, and CDRed through.

;; c) Like in b), CDR through all the records in a file.

;; d) A new division must implement its own install-division-package, provide a get-record
;; selector etc. just like the other divions. So we see an advantage the exisiting, divisions
;; don't need to be changed in this regard
