(defpackage :sicp (:use :cl)) (in-package :sicp)

;; Closure,
;; in abstract algebra refers to an operation that given a specific set elements
;; as input returns elements of the set. We say "the elements are closed an operation to
;; elements of the set produces an element that is again an element of the set".
;; Which is the case for the CONS operation, which always returns an object that
;; can be fed back into CONS.
;; Important distinction:
;; The lisp community, and by extension other programming languages by now, refer
;; to procedure with free variables as a closure:

;; (let ((x 2) (y 3))  ;; free variables
;;   (lambda (z)  
;;     (+ z (incf x) (decf y)))) ;; returns a closure

;; SICP will not refer to the word "closure" in this sense.

;; Furthermore on the topic of "means of combination" one of the three key properties
;; of any programming languge is always expected to satisfy the closure property.
;; Some langauges do not provide this property, such as Basic (some version at least)
;; forces one to assemble elements into arrays, but the array is not allowed to contain
;; arrays themselves.


(defun list-ref (n list)
  "CL:NTH"
  (if (= n 0)
      (car list)
      (list-ref (1- n) (cdr list))))


;; Exercise 2.17

(defun last-pair (list)
  "CL:LAST"
  (if (null (cdr list))
      list
      (last-pair (cdr list))))
