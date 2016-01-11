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
