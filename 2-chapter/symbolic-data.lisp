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

