
;; exercise 3.1
(defun make-accumulator (start-amount)
  (lambda (x)
    (incf start-amount x)))
;; usage:
;; (defparameter *a* (make-accumulator 10))
;; (funcall *a* 30)


