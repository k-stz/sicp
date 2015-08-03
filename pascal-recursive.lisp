;;;cl pascal triangle computation with recursive process:
(defun pascal-rec (row col)
  "return pascal number at row and column given"
  (if (or (= col 0) (= col row)) ;hit borders of triangle? left/right
      1
      (+ (pascal-rec (1- row) col)
	 (pascal-rec (1- row) (1- col)))))
