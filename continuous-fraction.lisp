(defun cont-frac (n d k)
  (if (< k 1)
      1 ;termination
      (/ n
	 (+ d
	    (cont-frac n d (1- k))))))
