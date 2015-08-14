(defpackage :sicp
  (:use :cl))

(defun gcd-m (a b)
  (if (= b 0)
      (abs a)
      (gcd-m b (mod a b))))

;;Normal-order evaluation: applying functions before function arguments
;(gcd 206 40)
;(if (= 40 0) ;1.
;      a
;      (gcd 40 (mod 206 40))) ;2. gcd
;gcd:
;(if (= (mod 206 40) 0) ;3. #1-mod  #|its a special form, so not normal-order eval?|#
;    40
;    (gcd (mod 206 40) (mod 40 (mod 206 40)))) ;4. gcd

;gcd
;(if (= (mod 40 (mod 206 40)) 0) ;5. #2-mod #3-mod
;      (mod 206 40)
;      (gcd (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))) ;6 gcd

;gcd
;(if (= (mod (mod 206 40) (mod 40 (mod 206 40))) 0) ;6. #4-#5-#6-#7-mod
;    (mod 40 (mod 206 40)) ;then
;    (gcd (mod (mod 206 40) (mod 40 (mod 206 40)))
;	 (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))))


;(if (= (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) 0) ;7 #14-mod
;    (mod (mod 206 40) (mod 40 (mod 206 40))) ;8 #18-mod FINAL EVALUTION--> 18 MOD evaluated
;    (gcd (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))
;	 (mod (mod (mod 206 40) (mod 40 (mod 206 40)))
;	      (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))


;;--> 18 mods evaluated using normal-order evaluation!   [RIGHT SOLUTION!]
