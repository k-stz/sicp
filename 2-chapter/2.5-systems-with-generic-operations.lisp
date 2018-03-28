(in-package 2-5-sicp)

;; exercise 2.77

;; tracing through all the calls when issuing (magnitude z)

;; 1. apply-generic 'magnitude is called on z, which will dispatch on
;; the the first tag to z, the type 'complex.

;; 2. magnitude is called. But _where_ does it come from?  Its index
;; with operation 'magnitude and type 'complex, returning the
;; operation, of the same name, 'magnitude.
;; This magnitude operation isn't the selector yet, its the complex-arithmetic
;; selector (see Ben's and Alyssa's implementation in Section 2.4.3).
;; Those call an apply-generic internally:

;; 3. apply-generic again, this time dispatching on the next tag, originally the second,
;; which is 'rectangular. And this finally returns the actual 'magnitude
;; function in the rectangular-package

;; 4. magnitude selector is called in the rectangular-package, it in turn only calls
;; its internal selectors, and the function sqrt, square.

