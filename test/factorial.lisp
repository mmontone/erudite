;; @code-indexing nil
;; This is the factorial function:
(defun factorial (n)
  (if (<= n 1)
      ;; @chunk base-case
      1
      ;; @end chunk
      ;; @chunk recursive-case
      (* n (factorial (1- n)))
      ;; @end chunk
      ))

;; The base case is simple, just check for @verb{n=1} less:
;; @insert-chunk base-case
;; The recursive step is @verb{n x n - 1}:
;; @insert-chunk recursive-case
