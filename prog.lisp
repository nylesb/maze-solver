;;; Solves an appropriately formatted maze and returns results.

(defun solve-maze (maze init-row init-col) ; maze a string
  (let ((row init-row)
        (col init-col)
        (pos-type nil)
        (path '(START))) ; path stores movements in reverse order
    (labels ((pos (i j)
               (nth j (nth i maze)))
             (navigate ()
               (setf pos-type (pos row col))
               ;; Stop navigating upon finding an exit or hitting an obstacle
               (cond ((or (equal pos-type '+) (equal pos-type nil)); (equal pos-type 'Q))
                      (return-from navigate (list (reverse path) (print "Invalid location."))))
                     ((equal pos-type 'E)
                      (return-from solve-maze (list (reverse path) (print "Hooray! I am free.") maze))))
               ;; Try every direction from current location
               (setf (nth col (nth row maze)) 'X) ; Mark current location
               (setf col (+ col 1) path (append '(R) path)) ; Right
               (navigate)
               (setf col (- col 1) path (cdr path)) ; Backtrack
               (setf row (+ row 1) path (append '(D) path)) ; Down
               (navigate)
               (setf row (- row 1) path (cdr path)) ; Backtrack
               (return-from navigate (list path "Help! I am trapped.")))) ; Paths exhausted
    (navigate))))