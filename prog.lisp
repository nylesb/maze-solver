;;; Helper functions

;;; Returns the value at position (x,y) in maze
(defun pos (maze i j) ; maze a list
  (nth j (nth i maze)))

;;; Main function

(defun solve-maze (maze init-row init-col) ; maze a string
  (let ((row init-row)
        (col init-col)
        (position-type (pos maze init-row init-col)))
    (defun navigate ()
      (setf position-type (pos maze row col))
      ;; Stop navigating upon finding an exit or hitting an obstacle
      (cond ((or (equal position-type '+) (equal position-type nil))
             (return-from navigate (print "Invalid location.")))
            ((equal position-type 'E)
             (return-from solve-maze (print "Hooray! I am free."))))
      (setf col (+ col 1)) (navigate) (setf col (- col 1)) ; Right
      (setf row (+ row 1)) (navigate) (setf row (- row 1))) ; Down
    (navigate)))