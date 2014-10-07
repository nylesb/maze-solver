;;; Helper functions

;;; Returns the value at position (x,y) in maze
(defun pos (maze i j) ; maze a list
  (nth j (nth i maze)))

;;; Main function

(defun solve-maze (maze init-row init-col) ; maze a string
  (let ((row init-row)
        (col init-col)
        (position-type (pos maze init-row init-col))
        (path '(START))) ; path stores movements in reverse order
    (defun navigate ()
      (setf position-type (pos maze row col))
      ;; Stop navigating upon finding an exit or hitting an obstacle
      (cond ((or (equal position-type '+) (equal position-type nil))
             (return-from navigate (list (reverse path) (print "Invalid location."))))
            ((equal position-type 'E)
             (return-from solve-maze (list (reverse path) (print "Hooray! I am free.")))))
      ;; Try every direction from current location
      (setf col (+ col 1)) ; Right 
      (setf path (append '(R) path))
      (navigate)
      (pop path)
      (setf col (- col 1)) 
      (setf row (+ row 1)) ; Down
      (setf path (append '(D) path))
      (navigate)
      (pop path)
      (setf row (- row 1))
      (return-from navigate "Help! I am trapped.")) ; Paths exhausted
    (navigate)))