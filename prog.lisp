;;; Helper functions

;;; Returns the value at position (x,y) in maze
(defun pos (maze i j) ; maze a list
  (nth j (nth i maze)))

;;; Main function

(defun solve-maze (maze init-row init-col) ; maze a string
  (let ((row init-row)
        (col init-col))
    (defun navigate ()
      ;; Verify valid starting location
      (if (equal (pos maze row col) 'O)
          (eval t)
          (print "Invalid starting location.")))
    (navigate)))