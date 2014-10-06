;;; Helper functions

;;; Returns the value at position (x,y) in maze
(defun pos (maze i j) ; maze a list
  (nth j (nth i maze)))

(defun navigate (maze starti startj) ; maze a list
  (if (equal (pos maze starti startj) 'O)
      (eval t)
      (print "Invalid starting location.")))

;;; Main function

(defun solve-maze (maze) ; maze a string
  maze)