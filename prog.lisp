;;; Helper functions

;;; Returns the value at position (x,y) in maze
(defun pos (maze x y) ; maze a list
  (nth y (nth x maze)))

(defun navigate (maze startx starty) ; maze a list
  (pos maze startx starty))

;;; Main function

(defun solve-maze (maze) ; maze a string
  maze)