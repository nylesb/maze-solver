;;; Solves an appropriately formatted maze and returns results.

(defun solve-maze (&key file maze-list)
  "solve-maze can be run two ways by the appropriate key arguments:
      :file gives input as a filename of a properly formatted maze file
      :maze-list gives input as a list of the form (maze-list init-row init-col)"
  (let* ((maze (copy-tree (first maze-list)))
        (row (second maze-list))
        (col (third maze-list))
        (pos-type nil)
        (path '(START))) ; path stores movements in reverse order
    (labels ((pos (i j)
               (if (and (> i -1) (> j -1))
                   (nth j (nth i maze))
                   nil))
             (navigate ()
               (setf pos-type (pos row col))
               ;; Stop navigating upon finding an exit or hitting an obstacle
               (cond ((or (equal pos-type '+) (equal pos-type nil) (equal pos-type 'X))
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
               (setf col (- col 1) path (append '(L) path)) ; Left
               (navigate)
               (setf col (+ col 1) path (cdr path)) ; Backtrack
               (setf row (- row 1) path (append '(U) path)) ; Up
               (navigate)
               (setf row (+ row 1) path (cdr path)) ; Backtrack
               (setf (nth col (nth row maze)) 'O) ; Unmark
               (return-from navigate (list path "Help! I am trapped.")))) ; Paths exhausted
      (navigate))))