;;; Solves an appropriately formatted maze and returns results.

(defun solve-maze (&key file maze-list)
  "solve-maze can be run two ways by the appropriate key arguments:
      :file gives input as a filename of a properly formatted maze file
      :maze-list gives input as a list of the form (maze-list init-row init-col)"
  (let* ((maze (copy-tree (first maze-list)))
        (row (second maze-list))
        (col (third maze-list))
        (pos-type nil)
        (moves 0)
        (message nil)
        (path '(START))) ; path stores movements in reverse order
    (labels ((pos (i j &key modify)
               "Safely access maze value at position i j in maze.
               If :modify is passed, sets position to that value first."
               (if (and (> i -1) (< i (length maze)) (> j -1) (< j (length (nth i maze))))
                   (progn (if (not (equal modify nil))
                              (setf (nth j (nth i maze)) modify))
                          (nth j (nth i maze)))))
             (results ()
               (pos (second maze-list) (third maze-list) :modify '*) ; Mark start
               (list (print (reverse path)) (print message) (print maze) (print moves)))
             (navigate ()
               "Recursively travels maze and keeps track of path to not doubleback."
               ;; Stop navigating upon finding an exit, obstacle, or previous path.
               (setf pos-type (pos row col))
               (cond ((or (equal pos-type '+) (equal pos-type nil) (equal pos-type 'X))
                      (progn (setf moves (- moves 2)) ; Fixes false movement
                             (return-from navigate)))
                     ((equal pos-type 'E)
                      (setf message "Hooray! I am free.")
                      (return-from solve-maze (results))))
               ;; Try every direction from current location
               (pos row col :modify 'X) ; Mark current location
               (setf col (+ col 1) path (append '(R) path) moves (+ moves 1)) ; Right
               (navigate)
               (setf col (- col 1) path (cdr path) moves (+ moves 1)) ; Backtrack
               (setf row (+ row 1) path (append '(D) path) moves (+ moves 1)) ; Down
               (navigate)
               (setf row (- row 1) path (cdr path) moves (+ moves 1)) ; Backtrack
               (setf col (- col 1) path (append '(L) path) moves (+ moves 1)) ; Left
               (navigate)
               (setf col (+ col 1) path (cdr path) moves (+ moves 1)) ; Backtrack
               (setf row (- row 1) path (append '(U) path) moves (+ moves 1)) ; Up
               (navigate)
               (setf row (+ row 1) path (cdr path) moves (+ moves 1)) ; Backtrack
               (pos row col :modify 'O) ; Unmark
               (setf message "Help! I am trapped.")
               (return-from navigate))) ; Paths exhausted
      (navigate)
      (setf message "Help! I am trapped.")
      (results))))