(defun solve-maze (&key file maze-list)
  "solve-maze can be run two ways by the appropriate key arguments:
      :file gives starting-positions as a filename of a properly formatted maze file
      :maze-list gives starting-positions as a list of the form (maze-list init-row init-col)"
  (let* ((startmaze nil)
         ; Collect starting positions depending if called with a file name or list
         (starting-positions
           (if (equal file nil)
             (progn (setf startmaze (first maze-list))
                    (list (list (second maze-list) (third maze-list))))
             (with-open-file (data file)
               (let ((temp '()))
                 (setf startmaze (read data))
                 (loop (if (listen data) ; Loop for each starting position
                           (setf temp (append temp (list (list (read data) (read data)))))
                           (return temp)))))))
         (maze nil)
         (startrow nil)
         (startcol nil)
         (row nil)
         (col nil)
         (pos-type nil) ; Stores type at particular position to avoid recomputations
         (moves 0)
         (message nil)
         (path '(START))) ; stores movements in reverse order
    (dolist (start starting-positions)
      ;; Initialize data for run
      (setf maze (copy-tree startmaze) row (first start) col (second start)
            moves 0 path '(START) message "Help! I am trapped.")
      (block top-level-navigate ; Block allows us to stop all recursion with a return-from
        (labels ((pos (i j &key modify)
                   "Safely access maze value at position i j in maze.
                   If :modify is passed, sets position to that value first."
                   (when (and (> i -1) (< i (length maze)) (> j -1) (< j (length (nth i maze))))
                     (unless (equal modify nil) ; Update maze
                       (setf (nth j (nth i maze)) modify))
                     (nth j (nth i maze))))
                 (results ()
                   "Formats and prints output after maze navigation is complete"
                   (pos (first start) (second start) :modify '*) ; Mark start
                   (list (print (reverse path)) (print message) (print maze) (print moves)))
                 (navigate ()
                   "Recursively travels maze using let* block's vars to store intermediate results."
                   ;; Stop navigating upon finding an exit, obstacle, or previous path.
                   (setf pos-type (pos row col))
                   (cond ((or (equal pos-type '+) (equal pos-type nil) (equal pos-type 'X))
                          (progn (setf moves (- moves 2)) ; Fixes false movement
                                 (return-from navigate)))
                         ((equal pos-type 'E)
                          (setf message "Hooray! I am free.")
                          (return-from top-level-navigate (results))))
                   ;; Recursively try every direction from current location
                   (pos row col :modify 'X) ; Mark current location
                   (setf col (+ col 1) path (append '(R) path) moves (+ moves 1)) ; Right
                   (navigate)
                   (setf col (- col 1) path (cdr path) ; Backtrack
                         row (+ row 1) path (append '(D) path) moves (+ moves 2)) ; Down
                   (navigate)
                   (setf row (- row 1) path (cdr path) ; Backtrack
                         col (- col 1) path (append '(L) path) moves (+ moves 2)) ; Left
                   (navigate)
                   (setf col (+ col 1) path (cdr path) ; Backtrack
                         row (- row 1) path (append '(U) path) moves (+ moves 2)) ; Up
                   (navigate)
                   (setf row (+ row 1) path (cdr path) moves (+ moves 1)) ; Backtrack
                   (pos row col :modify 'O))) ; Paths from this start exhausted, unmark
          (navigate)
          (results))))))

(defun create-problem (size)
  "Randomly generates a maze & set of starting coordinates, then runs solve-maze on them."
  (let ((maze (make-list size :initial-element (make-list size :initial-element 'O))))
    maze))