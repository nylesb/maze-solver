(defun solve-maze (&key file maze-list)
  "solve-maze can be run two ways by the appropriate key arguments:
      :file gives input as a filename of a properly formatted maze file
      :maze-list gives input as a list of the form (maze-list init-row init-col)"
  (let* ((startmaze nil)
         (input (if (equal file nil)
                    (progn (setf startmaze (first maze-list))
                           (list (list (second maze-list) (third maze-list))))
                    (with-open-file (data file)
                      (let ((temp '()))
                        (setf startmaze (read data))
                        (loop
                        (if (listen data)
                            (setf temp (append temp (list (list (read data) (read data)))))
                            (return temp)))))))
         (maze nil)
         (startrow nil)
         (startcol nil)
         (row nil)
         (col nil)
         (pos-type nil)
         (moves 0)
         (message nil)
         (path '(START))) ; stores movements in reverse order
    (dolist (start input)
      ;; Initialize data for next run
      (setf maze (copy-tree startmaze) row (first start) col (second start) moves 0 path '(START))
      (block top-level-navigate ;; Block allows us to stop all recursion with a return-from
        (labels ((pos (i j &key modify)
                   "Safely access maze value at position i j in maze.
                   If :modify is passed, sets position to that value first."
                   (when (and (> i -1) (< i (length maze)) (> j -1) (< j (length (nth i maze))))
                     (unless (equal modify nil) ; Update maze
                       (setf (nth j (nth i maze)) modify))
                     (nth j (nth i maze))))
                 (results ()
                   (pos (first start) (second start) :modify '*) ; Mark start
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
                          (return-from top-level-navigate (results))))
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
          (results))))))

(defun create-problem (size)
  "Randomly generates a maze & set of starting coordinates, then runs solve-maze on them."
  (let ((maze '()))
    (body)))