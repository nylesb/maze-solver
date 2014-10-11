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
            moves 0 path '(START) message "Invalid starting position.")
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
                   (progn (princ (format nil "(~A ~%" (first maze)))
                          (princ (format nil "~{ ~A ~%~}" (rest (reverse (rest (reverse maze))))))
                          (princ (format nil " ~A)" (nth (- (length maze) 1) maze)))
                          (print message) (print moves) (print (reverse path))))
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
          (unless (or (equal (pos row col) 'O) (equal (pos row col) 'E))
            (return-from top-level-navigate (results)))
          (navigate)
          (setf message "Help! I am trapped.")
          (results))))))

(defun create-problem (&key (size (+ (random 7) 3)) (wall-density 0.5) (times 3))
  "Randomly generates a maze & set of starting coordinates, then runs solve-maze on them.
  Size will be random from 3 to 10, or can be specified by user on call.
  Similarly, density of walls is default to 50%, or can be set by user on call.
  Will never generate an invalid starting position."
  (let* ((maze (copy-tree (make-list size :initial-element (make-list size :initial-element 'O))))
         (walls (* (* size size) wall-density))
         (choice nil))
    ;; Randomly sprinkle empty maze with walls to appropriate number of walls
    (do ((num-walls 1 (+ num-walls 1)))
        ((> num-walls walls))
        (setf choice (list (random size) (random size)))
        (if (equal (nth (second choice) (nth (first choice) maze)) '+)
            (setf num-walls (- num-walls 1))
            (setf (nth (second choice) (nth (first choice) maze)) '+)))
    (setf (nth (random size) (nth (random size) maze)) 'E) ; Place exit
    (dotimes (count times) ; Run however many times is asked for
      (setf choice (list (random size) (random size)))
      ;; Make sure we didn't pick an invlaid start
      (do ()
          ((equal (nth (second choice) (nth (first choice) maze)) 'O))
          (setf choice (list (random size) (random size))))
      (solve-maze :maze-list (list maze (first choice) (second choice))))))