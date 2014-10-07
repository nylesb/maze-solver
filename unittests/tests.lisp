;;; Contains unit tests for the maze solver program, prog.lisp.
;;; This file runs all tests and displays results

;;; Preparation

(cond ((ignore-errors (load "prog.lisp")) nil) ; Run from maze-solver dir
      ((ignore-errors (load "../prog.lisp")) nil) ; Run from unittests dir
      (t (progn (princ "Failure to load program file.")
                (princ "Try running from project directory.")
                (quit))))

;;; Suppresses any standard output printing function might do.
(defun funcall-suppressed (function)
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (funcall function)))

;;; Catches errors testcase might throw, then prints test results to output.
(defun run-test (testcase) ; testcase is a function
  (let ((error-capture (make-string-output-stream)))
    (if (with-open-stream (*error-output* (make-broadcast-stream error-capture))
          (ignore-errors (funcall-suppressed testcase)))
        (princ (format nil "  O Pass: ~A ~%" (documentation testcase 'function)))
        (princ (format nil " X  Fail: ~A ~%" (documentation testcase 'function))))
    (princ (get-output-stream-string error-capture))))

;;; Throws an error if unequal, allowing run-test to catch test failures.
;;; Also sends an error message to show expected vs actual result.
(defun assert-equal (expected actual)
  (if (equal expected actual)
      (eval t)
      (progn (format *error-output* "      Expected: ~A~%" expected)
             (format *error-output* "      Actual  : ~A.~%" actual)
             (error ""))))
  
;;; Test cases

(terpri) ; Readability
(terpri) ; Readability
(setf success-message "Hooray! I am free.")
(setf invalid-message "Invalid location.")
(setf failure-message "Help! I am trapped.")

(run-test (lambda ()
  "Should display success on exit or invalid on + or out of maze."
  (let ((maze '((E +))))
    (assert-equal success-message (second (solve-maze :maze-list (list maze 0 0))))
    (assert-equal invalid-message (second (solve-maze :maze-list (list maze 0 1))))
    (assert-equal invalid-message (second (solve-maze :maze-list (list maze 0 2)))))))

(run-test (lambda ()
  "Should move forward until obstacle is hit."
  (let ((maze '((O O O O O +))))
    (assert-equal failure-message (second (solve-maze :maze-list (list maze 0 0)))))))

(run-test (lambda ()
  "Should run right into wall, move down one row, move right to exit."
  (let ((maze '((O O +)
                (+ E O))))
    (assert-equal success-message (second (solve-maze :maze-list (list maze 0 0))))
    (setf maze '((O O + +)
                 (+ O O E)))
    (assert-equal success-message (second (solve-maze :maze-list (list maze 0 0)))))))

(run-test (lambda ()
  "Should store current path and return it as result on success."
  (let ((maze '((O O + + +)
                (+ O O O +)
                (+ O O E +)))
        (expected '(START R D R R D)))
    (assert-equal expected (first (solve-maze :maze-list (list maze 0 0)))))))

(run-test (lambda ()
  "Should mark path on maze and not cross it."
  (let ((maze     '((O O + + +)
                    (+ O O O +)
                    (+ E O O +)))
        (expected '((X X + + +)
                    (+ X X X +)
                    (+ E X X +))))
    (assert-equal expected (third (solve-maze :maze-list (list maze 0 0))))
    (setf maze '((O O O +))) ; Checks not crossing, otherwise infinite recursion
    (assert-equal failure-message (second (solve-maze :maze-list (list maze 0 0)))))))

(run-test (lambda ()
  "Should navigate a sprial successfully."
  (let ((maze '((O O O O O O +)
                (+ + + + + O +)
                (+ O O O + O +)
                (+ O + O + O +)
                (+ O + E + O +)
                (+ O + + + O +)
                (+ O O O O O +)
                (+ + + + + + +))))
    (assert-equal success-message (second (solve-maze :maze-list (list maze 0 0))))
    (setf (nth 3 (nth 4 maze)) 'O) ; No exit now
    (assert-equal failure-message (second (solve-maze :maze-list (list maze 0 0)))))))

(run-test (lambda ()
  "Should navigate a maze with simple backtracking."
  (let ((maze '((+ + + + + + +)
                (+ E O O O O +)
                (+ + + + O + +)))
        (expected '(START L L)))
    (assert-equal expected (first (solve-maze :maze-list (list maze 1 3))))
    (setf (nth 1 (nth 1 maze)) 'O) ; No exit now
    (assert-equal failure-message (second (solve-maze :maze-list (list maze 1 3)))))))

(run-test (lambda ()
  "Should navigate complicated maze which requires backtracking."
  (let ((maze '((+ O O O O O +)
                (+ O + + + O O)
                (O O O O + + O)
                (O + O O O + O)
                (O + + + + + O)
                (O O O + O + O)
                (O + O O O + O)
                (E + + + + + +)))
        (expected '(START U L L L L D D L D D D D D)))
    (assert-equal expected (first (solve-maze :maze-list (list maze 1 5))))
    (setf (nth 0 (nth 7 maze)) 'O) ; No exit
    (assert-equal failure-message (second (solve-maze :maze-list (list maze 1 5)))))))

(run-test (lambda ()
  "Should return number of moves made (including backtracking)."
  (let ((maze '((+ + + + + + +)
                (+ E O O O O +)
                (+ + + + O + +)))
        (expected 9))
    (assert-equal expected (fourth (solve-maze :maze-list (list maze 1 2))))
    (setf (nth 1 (nth 1 maze)) 'O expected (+ expected 1)) ; No exit now
    (assert-equal expected (fourth (solve-maze :maze-list (list maze 1 2)))))))

(terpri) ; Readability