;;; Contains unit tests for the maze solver program, prog.lisp.
;;; This file runs all tests and displays results

;;; Preparation

;;; Safety procaution in case file run from maze-solver or unittests directory
(cond ((ignore-errors (load "prog.lisp")) (setf testdir "unittests/"))
      ((ignore-errors (load "../prog.lisp")) (setf testdir ""))
      (t (progn (princ "Failure to load program file.")
                (princ "Try running from project directory.")
                (quit))))

;;; Suppresses screen output from a print command.
(defun funcall-suppressed (function)
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (funcall function)))

;;; Catches errors testcase might throw, then prints test results to output.
(defun run-test (testcase) ; testcase a function
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

;;; Run solve-maze on input and return value of interest
;;; Separates tests from needing to know specific output order
(defun results (input &key message maze path moves)
  (let ((reder nil)
        (output (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t)))
    (with-output-to-string (*standard-output* output)
      (if (typep input 'string)
          (solve-maze :file input)
          (solve-maze :maze-list input))
      (with-input-from-string (in output)
        (setf reader (read in))
        (when path (return-from results reader))
        (setf reader (read in))
        (when message (return-from results reader))
        (setf reader (read in))
        (when maze (return-from results reader))
        (setf reader (read in))
        (when moves (return-from results reader))))))

;;; Test cases
;;; Tests formatting by passing the test function into run-test.
;;; The test's doc string will display as the test description.
;;; 

(setf success-message "Hooray! I am free.")
(setf failure-message "Help! I am trapped.")
(terpri)
(terpri)


(run-test (lambda ()
  "Should check for valid starting positions & display success on exit."
  (let ((maze '((E +))))
    (assert-equal success-message (results (list maze 0 0) :message t))
    (assert-equal failure-message (results (list maze 0 1) :message t))
    (assert-equal failure-message (results (list maze 0 2) :message t)))))

(run-test (lambda ()
  "Should move forward until obstacle is hit."
  (let ((maze '((O O O O O +))))
    (assert-equal failure-message (results (list maze 0 0) :message t)))))

(run-test (lambda ()
  "Should run right into wall, move down one row, move right to exit."
  (let ((maze '((O O +)
                (+ E O))))
    (assert-equal success-message (results (list maze 0 0) :message t))
    (setf maze '((O O + +)
                 (+ O O E)))
    (assert-equal success-message (results (list maze 0 0) :message t)))))

(run-test (lambda ()
  "Should store current path and return it as result on success."
  (let ((maze '((O O + + +)
                (+ O O O +)
                (+ O O E +)))
        (expected '(START R D R R D)))
    (assert-equal expected (results (list maze 0 0) :path t)))))

(run-test (lambda ()
  "Should mark path on maze and not cross it."
  (let ((maze     '((O O + + +)
                    (+ O O O +)
                    (+ E O O +)))
        (expected '((* X + + +)
                    (+ X X X +)
                    (+ E X X +))))
    (assert-equal expected (results (list maze 0 0) :maze t))
    (setf maze '((O O O +))) ; Checks not crossing, otherwise infinite recursion
    (assert-equal failure-message (results (list maze 0 0) :message t)))))

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
    (assert-equal success-message (results (list maze 0 0) :message t))
    (setf (nth 3 (nth 4 maze)) 'O) ; No exit now
    (assert-equal failure-message (results (list maze 0 0) :message t)))))

(run-test (lambda ()
  "Should navigate a maze with simple backtracking."
  (let ((maze '((+ + + + + + +)
                (+ E O O O O +)
                (+ + + + O + +)))
        (expected '(START L L)))
    (assert-equal expected (results (list maze 1 3) :path t))
    (setf (nth 1 (nth 1 maze)) 'O) ; No exit now
    (assert-equal failure-message (results (list maze 1 3) :message t)))))

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
    (assert-equal expected (results (list maze 1 5) :path t))
    (setf (nth 0 (nth 7 maze)) 'O) ; No exit
    (assert-equal failure-message (results (list maze 1 5) :message t)))))

(run-test (lambda ()
  "Should return number of moves made (including backtracking)."
  (let ((maze '((+ O O O O O +)
                (+ O + + + O O)
                (O O O O + + O)
                (O + O O O + O)
                (O + + + + + O)
                (O O O + O + O)
                (O + O O O + O)
                (E + + + + + +)))
        (expected 55))
    (assert-equal expected (results (list maze 1 5) :moves t)))))

(run-test (lambda ()
  "Should navigate ill-formed mazes."
  (let ((maze '((O O O)
                (+ O + O O +)
                (O O O)
                (O +)
                (O O O E)))
        (expected success-message))
    (assert-equal expected (results (list maze 0 0) :message t)))))

;; Note: maze-for-unittests.txt file creased for following tests:

(run-test (lambda ()
  "Should solve a maze with one starting location from input file."
  (let ((expected success-message)
        (filename (format nil "~Amaze-for-unittests.txt" testdir)))
    (assert-equal success-message (results filename :message t)))))

(run-test (lambda ()
  "Should solve a maze from file that has multiple starting locations."
  (let* ((filename (format nil "~Amaze-for-unittests.txt" testdir))
         (output-capture (with-output-to-string (*standard-output*)
                  (results filename)))
         (actual nil))
    (with-input-from-string (in output-capture)
      (read in) ; discards first path
      (assert-equal success-message (read in nil)) ; first run success
      (progn (read in nil) (read in nil) (read in nil)) ; move to next run
      (assert-equal success-message (read in nil)) ; second run also sucess
      (progn (read in nil) (read in nil) (read in nil)) ; move to third run
      (assert-equal failure-message (read in nil)))))) ; third run is failure

(run-test (lambda ()
  "Should generate a variable size maze of all O's."
  (let ((expected '((O O O)
                    (O O O)
                    (O O O))))
    (assert-equal expected (create-problem 3))
    (setf expected '((O O O O O)
                     (O O O O O)
                     (O O O O O)
                     (O O O O O)
                     (O O O O O)))
    (assert-equal expected (create-problem 5)))))

(terpri)