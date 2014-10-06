;;; Contains unit tests for the maze solver program, prog.lisp.
;;; This file runs all tests and displays results

;;; Preparation

(cond ((ignore-errors (load "prog.lisp")) nil) ; Run from maze-solver dir
      ((ignore-errors (load "../prog.lisp")) nil) ; Run from unittests dir
      (t (progn (princ "Failure to load program file.")
                (princ "Try running from project directory.")
                (quit))))

;;; Suppresses any terminal printing function might do.
(defun funcall-suppressed (function)
  (with-output-to-string (*standard-output*
                           (make-array '(0)
                                       :element-type 'base-char
                                       :fill-pointer 0 :adjustable t))
    (funcall function)))

;;; Catches errors testcase might throw, then prints fail/pass to output.
(defun run-test (testcase) ; testcase is a function
  (if (ignore-errors (funcall-suppressed testcase))
      (princ (format nil "   O Pass: ~A ~%" (documentation testcase 'function)))
      (princ (format nil " X   Fail: ~A ~%" (documentation testcase 'function)))))

;;; Throws an error if unequal, allowing run-test to catch test failures.
;;; Also prints to display an error message.
(defun assert-equal (expected actual)
  (if (equal expected actual)
      (eval t)
      (error (princ (format nil "Wanted ~A, got ~A." expected actual)))))
  
;;; Test cases

(terpri) ; Readability
(terpri) ; Readability
(setf success-message "Hooray! I am free.")
(setf failure-message "Invalid location.")

(run-test (lambda ()
  "Should have function to access position in maze."
  (let ((maze '((t O O)
                (+ + "cat"))))
    (assert-equal t (pos maze 0 0))
    (assert-equal "cat" (pos maze 1 2))
    (assert-equal 'O (pos maze 0 1))
    (assert-equal nil (pos maze 10 10)))))

(run-test (lambda ()
  "Should display success on exit or fail on invalid position."
  (let ((maze '((E +))))
    (assert-equal success-message (solve-maze maze 0 0))
    (assert-equal failure-message (solve-maze maze 0 1))
    (assert-equal failure-message (solve-maze maze 0 2)))))

(run-test (lambda ()
  "Should move forward until obstacle is hit."
  (let ((maze '((O O O O O +))))
    (assert-equal failure-message (solve-maze maze 0 0)))))

(terpri) ; Readability