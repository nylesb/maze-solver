;;; Contains unit tests for the maze solver program, prog.lisp.
;;; This file runs all tests and displays results

;;; Setup

(cond ((ignore-errors (load "prog.lisp")) nil) ; Run from maze-solver dir
      ((ignore-errors (load "../prog.lisp")) nil) ; Run from unittests dir
      (t (print "Failure to load program file.")))
(terpri) ; Readability
(terpri) ; Readability

;;; Catches errors testcase might throw, then prints fail/pass to output.
(defun run-test (testcase) ; testcase is a function
  (if (eval (ignore-errors (funcall testcase)))
    (princ (format nil "   O Pass: ~A ~%" (documentation testcase 'function)))
    (princ (format nil " X   Fail: ~A ~%" (documentation testcase 'function)))))

;;; Test cases

(defun should-read-input-file ()
  "Should read input file until end of file."
  (equal (process-input) :eof))
(run-test #'should-read-input-file)

(terpri) ; Readability