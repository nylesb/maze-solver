;; This driver shows example usas of my (non-test) functions.
;; Additionally, it will create a dribble to see the results of the run
;; Exaime the test file for test specifics; otherwise the example funciton
;; runs should give an idea of the types of problems solved.

(dribble "breecher_hw1.out")
(load "unittests/tests.lisp")
(solve-maze :maze-list (list '((O + O)
                               (O + E)
                               (O O O)) 0 0))
(solve-maze :file "unittests/maze-for-unittests.txt")
(create-problem)
(create-problem :size 9 :times 5)
(dribble)