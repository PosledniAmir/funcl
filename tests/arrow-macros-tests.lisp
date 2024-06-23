(defpackage :arrow-macros-tests
  (:use :cl :fiveam :funcl-suite :arrow-macros))

(in-package :arrow-macros-tests)

(def-suite* arrow-macros-suite :in funcl-suite)

(test arrow-test0
      (let ((result (arrow-macros:-> 5 (+ 1) (/ 2) (* 3))))
        (is (= 9 result))))

(test arrow-test1
      (let ((result (arrow-macros:-> 5 (funcall (lambda (x) (+ x 1))) (/ 2) (* 3))))
        (is (= 9 result))))
