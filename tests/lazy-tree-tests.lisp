(defpackage :lazy-tree-tests
  (:use :cl :fiveam :lazy-tree :generics :generics-impl :funcl-suite))

(in-package :lazy-tree-tests)

(def-suite* lazy-tree-suite :in funcl-suite)

(test lazy-tree-test
      (let ((result (lazy-tree 1 3 2 6 5 4)))
        (is (= 1 (head result))
            (= 2 (head (tail result)))
            (= 3 (head (tail (tail result))))
            (= 4 (head (tail (tail (tail result)))))
            (= 5 (head (tail (tail (tail (tail result))))))
            (= 6 (head (tail (tail (tail (tail (tail result))))))))))



