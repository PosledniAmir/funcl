(defpackage :rb-tree-tests
  (:use :cl :fiveam :rb-tree :generics :funcl-suite))

(in-package :rb-tree-tests)

(def-suite* rb-treesuite :in funcl-suite)

(defmethod compare ((a integer) (b integer))
  (cond
    ((= a b) 0)
    ((< a b) -1)
    ((> a b) 1)))

(test rb-tree-test
  (let ((result (rb-tree 1 3 2 6 5 4)))
    (is (= 1 (head result))
        (= 2 (head (tail result)))
        (= 3 (head (tail (tail result))))
        (= 4 (head (tail (tail (tail result)))))
        (= 5 (head (tail (tail (tail (tail result))))))
        (= 6 (head (tail (tail (tail (tail (tail result))))))))))

(test concate-method
  (let ((result (concat 3 (concat 1 (rb-tree 2 4)))))
    (is (= 1 (head result))
        (= 2 (head (tail result)))
        (= 3 (head (tail (tail result))))
        (= 4 (head (tail (tail (tail result))))))))
