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

(test concat-method
  (let ((result (concat 3 (concat 1 (lazy-tree 2 4)))))
    (is (= 1 (head result))
        (= 2 (head (tail result)))
        (= 3 (head (tail (tail result))))
        (= 4 (head (tail (tail (tail result))))))))

(test tail-method
  (let ((result (tail (tail (lazy-tree 3 2 1)))))
    (is (= 3 (head result)))))

(test nil-method
  (let ((result (lazy-tree)))
    (is (nil? result))))

(test look-for-method
  (let ((result (lazy-tree 1 9 4 5 3 2 6 7 8)))
    (is (look-for 1 result)
        (look-for 9 result)
        (look-for 5 result)
        (not (look-for 11 result))
        (not (look-for 10 result))
        (not (look-for 0 result)))))

(test take-out-method
  (let ((result (take-out 1 (take-out 9 (take-out 5 (lazy-tree 8 7 6 2 3 5 4 9 1))))))
    (is (not (look-for 1 result))
        (not (look-for 9 result))
        (not (look-for 5 result))
        (look-for 2 result)
        (look-for 3 result)
        (look-for 4 result))))

(test count-test
      (let ((result (lazy-tree 20 17 16 8 12 2 5 11 10 6 1 15 7 18 13
                               14 9 4 19 3)))
        (is (= (get-count result) 20))))
