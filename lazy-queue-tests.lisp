(defpackage :lazy-queue-tests
  (:use :cl :fiveam :lazy-queue :lazy-thunk-tests))

(in-package :lazy-queue-tests)

(def-suite* lazy-queue-suite :in funcl-suite)

(test lazy-queue-test
  (let ((result (lazy-queue 1 2 3 4 5 6)))
        (is (= 1 (head result))
            (= 2 (head (tail result)))
            (= 3 (head (tail (tail result))))
            (= 4 (head (tail (tail (tail result)))))
            (= 5 (head (tail (tail (tail (tail result))))))
            (= 6 (head (tail (tail (tail (tail (tail result))))))))))

(test concat-method
  (let ((result (concat 4 (concat 3 (lazy-queue 1 2)))))
    (is (= 1 (head result))
        (= 2 (head (tail result)))
        (= 3 (head (tail (tail result))))
        (= 4 (head (tail (tail (tail result))))))))

(test tail-method
  (let ((result (tail (tail (lazy-queue 1 2 3)))))
    (is (= 3 (head result)))))

(test nil-method
  (let ((result (lazy-queue)))
    (is (nil? result))))
