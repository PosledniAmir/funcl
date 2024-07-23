(defpackage :lazy-queue-tests
  (:use :cl :fiveam :lazy-queue :funcl-suite :generics))

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

(test count-method
  (let ((result (lazy-queue 20 17 16 8 12 2 5 11 10 6 1 15 7 18 13
                            14 9 4 19 3)))
    (is (= (get-count result) 20))))

(test to-list-method
  (let ((result (to-list (lazy-queue 5 3 4 1 2))))
    (is (equal? result (list 5 3 4 1 2)))))

(test transform-method
  (let ((result (transform (lazy-queue 5 3 4 1 2) (lambda (x) (+ 1 x)))))
    (is (equal? (to-list result) (list 6 4 5 2 3)))))

(test filter-method
  (let ((result (filter (lazy-queue 5 3 4 1 2) #'evenp)))
    (is (equal? (to-list result) (list 4 2)))))
