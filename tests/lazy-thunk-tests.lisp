(defpackage :lazy-thunk-tests
  (:use :cl :fiveam :funcl-suite :lazy-thunk :generics))

(in-package :lazy-thunk-tests)

(def-suite* lazy-thunk-suite :in funcl-suite)

(test lazy-macro
  (let ((result (lazy (+ 1 2))))
    (is (= 3 (force result)))))

(test multiple-eval
  (let* ((x 0)
         (result (lazy (setf x (+ 5 x)) (+ 0 1))))
    (is (= 1 (force result))
        (= 1 (force result))
        (= 5 x))))

(test nil-method
  (let ((result (lazy nil)))
    (is (nil? result))))

(test lazy-stream-macro
  (let ((result (lazy-stream 1 2 3 4 5 6)))
    (is (= 1 (head result)))))

(test concat-method
  (let* ((a (lazy nil))
         (b (concat (+ 1 0) a))
         (c (concat (+ 2 0) b)))
    (is (= 2 (head c))
        (= 1 (head (tail c))))))

(test head-method
  (let* ((a (lazy nil))
         (b (concat (+ 1 0) a))
         (c (concat (+ 2 0) b)))
    (is (= 2 (head c))
        (= 1 (head (tail c))))))

(test tail-method
  (let* ((a (lazy nil))
         (b (concat (+ 1 0) a))
         (c (concat (+ 2 0) b)))
    (is (null (head (tail (tail c)))))))

(test count-method
      (let ((result (lazy-stream 20 17 16 8 12 2 5 11 10 6 1 15 7 18 13
                                 14 9 4 19 3)))
        (is (= (get-count result) 20))))
