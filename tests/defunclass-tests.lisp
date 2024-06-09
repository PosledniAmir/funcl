(defpackage :defunclass-tests
  (:use :cl :fiveam :funcl-suite :defunclass :generics))

(in-package :defunclass-tests)

(def-suite* lazy-thunk-suite :in funcl-suite)

(defunclass testing-class () ((item0 nil) (item1 1)) (:documentation "testing class"))

(test defunclass-test0
  (let ((result (<testing-class> :item0 25 :item1 nil)))
    (is (= 25 (@item0 result))
        (null (@item1 result)))))
