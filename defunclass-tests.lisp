(defpackage :defunclass-tests
  (:use :cl :fiveam :lazy-thunk-tests :defunclass))

(in-package :defunclass-tests)

(def-suite* lazy-thunk-suite :in funcl-suite)

(defunclass testing-class () ((item0 nil) (item1 1)) (:documentation "testing class"))

(test defunclass-test0
  (let ((result (make-instance 'testing-class :item0 25 :item1 nil)))
    (is (string= "#<TESTING-CLASS :ITEM0 25 :ITEM1 NIL>"
                 (format nil "~a" result))
        (= 25 (item0 result))
        (null (item1 result)))))

