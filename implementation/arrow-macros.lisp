(defpackage :arrow-macros
  (:use :cl)
  (:export :->))

(in-package :arrow-macros)

(defmacro -> (expression &body body)
  "basic arrow macro (-> 5 (+ 3 2) (* 2)) creates (* (+ 5 3 2) 2)"
  (let ((transformed (reduce
                      (lambda (acc form)
                        (cond
                          ((listp form) `(,(first form) ,acc ,@(rest form)))
                          (t `(,form ,acc))))
                      body
                      :initial-value expression)))
    transformed))
