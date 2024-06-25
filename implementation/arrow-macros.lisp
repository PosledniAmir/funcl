(defpackage :arrow-macros
  (:use :cl)
  (:export :->))

(in-package :arrow-macros)

(defun function? (list)
  (and (listp list)
       (eq 'function (first list))))

(defun funcall? (list)
  (and (listp list)
       (eq 'funcall (first list))))

(defmacro -> (expression &body body)
  "basic arrow macro (-> 5 (+ 3 2) (* 2)) creates (* (+ 5 3 2) 2)"
  (let ((transformed (reduce
                      (lambda (acc form)
                        (cond
                          ((funcall? form) `(,(first form) ,(second form) ,acc ,@(rest (rest form))))
                          ((function? form) `(funcall ,form ,acc))
                          ((listp form) `(,(first form) ,acc ,@(rest form)))
                          (t `(funcall ,form ,acc))))
                      body
                      :initial-value expression)))
    transformed))
