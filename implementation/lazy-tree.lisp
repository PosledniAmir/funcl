(defpackage :lazy-tree
  (:use :cl :defunclass :generics)
  (:export :rb-tree))

(in-package :lazy-tree)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defunclass lazy-tree-empty () () (:documentation "empty node"))

  (defunclass lazy-tree ()
      ((count 0)
       (value (error "slot value must be set in lazy-tree"))
       (left (make-instance 'lazy-tree-empty))
       (right (make-instance 'lazy-tree-empty)))
    (:documentation "Lazy B\[alpha\] tree")))

(defmethod nil? ((obj lazy-tree-empty))
  t)

(defmethod nil? ((obj lazy-tree))
  nil)

