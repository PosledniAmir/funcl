(defpackage :generics-impl
  (:use :cl :generics))

(in-package :generics-impl)

(defmethod equal? ((first symbol) (second symbol))
  (equal first second))

(defmethod equal? ((first character) (second character))
  (char= first second))

(defmethod to-list ((collection string))
  (coerce collection 'list))

(defmethod to-list ((collection cons))
  collection)
