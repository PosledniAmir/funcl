(defpackage :generics
  (:use :cl)
  (:export :realized?
           :force
           :head
           :tail
           :concat
           :nil?
           :look-for
           :take-out
           :compare))

(in-package :generics)

(defgeneric realized? (object)
  (:documentation "Checks whether the object is realized"))

(defgeneric force (object)
  (:documentation "Forces the lazy evaluation to evaluate"))

(defgeneric head (object)
  (:documentation "Obtains first element from a collection"))

(defgeneric tail (object)
  (:documentation "Obtains rest of the collection (without the first element)"))

(defgeneric concat (element object)
  (:documentation "Inserts element into the collection"))

(defgeneric nil? (object)
  (:documentation "Checks whether the collection is empty"))

(defgeneric look-for (element object)
  (:documentation "Checks whether the collection contains element"))

(defgeneric take-out (element object)
  (:documentation "Removes the element from the collection"))

(defgeneric compare (first second)
  (:documentation "Compares the first element to the second"))
