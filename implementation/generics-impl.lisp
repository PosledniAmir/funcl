(defpackage :generics-impl
  (:use :cl :generics))

(in-package :generics-impl)

(defmethod equal? ((first symbol) (second symbol))
  (equal first second))

(defmethod equal? ((first character) (second character))
  (char= first second))

(defmethod equal? ((first symbol) (second character))
  nil)

(defmethod equal? ((first character) (second symbol))
  nil)

(defmethod compare ((first symbol) (second symbol))
  (let ((a (string first))
        (b (string second)))
    (cond
      ((string= a b) 0)
      ((string< a b) -1)
      ((string> a b) 1))))

(defmethod compare ((first character) (second character))
  (cond
    ((char= first second) 0)
    ((char< first second) -1)
    ((char> first second) 1)))

(defmethod compare ((first null) (second character))
  -1)

(defmethod compare ((first character) (second null))
  1)

(defmethod compare ((first cons) (second cons))
  (cond
    ((null first) -1)
    ((null second) 1)
    (t (let ((result (compare (car first) (car second))))
         (cond
           ((= 0 result) (compare (cdr first) (cdr second)))
           (t result))))))

(defmethod compare ((first cons) (second null))
  1)

(defmethod compare ((first null) (second cons))
  -1)

(defmethod compare ((first integer) (second integer))
  (cond
    ((= first second) 0)
    ((< first second) -1)
    ((> first second) 1)))

(defmethod to-list ((collection string))
  (coerce collection 'list))

(defmethod to-list ((collection cons))
  collection)
