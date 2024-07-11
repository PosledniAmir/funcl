(defpackage :lazy-thunk
  (:use :cl :bordeaux-threads :generics)
  (:export :lazy
           :lazy-stream))

(in-package :lazy-thunk)

(defclass thunk ()
  ((form
    :initarg :form
    :initform (error "you did not provide a value for slot from")
    :reader @form)
   (value
    :accessor value)
   (lock
    :initform (make-lock)
    :reader @lock))
  (:documentation "A class representing lazy evaluation"))

(defmethod realized? ((obj thunk))
  (slot-boundp obj 'value))

(defmethod print-object ((obj thunk) stream)
  (print-unreadable-object (obj stream)
    (cond
      ((realized? obj) (format stream "~a" (value obj)))
      (t (format stream "UNREALIZED")))))

(defmacro lazy (&body body)
  "Creates lazy thunk of body"
  `(make-instance 'thunk :form (lambda () ,@body)))

(defun force-aux (thunk)
  "Auxiliary function for force"
  (with-lock-held ((@lock thunk))
    (cond
      ((realized? thunk) (value thunk))
      (t (let ((result (multiple-value-list (funcall (@form thunk)))))
           (setf (value thunk) result)
           (values-list result))))))

(defmethod force ((obj thunk))
  (cond
    ((realized? obj) (values-list (value obj)))
    (t (force-aux obj))))

(defmethod nil? ((obj thunk))
  (null (force obj)))

(defmethod concat (item (obj thunk))
  (lazy (cons item obj)))

(defmethod head ((obj thunk))
  (car (force obj)))

(defmethod tail ((obj thunk))
  (cdr (force obj)))

(defmacro lazy-stream (&body body)
  "constructor for a lazy-stream"
  (reduce (lambda (x y) (concat y x))
          (reverse body)
          :initial-value (lazy nil)))

(defmethod look-for (element (obj thunk))
  (cond
    ((nil? obj) (values nil nil))
    ((equal? element (head obj)) (values (head obj) t))
    (t (look-for element (tail obj)))))

(defmethod take-out (element (obj thunk))
  (cond
    ((nil? obj) (lazy nil))
    ((equal? element (head obj)) (tail obj))
    (t (concat (head obj) (take-out element (tail obj))))))

(defmethod get-count ((obj thunk))
  (cond
    ((nil? obj) 0)
    (t (+ 1 (get-count (tail obj))))))

(defmethod to-list ((collection thunk))
  (cond
    ((nil? collection) '())
    (t (cons (head collection)
             (to-list (tail collection))))))

(defmethod transform ((collection thunk) function)
  (cond
    ((nil? collection) (lazy nil))
    (t (lazy (cons (funcall function (head collection))
                   (transform (tail collection) function))))))
