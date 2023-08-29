(defpackage :lazy-thunk
  (:use :cl :bordeaux-threads :generics)
  (:export :lazy
           :lazy-stream))

(in-package :lazy-thunk)

(defclass thunk ()
  ((form
    :initarg :form
    :initform (error "you did not provide a value for slot from")
    :reader form)
   (value
    :accessor value)
   (lock
    :initform (make-lock)
    :reader lock))
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
  (with-lock-held ((lock thunk))
    (cond
      ((realized? thunk) (value thunk))
      (t (let ((result (multiple-value-list (funcall (form thunk)))))
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
