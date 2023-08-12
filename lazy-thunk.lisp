(defpackage :lazy-thunk
  (:use :cl :bordeaux-threads)
  (:export :lazy
           :force
           :nil?
           :concat
           :head
           :tail
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
  "checks whether the thunk is realized"
  (slot-boundp obj 'value))

(defmethod print-object ((obj thunk) stream)
  "prints either value of thunk or UNREALIZED"
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
  "Forces thunk to evaluate and returns the result"
  (cond
    ((realized? obj) (values-list (value obj)))
    (t (force-aux obj))))

(defmethod nil? ((obj thunk))
  "Checks whether the thunk is nil or not"
  (null (force obj)))

(defmethod concat (item (obj thunk))
  "Concatenates item with lazy thunk - for lazy-streams"
  (lazy (cons item obj)))

(defmethod head ((obj thunk))
  "car for lazy streams"
  (car (force obj)))

(defmethod tail ((obj thunk))
  "cdr for lazy stream"
  (cdr (force obj)))

(defmacro lazy-stream (&body body)
  "constructor for a lazy-stream"
  (reduce (lambda (x y) (concat y x))
          (reverse body)
          :initial-value (lazy nil)))
