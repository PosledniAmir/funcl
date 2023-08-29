(defpackage :defunclass
  (:use :cl :generics)
  (:export :defunclass))

(in-package :defunclass)

(defun transform-slot (slot)
  "produces code for reader slots in defunclass"
  (let* ((name (first slot))
         (initform (second slot))
         (initarg (intern (string name) "KEYWORD")))
    `(,name :initarg ,initarg :initform ,initform :reader ,name)))

(defun make-slot-text (slot)
  "produces format string printing a reader in defunclass"
  (concatenate 'string (format nil ":~a" slot) " ~a"))

(defun intertwine (what where)
  "basically zip, but the second list is generated from single item"
  (cond
    ((null where) nil)
    ((null (cdr where)) (cons (car where) nil))
    (t (cons (car where) (cons what (intertwine what (cdr where)))))))

(defun make-print-text (slots)
  "produces string to printing defunclass by format"
  (let ((texts (map 'list #'make-slot-text slots)))
    (apply #'concatenate 'string (intertwine " " texts))))

(defun make-slot-call (slot)
  "produces code calling slot of an obj - used in defunclass macro for printing"
  `(,slot obj))

(defmacro defunclass (name parents slots &body body)
  "defines functional class, that means that all slots are readers"
  (let* ((transformed (map 'list #'transform-slot slots))
         (slot-names (map 'list #'car slots))
         (text (make-print-text slot-names))
         (items (map 'list #'make-slot-call slot-names)))
    `(progn
       (defclass ,name ,parents ,transformed ,@body)
       (defmethod print-object ((obj ,name) stream)
         (print-unreadable-object (obj stream :type t)
           (format stream ,text ,@items))))))
