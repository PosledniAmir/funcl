(defpackage :defunclass
  (:use :cl :generics)
  (:export :defunclass))

(in-package :defunclass)

(defun transform-slot (slot)
  "produces code for reader slots in defunclass"
  (let* ((name (first slot))
         (initform (second slot))
         (initarg (intern (string name) "KEYWORD"))
         (reader (intern (format nil "@~a" name))))
    `(,name :initarg ,initarg :initform ,initform :reader ,reader)))

(defun make-slot-name-pair (slot)
  "produces (:name name)"
  (let* ((name (first slot))
         (initarg (intern (string name) "KEYWORD")))
    (list initarg name)))

(defun make-slot-name-pairs (slots)
  "produces flat list of slot-name-pairs"
  (cond
    ((null slots))
    (t (let* ((result (make-slot-name-pair (first slots)))
              (init (first result))
              (var (second result)))
         (cons init (cons var (make-slot-name-pairs (rest slots))))))))

(defmacro defunclass (name parents slots &body body)
  "defines functional class, that means that all slots are readers"
  (let* ((transformed (map 'list #'transform-slot slots))
         (constructor (intern (format nil "<~a>" name)))
         (slot-name-pairs (make-slot-name-pairs slots)))
    `(progn
       (defclass ,name ,parents ,transformed ,@body)
       (defun ,constructor (&key ,@slots)
         ,(format nil "Constructor for class ~a" name)
         (make-instance (quote ,name) ,@slot-name-pairs)))))
