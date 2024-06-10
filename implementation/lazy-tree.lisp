(defpackage :lazy-tree
  (:use :cl :defunclass :generics)
  (:export :rb-tree))

(in-package :lazy-tree)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defunclass lazy-tree-empty () () (:documentation "empty node"))

  (defunclass lazy-tree ()
      ((count 0)
       (value (error "slot value must be set in lazy-tree"))
       (left (<lazy-tree-empty>))
       (right (<lazy-tree-empty>)))
    (:documentation "Lazy B\[alpha\] tree")))

(defmethod nil? ((obj lazy-tree-empty))
  t)

(defmethod nil? ((obj lazy-tree))
  nil)

(defmethod to-list ((obj lazy-tree-empty))
  '())

(defmethod to-list ((obj lazy-tree))
  (concatenate 'list
               (to-list (@left obj))
               (cons (@value obj)
                     (to-list (@right obj)))))

(defun split-list-at (index list1 list2)
  "Splits list into two lists at index."
  (cond
    ((<= index 0) (values (reverse list1) list2))
    (t (split-list-at (- index 1)
                      (cons (rest list2) list1)
                      (cdr list2)))))

(defun build-from-first-aux (size list)
  "Builds balanced binary search tree from the first size items in list."
  (let* ((sizes (multiple-value-list (truncate size 2)))
         (left-size (+ (first sizes) (second sizes)))
         (right-size (first sizes))
         (result (build-from-first (- left-size 1) list))
         (left (first result))
         (rest (second result))
         (value (first rest))
         (result (build-from-first right-size (rest rest)))
         (right (first result))
         (remains (second result)))
    (values (<lazy-tree> size value left right) remains)))

(defun build-from-first (size list)
  "Builds balanced binary search tree from the first size items in the list."
  (cond
    ((<= size 0) (values (<lazy-tree-empty>) list))
    (t (build-from-first-aux size list))))
