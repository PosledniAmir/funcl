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

(defun split-at (index list acc)
  (cond
    ((<= index 0) (values (reverse acc) list))
    (t (split-at (- index 1)
                 (rest list)
                 (cons (first list) acc)))))

(defun from-sorted (size list)
  (cond
    ((<= size 0) (values (<lazy-tree-empty>) list))
    (t (let* ((result (multiple-value-list (truncate size 2)))
              (left-size (- (+ (first result) (second result)) 1))
              (right-size (first result))
              (left-result (from-sorted left-size list))
              (left-tree (first left-result))
              (value (second (first left-result)))
              (right-result (from-sorted right-size (second (rest left-result))))
              (right-tree (first right-result))
              (result-list (second right-result)))
         (values (<lazy-tree> :count size
                              :value value
                              :left left-tree
                              :right right-tree)
                 result-list)))))

(defmethod head ((obj lazy-tree))
  (let ((v (@value obj))
        (l (@left obj)))
    (cond
      ((nil? l) v)
      (t (head l)))))

(defmethod concat (element (obj lazy-tree-empty))
  (<lazy-tree> :count 1
               :value element))
