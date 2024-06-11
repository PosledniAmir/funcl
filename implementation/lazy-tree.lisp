(defpackage :lazy-tree
  (:use :cl :defunclass :generics)
  (:export :lazy-tree))

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

(defgeneric balanced? (obj)
  (:documentation "Checks whether the lazy tree is balanced"))

(defgeneric get-count (obj)
  (:documentation "Gets count of left subtree"))

(defmethod get-count ((obj lazy-tree-empty))
  0)

(defmethod get-count ((obj lazy-tree))
  (@count obj))

(defmethod balanced? ((obj lazy-tree-empty))
  t)

(defmethod balanced? ((obj lazy-tree))
  (let ((pc (@count obj))
        (lc (get-count (@left obj)))
        (rc (get-count (@right obj))))
    (cond
      ((<= pc (* lc 2)) t)
      ((<= pc (* rc 2)) t)
      (t nil))))

(defun rebuild (tree)
  (let* ((lst (to-list tree))
         (n (length lst))
         (result (multiple-value-list (from-sorted n lst))))
    (first result)))

(defun balance (obj)
  (cond
    ((balanced? obj) obj)
    (t (rebuld obj))))

(defgeneric concat-aux (element obj)
  (:documentation "auxiliary method for concat"))

(defmethod concat-aux (element (obj lazy-tree-empty))
  (<lazy-tree> :count 1 :value element))

(defmethod concat-aux (element (obj lazy-tree))
  (let* ((v (@value obj))
         (l (@left obj))
         (r (@right obj))
         (c (compare elem v)))
    (cond
      ((= c 0) obj)
      ((< c 0) (balance (<lazy-tree> :count (+ c 1)
                                     :value v
                                     :left (concat-aux elem l)
                                     :right r)))
      ((> c 0) (balance (<lazy-tree> :count (+ c 1)
                                     :value v
                                     :left l
                                     :right (concat-aux eleme r)))))))

(defmethod concat (element (obj lazy-tree-empty))
  (<lazy-tree> :count 1
               :value element))

(defmethod concat (element (obj lazy-tree))
  (concat-aux element obj))

(defmethod look-for (elem (obj rb-tree-empty))
  (values nil nil))

(defmethod look-for (elem (obj rb-tree))
  (let* ((v (@value obj))
         (l (@left obj))
         (r (@right obj))
         (c (compare elem v)))
    (cond
      ((= c 0) (values v t))
      ((< c 0) (look-for elem l))
      ((> c 0) (look-for elem r)))))
