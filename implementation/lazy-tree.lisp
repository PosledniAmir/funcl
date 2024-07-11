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

(defun get-sizes (size)
  (let ((result (multiple-value-list (truncate size 2))))
    (values (- (+ (first result) (second result)) 1)
            (first result))))

(defun from-sorted (size list)
  (cond
    ((<= size 0) (values (<lazy-tree-empty>) list))
    (t (let* ((result (multiple-value-list (get-sizes size)))
              (left-size (first result))
              (right-size (second result))
              (left-pair (multiple-value-list (from-sorted left-size list)))
              (left-tree (first left-pair))
              (value (first (second left-pair)))
              (left-list (rest (second left-pair)))
              (right-pair (multiple-value-list (from-sorted right-size left-list)))
              (right-tree (first right-pair))
              (right-list (second right-pair)))
         (values (<lazy-tree> :count size
                              :value value
                              :left left-tree
                              :right right-tree)
                 right-list)))))

(defmethod head ((obj lazy-tree))
  (let ((v (@value obj))
        (l (@left obj)))
    (cond
      ((nil? l) v)
      (t (head l)))))

(defgeneric balanced? (obj)
  (:documentation "Checks whether the lazy tree is balanced"))

(defmethod @count ((obj lazy-tree-empty))
  0)

(defmethod balanced? ((obj lazy-tree-empty))
  t)

(defmethod balanced? ((obj lazy-tree))
  (let ((pc (* (/ 2 3) (@count obj)))
        (lc (@count (@left obj)))
        (rc (@count (@right obj))))
    (cond
      ((<= pc lc) t)
      ((<= pc rc) t)
      (t nil))))

(defun rebuild (tree)
  "auxilliary function that rebuilds the tree"
  (let* ((lst (to-list tree))
         (n (length lst))
         (result (multiple-value-list (from-sorted n lst))))
    (first result)))

(defun balance (obj)
  "if necessary it rebuilds the tree"
  (cond
    ((balanced? obj) obj)
    (t (rebuild obj))))

(defun recalculate-count (tree)
  "fixes cound by checking the children"
  (let ((l (@left tree))
        (r (@right tree))
        (v (@value tree)))
    (<lazy-tree> :count (+ (@count l)
                           (@count r)
                           1)
                 :value v
                 :left l
                 :right r)))

(defgeneric concat-aux (element obj)
  (:documentation "auxiliary method for concat"))

(defmethod concat-aux (element (obj lazy-tree-empty))
  (<lazy-tree> :count 1 :value element))

(defmethod concat-aux (element (obj lazy-tree))
  (let* ((v (@value obj))
         (l (@left obj))
         (r (@right obj))
         (c (compare element v)))
    (cond
      ((= c 0) obj)
      ((< c 0) (balance
                (recalculate-count
                 (<lazy-tree> :count 0
                              :value v
                              :left (concat-aux element l)
                              :right r))))
      ((> c 0) (balance
                (recalculate-count
                 (<lazy-tree> :count 0
                              :value v
                              :left l
                              :right (concat-aux element r))))))))

(defmethod concat (element (obj lazy-tree-empty))
  (<lazy-tree> :count 1
               :value element))

(defmethod concat (element (obj lazy-tree))
  (concat-aux element obj))

(defmethod look-for (elem (obj lazy-tree-empty))
  (values nil nil))

(defmethod look-for (elem (obj lazy-tree))
  (let* ((v (@value obj))
         (l (@left obj))
         (r (@right obj))
         (c (compare elem v)))
    (cond
      ((= c 0) (values v t))
      ((< c 0) (look-for elem l))
      ((> c 0) (look-for elem r)))))

(defgeneric take-out-aux (obj)
  (:documentation "Aux method for taking out"))

(defmethod take-out-aux ((obj lazy-tree-empty))
  obj)

(defmethod take-out-aux ((obj lazy-tree))
  (let* ((l (@left obj))
         (r (@right obj))
         (c (@count obj)))
    (cond
      ((not (nil? l)) (<lazy-tree> :count (- c 1)
                                   :value (@value l)
                                   :left (take-out-aux l)
                                   :right r))
      ((not (nil? r)) (<lazy-tree> :count (- c 1)
                                   :value (@value r)
                                   :left l
                                   :right (take-out-aux r)))
      (t (<lazy-tree-empty>)))))

(defmethod take-out (elem (obj lazy-tree-empty))
  obj)

(defmethod take-out (elem (obj lazy-tree))
  (let* ((v (@value obj))
         (l (@left obj))
         (r (@right obj))
         (c (compare elem v)))
    (cond
      ((= c 0) (balance (take-out-aux obj)))
      ((< c 0) (balance
                (recalculate-count
                 (<lazy-tree> :count 0
                              :value v
                              :left (take-out elem l)
                              :right r))))
      ((> c 0) (balance
                (recalculate-count
                 (<lazy-tree> :count 0
                              :value v
                              :left l
                              :right (take-out elem r))))))))

(defmethod tail ((obj lazy-tree))
  (let ((v (head obj)))
    (take-out v obj)))

(defun lazy-tree (&rest rest)
  "Returns a lazy tree, collection consisting of function arguments as elements."
  (reduce (lambda (x y) (concat y x))
          rest
          :initial-value (<lazy-tree-empty>)))

(defmethod get-count ((obj lazy-tree-empty))
  (@count obj))

(defmethod get-count ((obj lazy-tree))
  (@count obj))

(defgeneric to-list-aux (collection acc)
  (:documentation "Auxilliary function for to-list for lazy-tree"))

(defmethod to-list-aux ((collection lazy-tree-empty) acc)
  acc)

(defmethod to-list-aux ((collection lazy-tree) acc)
  (to-list-aux (@left collection)
               (cons (@value collection)
                     (to-list-aux (@right collection) acc))))

(defmethod to-list ((collection lazy-tree-empty))
  '())

(defmethod to-list ((collection lazy-tree))
  (to-list-aux collection '()))

(defgeneric transform-aux (collection function acc)
  (:documentation "Auxilliary function for transform method for lazy-tree"))

(defmethod transform-aux ((collection lazy-tree-empty) function acc)
  acc)

(defmethod transform-aux ((collection lazy-tree) function acc)
  (transform-aux (@left collection)
                 function
                 (concat (funcall function (@value collection))
                         (transform-aux (@right collection) function acc))))

(defmethod transform ((collection lazy-tree-empty) function)
  collection)

(defmethod transform ((collection lazy-tree) function)
  (transform-aux collection function (<lazy-tree-empty>)))


