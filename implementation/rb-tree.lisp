(defpackage :rb-tree
  (:use :cl :defunclass :generics :trivia)
  (:export :rb-tree))

(in-package :rb-tree)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defunclass rb-tree-empty () () (:documentation "empty node"))

  (defunclass rb-tree ()
      ((color 'black)
       (value (error "slot value must be set in rb-tree"))
       (left (<rb-tree-empty>))
       (right (<rb-tree-empty>)))
    (:documentation "okasaki functional red-black tree")))

(defmethod nil? ((obj rb-tree-empty))
  t)

(defmethod nil? ((obj rb-tree))
  nil)

(defun make-black (obj)
  "swaps color to black"
  (<rb-tree> :color 'black
             :value (@value obj)
             :left (@left obj)
             :right (@right obj)))

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

(defun balance-items (a b c d x y z)
  "balances the tree according to okasaki's paper"
  (<rb-tree> :color 'red
             :value y
             :left (<rb-tree> :color 'black
                              :value x
                              :left a
                              :right b)
             :right (<rb-tree> :color 'black
                               :value z
                               :left c
                               :right d)))

(defun balance (obj)
  "balance function implemented as in okasaki's paper"
  (match obj
    ((rb-tree :color 'black
              :value z
              :left (rb-tree :color 'red
                             :value y
                             :left (rb-tree
                                    :color 'red
                                    :value x
                                    :left a
                                    :right b)
                             :right c)
              :right d)
     (balance-items a b c d x y z))
    ((rb-tree :color 'black
              :value z
              :left (rb-tree :color 'red
                             :value x
                             :left a
                             :right (rb-tree
                                     :color 'red
                                     :value y
                                     :left b
                                     :right c))
              :right d)
     (balance-items a b c d x y z))
    ((rb-tree :color 'black
              :value x
              :left a
              :right (rb-tree :color 'red
                              :value z
                              :left (rb-tree
                                     :color 'red
                                     :value y
                                     :left b
                                     :right c)
                              :right d))
     (balance-items a b c d x y z))
    ((rb-tree :color 'black
              :value x
              :left a
              :right (rb-tree :color 'red
                              :value y
                              :left b
                              :right (rb-tree
                                      :color 'red
                                      :value z
                                      :left c
                                      :right d)))
     (balance-items a b c d x y z))
    ((rb-tree) obj)
    ((rb-tree-empty) obj)))

(defgeneric concat-aux (elem obj)
  (:documentation "aux function for rb-tree to insert an element"))

(defmethod concat-aux (elem (obj rb-tree-empty))
  (<rb-tree> :color 'red :value elem))

(defmethod concat-aux (elem (obj rb-tree))
  (let* ((v (@value obj))
         (l (@left obj))
         (r (@right obj))
         (c (compare elem v))
         (col (@color obj)))
    (cond
      ((= c 0) obj)
      ((< c 0) (balance
                (<rb-tree> :color col
                           :value v
                           :left (concat-aux elem l)
                           :right r)))
      ((> c 0) (balance
                (<rb-tree> :color col
                           :value v
                           :left l
                           :right (concat-aux elem r)))))))

(defmethod concat (elem (obj rb-tree))
  (make-black (concat-aux elem obj)))

(defmethod concat (elem (obj rb-tree-empty))
  (make-black (concat-aux elem obj)))

(defgeneric take-out-aux (obj)
  (:documentation "Aux method for taking out"))

(defmethod take-out (elem (obj rb-tree-empty))
  obj)

(defmethod take-out-aux ((obj rb-tree-empty))
  obj)

(defmethod take-out-aux ((obj rb-tree))
  (let* ((l (@left obj))
         (r (@right obj))
         (c (@color obj)))
    (cond
      ((not (nil? l)) (<rb-tree> :color c
                                 :value (@value l)
                                 :left (take-out-aux l)
                                 :right r))
      ((not (nil? r)) (<rb-tree> :color c
                                 :value (@value r)
                                 :left l
                                 :right (take-out-aux r)))
      (t (<rb-tree-empty>)))))

(defmethod take-out (elem (obj rb-tree))
  (let* ((v (@value obj))
         (l (@left obj))
         (r (@right obj))
         (c (compare elem v))
         (col (@color obj)))
    (cond
      ((= c 0) (balance (take-out-aux obj)))
      ((< c 0) (balance
                (<rb-tree> :color col
                           :value v
                           :left (take-out elem l)
                           :right r)))
      ((> c 0) (balance
                (make-instance 'rb-tree
                               :color col
                               :value v
                               :left l
                               :right (take-out elem r)))))))

(defun rb-tree (&rest rest)
  "Returns a red-black tree collection consisting of function arguments as elements."
  (reduce (lambda (x y) (concat y x))
          rest
          :initial-value (<rb-tree-empty>)))

(defmethod head ((obj rb-tree))
  (let ((v (@value obj))
        (l (@left obj)))
    (cond
      ((nil? l) v)
      (t (head l)))))

(defmethod tail ((obj rb-tree))
  (let ((v (head obj)))
    (take-out v obj)))

(defmethod get-count ((obj rb-tree-empty))
  0)

(defmethod get-count ((obj rb-tree))
  (+ (get-count (@left obj))
     (get-count (@right obj))
     1))

(defgeneric to-list-aux (collection acc)
  (:documentation "Auxilliary functionfor to-list for rb-tree"))

(defmethod to-list-aux ((collection rb-tree-empty) acc)
  acc)

(defmethod to-list-aux ((collection rb-tree) acc)
  (to-list-aux (@left collection)
               (cons (@value collection)
                     (to-list-aux (@right collection) acc))))

(defmethod to-list ((collection rb-tree-empty))
  '())

(defmethod to-list ((collection rb-tree))
  (to-list-aux collection '()))

(defgeneric transform-aux (collection function acc)
  (:documentation "Auxilliary function for transform method for rb-tree"))

(defmethod transform-aux ((collection rb-tree-empty) function acc)
  acc)

(defmethod transform-aux ((collection rb-tree) function acc)
  (transform-aux (@left collection)
                 function
                 (concat (funcall function (@value collection))
                         (transform-aux (@right collection) function acc))))

(defmethod transform ((collection rb-tree-empty) function)
  collection)

(defmethod transform ((collection rb-tree) function)
  (transform-aux collection function (<rb-tree-empty>)))

(defgeneric filter-aux (collection predicate acc)
  (:documentation "Auxilliary function for filter method for rb-tree"))

(defmethod filter-aux ((collection rb-tree-empty) predicate acc)
  acc)

(defmethod filter-aux ((collection rb-tree) predicate acc)
  (let ((take? (funcall predicate (@value collection)))
        (filtered (filter-aux (@right collection) predicate acc)))
    (filter-aux (@left collection)
                predicate
                (cond
                  (take? (concat (@value collection) filtered))
                  (t filtered)))))

(defmethod filter ((collection rb-tree-empty) predicate)
  collection)

(defmethod filter ((collection rb-tree) predicate)
  (filter-aux collection predicate (<rb-tree-empty>)))
