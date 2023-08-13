(defpackage :rb-tree
  (:use :cl :defunclass :generics)
  (:export :rb-tree))

(in-package :rb-tree)

(defunclass rb-tree-empty () () (:documentation "empty node"))

(defunclass rb-tree ()
    ((color 'black)
     (value (error "slot value must be set in rb-tree"))
     (left (make-instance 'rb-tree-empty))
     (right (make-instance 'rb-tree-empty)))
  (:documentation "okasaki functional red-black tree"))

(defmethod nil? ((obj rb-tree-empty))
  t)

(defmethod nil? ((obj rb-tree))
  nil)

(defun make-black (obj)
  "swaps color to black"
  (make-instance 'rb-tree
                 :color 'black
                 :value (value obj)
                 :left (left obj)
                 :right (right obj)))

(defmethod look-for (elem (obj rb-tree-empty))
  (values nil nil))

(defmethod look-for (elem (obj rb-tree))
  (let* ((v (value obj))
         (l (left obj))
         (r (right obj))
         (c (compare elem v)))
    (cond
      ((= 0 c) (values v t))
      ((< 0 c) (look-for elem l))
      ((> 0 c) (look-for elem r)))))

(defun balance-items (a b c d x y z)
  (make-instance 'rb-tree
                 :color 'red
                 :value y
                 :left (make-instance 'rb-tree
                                      :color 'black
                                      :value x
                                      :left a
                                      :right b)
                 :right (make-instance 'rb-tree
                                       :color 'black
                                       :value z
                                       :left c
                                       :right d)))

(defun balance-black-red (obj)
  (cond
    ((equal 'red (left (left obj)))
     (balance-items (left (left (left obj)))
                    (right (left (left obj)))
                    (right (left obj))
                    (right obj)
                    (value (left (left obj)))
                    (value (left obj))
                    (value obj)))
    (t
     (balance-items (left (left obj))
                    (left (right (left obj)))
                    (right (right (left obj)))
                    (right obj)
                    (value (left obj))
                    (value (right (left obj)))
                    (value obj)))))

(defun balance-black-black (obj)
  (cond
    ((equal 'red (left (right obj)))
     (balance-items (left obj)
                    (left (left (right obj)))
                    (right (left (right obj)))
                    (right (right obj))
                    (value obj)
                    (value (left (right obj)))
                    (value (right obj))))
    (t
     (balance-items (left obj)
                    (left (right obj))
                    (left (right (right obj)))
                    (right (right (right obj)))
                    (value obj)
                    (value (right obj))
                    (value (right (right obj)))))))

(defun balance-black (obj)
  (cond
    ((equal 'red (left obj)) (balance-black-red obj))
    (t (balance-black-black obj))))

(defun balance (obj)
  (cond
    ((equal 'red (color obj)) obj)
    (t (balance-black obj))))

(defgeneric insert (elem obj)
  (:documentation "help method for rb-tree to insert an element"))

(defmethod insert (elem (obj rb-tree-empty))
  (make-instance 'rb-tree
                 :color 'red
                 :value elem))

(defmethod insert (elem (obj rb-tree))
  (let* ((v (value obj))
         (l (left obj))
         (r (right obj))
         (c (compare elem v))
         (col (color obj)))
    (cond
      ((= 0 c) obj)
      ((< 0 c) (balance
                (make-instance 'rb-tree
                               :color col
                               :value v
                               :left (insert elem l)
                               :right r)))
      ((> 0 c) (balance
                (make-instance 'rb-tree
                               :color col
                               :value v
                               :left l
                               :right (insert elem r)))))))

(defmethod concat (elem (obj rb-tree))
  (make-black (insert elem obj)))

(defmethod concat (elem (obj rb-tree-empty))
  (make-black (insert elem obj)))

(defgeneric take-out-aux (obj)
  (:documentation "Aux method for taking out"))

(defmethod take-out (elem (obj rb-tree-empty))
  obj)

(defmethod take-out-aux ((obj rb-tree-empty))
  obj)

(defmethod take-out-aux ((obj rb-tree))
  (let* ((l (left obj))
         (r (right obj))
         (c (color obj)))
    (cond
      ((not (nil? l)) (make-instance 'rb-tree
                                     :color c
                                     :value (value l)
                                     :left (take-out-aux l)
                                     :right r))
      ((not (nil? r)) (make-instance 'rb-tree
                                     :color c
                                     :value (value r)
                                     :left l
                                     :right (take-out-aux r)))
      (t (make-instance 'rb-tree-empty)))))

(defmethod take-out (elem (obj rb-tree))
  (let* ((v (value obj))
         (l (left obj))
         (r (right obj))
         (c (compare elem v))
         (col (color obj)))
    (cond
      ((= 0 c) (balance (take-out-aux obj)))
      ((< 0 c) (balance
                (make-instance 'rb-tree
                               :color col
                               :value v
                               :left (take-out elem l)
                               :right r)))
      ((> 0 c) (balance
                (make-instance 'rb-tree
                               :color col
                               :value v
                               :left l
                               :right (take-out elem r)))))))

(defun rb-tree (&rest rest)
  "constructor for a red black tree"
  (reduce (lambda (x y) (concat y x))
          rest
          :initial-value (make-instance 'rb-tree-empty)))

(defmethod head ((obj rb-tree))
  (value obj))

(defmethod tail ((obj rb-tree))
  (let ((v (value obj)))
    (take-out v obj)))
