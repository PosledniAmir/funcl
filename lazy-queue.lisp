(defpackage :lazy-queue
  (:use :cl :lazy-thunk :defunclass)
  (:export :lazy-queue
           :head
           :tail
           :concat
           :nil?))

(in-package :lazy-queue)

(defunclass lazy-queue ()
  ((left (lazy nil))
   (right (lazy nil))
   (left-size 0)
   (right-size 0))
  (:documentation "okasaki's lazy queue"))

(defun rot (left right acc)
  "auxilliary function for L ++ rev R"
  (cond
    ((nil? left) (lazy (cons (head right) acc)))
    (t (lazy (cons (head left)
                   (rot (tail left)
                        (tail right)
                        (lazy (cons (head right) acc))))))))

(defmethod make-queue ((obj lazy-queue))
  "auxilliary function for updating queue in each step"
  (if (not (nil? (left obj)))
      (tail (left obj)))
  (cond
    ((<= (right-size obj) (left-size obj)) obj)
    (t (make-instance 'lazy-queue
                      :left (rot (left obj) (right obj) (lazy nil))
                      :right (lazy nil)
                      :left-size (+ (left-size obj) (right-size obj))
                      :right-size 0))))

(defmethod concat (elem (obj lazy-queue))
  "adds elem to the end of the queue"
  (make-queue (make-instance 'lazy-queue
                             :left (left obj)
                             :right (concat elem (right obj))
                             :left-size (left-size obj)
                             :right-size (+ 1 (right-size obj)))))

(defmethod tail ((obj lazy-queue))
  "returns queue without the first elem"
  (make-queue (make-instance 'lazy-queue
                             :left (tail (left obj))
                             :right (right obj)
                             :left-size (- (left-size obj) 1)
                             :right-size (right-size obj))))

(defmethod head ((obj lazy-queue))
  "returns first elem in the queue"
  (head (left obj)))

(defmethod nil? ((obj lazy-queue))
  "empty check"
  (and
   (<= (left-size obj) 0)
   (<= (right-size obj) 0)))

(defun lazy-queue (&rest rest)
  (reduce (lambda (x y) (concat  y x))
          rest
          :initial-value (make-instance 'lazy-queue)))
