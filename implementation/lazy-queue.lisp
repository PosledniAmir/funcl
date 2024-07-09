(defpackage :lazy-queue
  (:use :cl :generics :lazy-thunk :defunclass)
  (:export :lazy-queue))

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


(defun make-queue (obj)
  "auxilliary function for updating queue in each step"
  (if (not (nil? (@left obj)))
      (tail (@left obj)))
  (cond
    ((<= (@right-size obj) (@left-size obj)) obj)
    (t (<lazy-queue> :left (rot (@left obj) (@right obj) (lazy nil))
                     :right (lazy nil)
                     :left-size (+ (@left-size obj) (@right-size obj))
                     :right-size 0))))

(defmethod concat (elem (obj lazy-queue))
  (make-queue (<lazy-queue> :left (@left obj)
                            :right (concat elem (@right obj))
                            :left-size (@left-size obj)
                            :right-size (+ 1 (@right-size obj)))))

(defmethod tail ((obj lazy-queue))
  (make-queue (make-instance 'lazy-queue
                             :left (tail (@left obj))
                             :right (@right obj)
                             :left-size (- (@left-size obj) 1)
                             :right-size (@right-size obj))))

(defmethod head ((obj lazy-queue))
  (head (@left obj)))

(defmethod nil? ((obj lazy-queue))
  (and
   (<= (@left-size obj) 0)
   (<= (@right-size obj) 0)))

(defun lazy-queue (&rest rest)
  "Returns a queue collection consisting of function arguments as elements."
  (reduce (lambda (x y) (concat  y x))
          rest
          :initial-value (make-instance 'lazy-queue)))

(defmethod look-for (element (obj lazy-queue))
  (cond
    ((nil? obj) (values nil nil))
    ((equal? element (head obj)) (values (head obj) t))
    (t (look-for element (tail obj)))))

(defmethod take-out (element (obj lazy-queue))
  (cond
    ((nil? obj) (make-instance 'lazy-queue))
    ((equal? element (head obj)) (tail obj))
    (t (concat (head obj) (take-out element (tail obj))))))

(defmethod get-count ((obj lazy-queue))
  (+ (@right-size obj) (@left-size obj)))

(defmethod to-list ((collection lazy-queue))
  (cond
    ((nil? collection) '())
    (t (cons (head collection) (to-list (tail collection))))))

(defmethod transform ((collection lazy-queue) function)
  (cond
    ((nil? collection) (make-instance 'lazy-queue))
    (t (concat (apply function (head collection))
               (transform (tail collection) function)))))
