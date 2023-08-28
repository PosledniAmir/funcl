(defpackage :trie
  (:use :cl :defunclass :generics :trivia)
  (:export :trie))

(in-package :trie)

(defunclass no-value () () (:documentation "empty"))

(defunclass trie-empty () () (:documentation "empty node"))

(defunclass trie ()
    ((letter (error "slot letter must be set in a trie"))
     (value (make-instance 'no-value))
     (next nil))
  (:documentation "node with no value"))

(defmethod contains-value? ((obj trie-empty))
  nil)

(defmethod contains-value? ((obj trie))
  (not (typep (value obj) 'no-value)))

(defmethod nil? ((obj trie))
  nil)

(defmethod nil? ((obj trie-empty))
  t)

(defmethod head ((obj trie))
  (cond
    ((contains-value? obj) (get-value obj))
    (t (head (car (next obj))))))

(defun make-trie (letter value next)
  (make-instance 'trie
                 :letter letter
                 :value value
                 :next next))

(defun make-no-val ()
  (make-instance 'no-value))

(defun cond-cons (elt lst pred)
  (cond
    ((funcall pred elt) (cons elt lst))
    (t lst)))

(defmethod tail ((obj trie))
  (cond
    ((contains-value? obj) (make-trie (letter obj) (make-no-val) (next obj)))
    (t (make-trie (letter obj)
                  (value obj)
                  (cond-cons (tail (car (next obj)))
                             (cdr (next obj))
                             (lambda (x) (or (contains-value? x)
                                             (not (null (next x))))))))))

(defun cons-char-trie (char trie)
  (make-trie char (make-no-val) (list trie)))

(defmethod make-string-nodes (text value)
  (let ((lst (reverse (coerce text 'list))))
    (cond
      ((null lst) (make-trie 'root value nil))
      (t (make-trie 'root
                    (make-no-val)
                    (list (reduce (lambda (x y) (cons-char-trie y x))
                                  (cdr lst)
                                  :initial-value (make-trie (car lst) value nil))))))))

(defun list-update-add (list predicate update add)
  (cond
    ((null list) (cons (funcall add) nil))
    ((funcall predicate (car list)) (cons (funcall update (car list))
                                          (cdr list)))
    (t (cons (car list)
             (list-update-add (cdr list)
                              predicate
                              update
                              add)))))

(defun merge-in (list node)
  (list-update-add list
                   (lambda (x) (equal (letter node) (letter x)))
                   (lambda (x) (cond
                                 ((null (next node)) (make-trie (letter x) (value node) (next x)))
                                 (t (make-trie (letter x) (value x) (merge-in (next x) (car (next node)))))))
                   (lambda () node)))

(defmethod concat (elem (obj trie-empty))
  (let ((text (first elem))
        (value (second elem)))
    (make-string-nodes text value)))

(defmethod concat (elem (obj trie))
  (let ((text (first elem))
        (value (second elem))
        (lst (list obj)))
    (car (merge-in lst (make-string-nodes text value)))))

(defun trie (&rest rest)
  (reduce (lambda (x y) (concat y x))
          rest
          :initial-value (make-instance 'trie-empty)))

(defun list-find (list predicate)
  (cond
    ((null list) (values nil nil))
    ((funcall predicate (car list)) (values (car list) t))
    (t (list-find (cdr list) predicate))))

(defun find-in (list node)
  (list-find list
             (lambda (x) (and (equal (letter x) (letter node))
                              (or (null (next node))
                                  (find-in (next x) (car (next node))))))))

(defmethod look-for (text (obj trie-empty))
  (values nil nil))

(defmethod look-for (text (obj trie))
  (find-in (list obj) (make-string-nodes text (make-no-val))))

(defun get-value (obj)
  (if (null obj)
      (values nil nil)
      (if (contains-value? obj)
          (values (value obj) t)
          (values nil nil))))

(defun list-remove (list predicate)
  (cond
    ((null list) nil)
    ((funcall predicate (car list)) (cdr list))
    (t (cons (car list) (list-remove (cdr list) predicate )))))

(defun find-and-remove (list predicate)
  (list
   (list-find list predicate)
   (list-remove list predicate)))

(defun clean-pair (pair clean)
  (let ((first (first pair))
        (second (second pair)))
    (cond
      ((null first) second)
      (t (let ((cleaned (funcall clean first)))
           (cond
             ((or (contains-value? cleaned)
                  (not (null (next cleaned))))
              (cons cleaned second))
             (t second)))))))

(defun take-out-aux (text index node)
  (cond
    ((>= (+ 1 index) (length text))
     (make-instance 'trie
                    :letter (letter node)
                    :value (value node)
                    :next (clean-pair (find-and-remove (next node)
                                                       (lambda (x)
                                                         (equal (elt text index)
                                                                (letter x))))
                                      (lambda (x) (make-instance 'trie
                                                                 :letter (letter x)
                                                                 :value (make-instance 'no-value)
                                                                 :next (next x))))))
    (t
     (make-instance 'trie
                    :letter (letter node)
                    :value (value node)
                    :next (clean-pair (find-and-remove (next node)
                                                       (lambda (x)
                                                         (equal (elt text index)
                                                                (letter x))))
                                      (lambda (x) (take-out-aux text (+ 1 index) x)))))))

(defmethod take-out (text (obj trie-empty))
  obj)

(defmethod take-out (text (obj trie))
  (cond
    ((= 0 (length text))
     (make-instance 'trie
                    :letter 'root
                    :value (make-instance 'no-value)
                    :next (next obj)))
    (t (take-out-aux text 0 obj))))
