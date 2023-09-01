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

(defgeneric contains-value? (obj)
  (:documentation "aux function for checking whether node is non-empty"))

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
    ((contains-value? obj) (value obj))
    (t (head (car (next obj))))))

(defun make-trie (letter value next)
  "aux constructor for trie"
  (make-instance 'trie
                 :letter letter
                 :value value
                 :next next))

(defun make-no-val ()
  "aux constructor for no-value"
  (make-instance 'no-value))

(defun cond-cons (elt lst pred)
  "conditional cons"
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
  "creates trie containing text and value only"
  (let ((lst (reverse (coerce text 'list))))
    (cond
      ((null lst) (make-trie 'root value nil))
      (t (make-trie 'root
                    (make-no-val)
                    (list (reduce (lambda (x y) (cons-char-trie y x))
                                  (cdr lst)
                                  :initial-value (make-trie (car lst) value nil))))))))

(defun list-update-add (list predicate update add)
  "finds first element that evaluates predicate to t and updates is using update
if not successful it adds it using add function"
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
  "merges node into a list of nodes"
  (list-update-add list
                   (lambda (x) (equal? (letter node) (letter x)))
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
  "Returns a trie collection consisting of function arguments as elements."
  (reduce (lambda (x y) (concat y x))
          rest
          :initial-value (make-instance 'trie-empty)))

(defun list-find-call (list predicate func)
  "finds first element according to predicate
if successful returns (values val t) where val is func applied to the found element
otherwise returns (values nil nil)"
  (cond
    ((null list) (values nil nil))
    ((funcall predicate (car list)) (values (funcall func (car list)) t))
    (t (list-find-call (cdr list) predicate func))))

(defun find-in (list node)
  "finds node in the list of nodes"
  (list-find-call list
                  (lambda (x) (equal? (letter x) (letter node)))
                  (lambda (x) (cond
                                ((null (next node)) (value x))
                                (t (find-in (next x) (car (next node))))))))

(defmethod look-for (text (obj trie-empty))
  (values nil nil))

(defmethod look-for (text (obj trie))
  (find-in (list obj) (make-string-nodes text (make-no-val))))

(defun list-update-remove (lst pred update remove)
  "finds element in lst according to the predicate
if successful then updates the element according to the update function
if updated element meets remove predicate then it is removed from the list"
  (cond
    ((null lst) nil)
    ((funcall pred (car lst))
     (let ((updated (funcall update (car lst))))
       (if (funcall remove updated)
           (cdr lst)
           (cons updated (cdr lst)))))
    (t (cons (car lst)
             (list-update-remove (cdr lst) pred update remove)))))

(defun separate-from (lst node)
  "removes node from list"
  (list-update-remove lst
                      (lambda (x) (equal? (letter x) (letter node)))
                      (lambda (x) (cond
                                    ((null (next node)) (make-trie (letter x) (make-no-val) (next x)))
                                    (t (make-trie (letter x) (value x) (separate-from (next x) (car (next node)))))))
                      (lambda (x) (and (null (next x))
                                       (not (contains-value? x))))))

(defun nil-to-empty (elem)
  "transforms nil to trie-empty, otherwise returns elem"
  (cond
    ((null elem) (make-instance 'trie-empty))
    (t elem)))

(defmethod take-out (elem (obj trie-empty))
  obj)

(defmethod take-out (elem (obj trie))
  (nil-to-empty (car (separate-from (list obj) (make-string-nodes elem nil)))))

(defmethod equal? ((first symbol) (second symbol))
  (equal first second))

(defmethod equal? ((first character) (second character))
  (char= first second))
