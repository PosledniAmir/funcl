(defpackage :trie
  (:use :cl :defunclass :generics :trivia))

(in-package :trie)

(defunclass trie-empty () () (:documentation "empty node"))

(defunclass trie-letter ()
    ((letter (error "slot letter must be set in a trie-letter"))
     (next nil))
  (:documentation "node with no value"))

(defunclass trie (trie-letter)
    ((value (error "slot value must be set in trie")))
  (:documentation "node with value"))

(defmethod nil? ((obj trie-letter))
  nil)

(defmethod nil? ((obj trie-empty))
  t)

(defun select-next (elem index obj)
  (cond
    ((>= index (length elem)) (values nil nil))
    (t (let ((found (member-if (lambda (x) (char= (letter x) (elt elem index))) (next obj))))
         (cond
           ((null found) (values nil nil))
           (t (look-for-aux elem (+ 1 index) obj)))))))

(defgeneric look-for-aux (elem index obj)
  (:documentation "aux function for look-for in trie"))

(defmethod look-for-aux (elem index (obj trie-empty))
  (values nil nil))

(defmethod look-for-aux (elem index (obj trie-letter))
  (cond
    ((= index (length elem)) (values nil nil))
    (t (select-next elem index obj))))

(defmethod look-for-aux (elem index (obj trie))
  (cond
    ((= index (length elem)) (values (value obj) t))
    (t (select-next elem index obj))))

(defmethod look-for (elem (obj trie-empty))
  (values nil nil))

(defmethod look-for (elem (obj trie-letter))
  (look-for-aux elem 0 obj))
