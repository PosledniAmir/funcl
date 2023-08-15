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
  t)

(defmethod nil? ((obj trie))
  nil)

(defmethod nil? ((obj trie-empty))
  t)

(defgeneric matches? (elem index obj)
  (:documentation "aux function checking for the letters"))

(defmethod matches? (elem index (obj trie-empty))
  nil)

(defmethod matches? (elem index (obj trie))
  (char= (elt elem index) (letter obj)))

(defun car? (list)
  (cond
    ((null list) nil)
    (t (car list))))

(defgeneric select-next (elem index obj)
  (:documentation "aux function for selecting next node"))

(defmethod select-next (elem index (obj trie-empty))
  obj)

(defmethod select-next (elem index (obj trie))
  (cond
    ((>= index (length elem)) (make-instance 'trie-empty))
    (t (let  ((result (car?
                       (member-if (lambda (x) (matches? elem index x))
                                  (next obj)))))
         (if result result (make-instance 'trie-empty))))))

(defgeneric look-for-aux (elem index obj)
  (:documentation "aux function for look-for in trie"))

(defmethod look-for-aux (elem index (obj trie-empty))
  (values nil nil))

(defmethod look-for-aux (elem index (obj trie))
  (cond
    ((>= index (length elem)) (if (contains-value? obj)
                                  (values (value obj) t)
                                  (values nil nil)))
    (t (look-for-aux elem (+ 1 index) (select-next elem index obj)))))

(defmethod look-for (elem (obj trie-empty))
  (values nil nil))

(defmethod look-for (elem (obj trie))
  (look-for-aux elem 0 obj))

(defun make-string-nodes (elem index value)
  "Aux method making string of nodes"
  (cond
    ((>= index (length elem)) nil)
    ((= index (- (length elem) 1)) (make-instance 'trie
                                                  :letter (elt elem index)
                                                  :value value
                                                  :next nil))
    (t (make-instance 'trie
                      :letter (elt elem index)
                      :next (list (make-string-nodes elem (+ 1 index) value))))))

(defgeneric concat-aux (elem index value obj)
  (:documentation "aux function for concat for trie"))

(defmethod concat-aux (elem index value (obj trie-empty))
  (make-string-nodes elem index value))

(defmethod concat-aux (elem index value (obj trie))
  (cond
    ((>= index (length elem)) (make-instance 'trie
                                             :letter (letter obj)
                                             :value value
                                             :next (next obj)))
    (t (let* ((found (select-next elem index obj)))
         (cond
           ((nil? found) (make-instance 'trie
                                        :letter (letter obj)
                                        :value (value obj)
                                        :next (cons (concat-aux elem index value found)
                                                    (next obj))))
           (t (concat-aux elem (+ 1 index) value found)))))))

(defmethod concat (elem (obj trie-empty))
  (make-instance 'trie
                 :letter 'root
                 :next (list (concat-aux (first elem) 0 (second elem) obj))))

(defmethod concat (elem (obj trie))
  (concat-aux (first elem) 0 (second elem) obj))

(defun trie (&rest rest)
  "constructor for a trie"
  (reduce (lambda (x y) (concat y x))
          rest
          :initial-value (make-instance 'trie-empty)))
