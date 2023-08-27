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

(defun make-string-nodes (text index value)
  (cond
    ((>= (+ 1 index) (length text))
     (make-instance 'trie
                    :letter (elt text index)
                    :value value
                    :next nil))
    (t
     (make-instance 'trie
                    :letter (elt text index)
                    :next (list
                            (make-string-nodes text
                                               (+ index 1)
                                               value))))))

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
                                 ((null (next node))
                                  (make-instance 'trie
                                                 :letter (letter x)
                                                 :value (value node)
                                                 :next (next x)))
                                 (t
                                  (make-instance 'trie
                                                 :letter (letter x)
                                                 :value (value x)
                                                 :next (merge-in (next x)
                                                                 (car (next node)))))))
                   (lambda () node)))

(defun make-rooted (text index value)
  (cond
    ((= 0 (length text))
     (make-instance 'trie
                    :letter 'root
                    :next nil
                    :value value))
    (t (make-instance 'trie
                      :letter 'root
                      :value nil
                      :next (list (make-string-nodes text index value))))))

(defmethod concat (elem (obj trie-empty))
  (make-rooted (first elem) 0 (second elem)))

(defmethod concat (elem (obj trie))
  (car (merge-in (list obj)
                 (make-rooted (first elem) 0 (second elem)))))

(defun trie (&rest rest)
  (reduce (lambda (x y) (concat y x))
          rest
          :initial-value (make-instance 'trie-empty)))

(defun list-find (list predicate)
  (cond
    ((null list) (values nil nil))
    ((funcall predicate (car list))
     (values (car list) t))
    (t (list-find (cdr list) predicate))))

(defun look-for-aux (text index obj)
  (cond
    ((null obj) (values nil nil))
    ((>= (+ 1 index) (length text))
     (list-find (next obj)
                (lambda (x) (equal (elt text index)
                                   (letter x)))))
    (t (look-for-aux text
                     (+ index 1)
                     (list-find (next obj)
                                (lambda (x) (equal (elt text index)
                                                   (letter x))))))))

(defun get-value (obj)
  (if (null obj)
      (values nil nil)
      (if (contains-value? obj)
          (values (value obj) t)
          (values nil nil))))

(defmethod look-for (elem (obj trie-empty))
  (values nil nil))

(defmethod look-for (elem (obj trie))
  (cond
    ((= 0 (length elem))
     (get-value obj))
    (t (get-value (look-for-aux elem 0 obj)))))

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
