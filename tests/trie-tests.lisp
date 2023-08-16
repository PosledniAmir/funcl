(defpackage :trie-tests
  (:use :cl :fiveam :trie :generics :funcl-suite))

(in-package :trie-tests)

(def-suite* trie-tests :in funcl-suite)

(test trie-test
  (let ((result (trie (list "atest" "atest")
                      (list "btest" "btest")
                      (list "ctest" "ctest")
                      (list "testa" "testa")
                      (list "testb" "testb")
                      (list "testc" "testc"))))
    (is (string= "atest" (look-for "atest" result)))))
