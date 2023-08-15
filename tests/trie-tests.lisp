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
    (is (string= "atest" (look-for "atest" result))
        (string= "btest" (look-for "btest" result))
        (string= "ctest" (look-for "ctest" result))
        (string= "testa" (look-for "testa" result))
        (string= "testb" (look-for "testb" result))
        (string= "testc" (look-for "testc" result)))))
