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

(test concat-method
  (let ((result (concat (list "three" 3)
                        (concat (list "one" 1)
                                (trie (list "two" 2)
                                      (list "four" 4))))))
    (is (= 1 (look-for "one" result))
        (= 2 (look-for "two" result))
        (= 3 (look-for "three" result))
        (= 4 (look-for "four" result)))))

(test nil-method
  (let ((result (trie)))
    (is (nil? result))))

(test look-for-method
  (let ((result (trie (list "one"   1)
                      (list "two"   2)
                      (list "three" 3)
                      (list "four"  4)
                      (list "five"  5)
                      (list "six"   6)
                      (list "seven" 7)
                      (list "eight" 8)
                      (list "nine"  9))))
    (is (look-for "one"   result)
        (look-for "two"   result)
        (look-for "three" result)
        (look-for "five"  result)
        (look-for "nine"  result)
        (not (look-for "eleven" result))
        (not (look-for "teen"   result))
        (not (look-for "zero"   result)))))

(test take-out-method
  (let ((result (take-out "one"
                          (take-out "nine"
                                    (take-out "five"
                                              (trie (list "one" 1)
                                                    (list "two" 2)
                                                    (list "three" 3)
                                                    (list "four" 4)
                                                    (list "five" 5)
                                                    (list "six" 6)
                                                    (list "seven" 7)
                                                    (list "eight" 8)
                                                    (list "nine" 9)))))))
    (is (not (look-for "one" result))
        (not (look-for "nine" result))
        (not (look-for "five" result))
        (look-for "two" result)
        (look-for "three" result)
        (look-for "four" result))))
