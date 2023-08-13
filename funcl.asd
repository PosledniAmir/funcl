(defsystem "funcl"
  :depends-on ("bordeaux-threads")
  :components ((:file "implementation/generics")
               (:file "implementation/lazy-thunk")
               (:file "implementation/defunclass")
               (:file "implementation/lazy-queue"))
  :in-order-to ((test-op (test-op "funcl/tests"))))

(defsystem "funcl/tests"
  :depends-on ("funcl" "fiveam")
  :components ((:file "tests/funcl-suite")
               (:file "tests/lazy-thunk-tests")
               (:file "tests/defunclass-tests")
               (:file "tests/lazy-queue-tests"))
  :perform (test-op (o c) (symbol-call :fiveam `#:run! (find-symbol "FUNCL-SUITE" "LAZY-THUNK-TESTS"))))

                                        ; issues:
                                        ; tests for rb-tree
                                        ; documentation for rb-tree
                                        ; fix documentation

