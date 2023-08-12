(defsystem "funcl"
  :depends-on ("bordeaux-threads")
  :components ((:file "lazy-thunk")
               (:file "defunclass")
               (:file "lazy-queue"))
  :in-order-to ((test-op (test-op "funcl/tests"))))

(defsystem "funcl/tests"
  :depends-on ("funcl" "fiveam")
  :components ((:file "lazy-thunk-tests")
               (:file "defunclass-tests")
               (:file "lazy-queue-tests"))
  :perform (test-op (o c) (symbol-call :fiveam `#:run! (find-symbol "FUNCL-SUITE" "LAZY-THUNK-TESTS"))))
