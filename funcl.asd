(defsystem "funcl"
  :version "0.0.1"
  :license "MIT"
  :depends-on ("bordeaux-threads"
               "trivia")
  :components ((:file "implementation/generics")
               (:file "implementation/generics-impl" :depends-on ("generics"))
               (:file "implementation/lazy-thunk" :depends-on ("generics" "generics-impl"))
               (:file "implementation/defunclass" :depends-on ("generics" "generics-impl"))
               (:file "implementation/lazy-queue" :depends-on ("lazy-thunk" "defunclass" "generics" "generics-impl"))
               (:file "implementation/rb-tree" :depends-on ("defunclass" "generics" "generics-impl"))
               (:file "implementation/trie" :depends-on ("defunclass" "generics")))
  :in-order-to ((test-op (test-op "funcl/tests"))))

(defsystem "funcl/tests"
  :depends-on ("funcl" "fiveam")
  :components ((:file "tests/funcl-suite")
               (:file "tests/lazy-thunk-tests")
               (:file "tests/defunclass-tests")
               (:file "tests/lazy-queue-tests")
               (:file "tests/rb-tree-tests")
               (:file "tests/trie-tests"))
  :perform (test-op (o c) (symbol-call :fiveam `#:run! (find-symbol "FUNCL-SUITE" "FUNCL-SUITE"))))
