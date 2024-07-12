(defpackage :funcl
  (:use :cl :generics :generics-impl :arrow-macros :lazy-thunk :defunclass :lazy-queue
        :rb-tree :lazy-tree :trie)
  (:import-from :generics :realized? :force :head :tail :concat :nil? :look-for :take-out
   :compare :equal? :to-list :get-count :transform :filter)
  (:import-from :arrow-macros :->)
  (:import-from :lazy-thunk :lazy :lazy-stream)
  (:import-from :defunclass :defunclass)
  (:import-from :lazy-queue :lazy-queue)
  (:import-from :rb-tree :rb-tree)
  (:import-from :lazy-tree :lazy-tree)
  (:import-from :trie :trie)
  (:export :realized? :force :head :tail :concat :nil? :look-for :take-out :compare
   :equal? :to-list :-> :lazy :lazy-stream :defunclass :lazy-queue :rb-tree
   :lazy-tree :trie :get-count))
