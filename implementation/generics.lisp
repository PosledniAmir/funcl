(defpackage :generics
  (:use :cl)
  (:export :realized? :force :head :tail :concat :nil? :look-for
   :take-out :compare :equal? :to-list :get-count :transform
   :filter))

(in-package :generics)

(defgeneric realized? (object)
  (:documentation "Checks whether the object is realized"))

(defgeneric force (object)
  (:documentation "Forces the lazy evaluation to evaluate"))

(defgeneric head (collection)
  (:documentation "Selects the 'first' element in the collection and returns it."))

(defgeneric tail (collection)
  (:documentation "Returns the collection without the 'first' element."))

(defgeneric concat (element collection)
  (:documentation "Concatenates the element with the collections, returns a new collection."))

(defgeneric nil? (collection)
  (:documentation "Checks whether the collection is empty."))

(defgeneric look-for (element collection)
  (:documentation "Looks for the item given by the key in the collection."))

(defgeneric take-out (element collection)
  (:documentation "Removes the item given by the key from the collection."))

(defgeneric compare (first second)
  (:documentation "Compares the first element to the second."))

(defgeneric equal? (first second)
  (:documentation "Tells whether the elements are equal."))

(defgeneric to-list (collection)
  (:documentation "Casts collection to a list."))

(defgeneric join (a b)
  (:documentation "Merges collections a and b."))

(defgeneric get-count (collection)
  (:documentation "Gets number of elements in the collection."))

(defgeneric transform (collection function)
  (:documentation "Applies function on every item on the collection and returns the same collection with the elements modified."))

(defgeneric filter (collection predicate)
  (:documentation "Filters the collection according to the predicate, returns the same collection with the elements filtered."))
