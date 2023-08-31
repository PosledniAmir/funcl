# funcl

 Persistent data structures for Common Lisp.

## Documentation

### Generic interface

Collections implemented in funcl use common interface.

#### `head (collection) -> element`

Selects the 'first' element in the collection and returns it.

#### `tail (collection) -> collection`

Returns the collection without the 'first' element.

#### `concat (element collection) -> collection`

Concatenates the element with the collections, returns a new collection.

#### `nil? (collection) -> boolean`

Checks whether the collection is empty.

#### `look-for (key collection) -> (element boolean)`

Looks for the item given by the key in the collection.

#### `take-out (key collection) -> collection`

Removes the item given by the key from the collection.

### Queue

#### `lazy-queue (&rest) -> lazy-queue`

Returns a queue collection consisting of function arguments as elements.

### Red-black tree

#### `rb-tree (&rest) -> rb-tree`

Returns a red-black tree collection consisting of function arguments as elements.

### Trie

#### `trie (&rest) -> trie`

Returns a trie collection consisting of function arguments as elements.