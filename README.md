# githack
Bindings to libgit

## Persistent lists

`PERSISTENT-CONS` stores a Lisp value and a persistent CDR as a Git tree and
returns its SHA. `PERSISTENT-CAR` retrieves the Lisp value, and
`PERSISTENT-CDR` retrieves the next tree SHA. `PERSISTENT-NULL` returns the SHA
of the canonical empty tree.

Use `LIST->PERSISTENT-CONS` to persist a proper Lisp list and
`PERSISTENT-CONS->LIST` to resolve the resulting linked tree objects.

## Persistent weight-balanced trees

`PERSISTENT-NODE` objects wrap immutable Git tree nodes containing key, value,
weight, and child references. `PERSISTENT-WTTREE-TABLE` and
`PERSISTENT-IMMUTABLE-WTTREE` implement the `table` protocol with those nodes.
Every update writes only changed paths and retains the previous root.

Create tables with `MAKE-PERSISTENT-WTTREE-TABLE` or
`MAKE-PERSISTENT-IMMUTABLE-WTTREE`. `PERSISTENT-WTTREE-SHA` returns a root that
can later be supplied as `:REPRESENTATION` to either constructor.

## Persistent vectors

`MAKE-PERSISTENT-VECTOR` stores vector indexes in a persistent WTTREE. Its root
Git tree links that contents tree to a separate tree containing the vector
length. `PERSISTENT-VECTOR-REF` and `PERSISTENT-VECTOR-AREF` read elements.

`PERSISTENT-VECTOR-UPDATE` returns a new vector with one index replaced.
`PERSISTENT-VECTOR-PUSH-EXTEND` returns the grown vector and appended index as
two values. Reload a vector using `PERSISTENT-VECTOR-FROM-SHA`.

## Persistent hash tables

`MAKE-PERSISTENT-HASH-TABLE` creates an immutable hash table whose bucket array
is a persistent vector and whose collision buckets are persistent CONS chains.
The root tree persists the entry count and `EQL`, `EQUAL`, or `EQUALP` test.

Use `PERSISTENT-GETHASH`, `PERSISTENT-PUTHASH`, and `PERSISTENT-REMHASH`.
Updates return new tables while retaining prior roots. Tables can be reopened
from `PERSISTENT-HASH-TABLE-SHA` with `PERSISTENT-HASH-TABLE-FROM-SHA`.
