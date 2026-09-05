# Persistent Cons Object

This Git tree represents a single Lisp cons cell
within a persistent, immutable data structure.
  * **.meta**: Contains the serialized property list defining the  `:tag`, calculated `:length`, and `:proper` boolean flag for this  node and its descendants. 
  * **car**: The SHA pointer to the data or nested structure held in the `car` of this cons. 
  * **cdr**: The SHA pointer to the data or nested structure held in the `cdr` of this cons.
This 1:1 tree-to-cons mapping intentionally preserves Lisp's
native structural sharing and tail-deduplication within the Git object database.
