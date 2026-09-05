# Persistent Vector Object

This Git tree represents a 1D persistent vector (array).
  * **.meta**: Contains the serialized property list defining the `:tag`, `:length`, and `:element-type`.
  * **[0...N-1]**: Files named with integer indices. Each contains the SHA pointer
    to the data or nested structure at that index within the vector.
This flat tree structure allows for O(1) length lookups via `.meta` and efficient
lazy-loading of specific indices upon access.
