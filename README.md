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

## Change identifiers

`CID-OBJECT` persistently associates a distributed identifier with an optional
positive local CID. `CID-SET` is a repository-scoped persistent integer set
with immutable union, intersection, exclusive-or, adjoin, and remove
operations.

`CID-DETAIL-TABLE` records changed objects and a unique persistent vector of
modified slot names for each object. `CID-DETAIL-TABLE/LOG-CHANGE` returns the
updated table and affected entry without modifying prior roots.

`DENSE-CID-SET` is the concrete Git-backed CID-set representation.
`CANONICAL-CLASS-DICTIONARY` partitions canonical objects by class and reuses an
equal existing object instead of adding a duplicate reference.

## Distributed identifiers and mappers

`CANONICAL-IDENTIFIER` persists ordered identifier components.
`DISTRIBUTED-IDENTIFIER` provides validated domain, repository, class, and
numeric-ID components with canonical list and string forms and parsing.

`UNORDERED-MAPPER` and `ORDERED-MAPPER` provide immutable persistent hash and
vector indexes. Mapper prefixes encode their hierarchy without cyclic Git
links, and `DISTRIBUTED-IDENTIFIER/RESOLVE` walks that hierarchy to a mapped
object.

`INTEGER-RANGE-MAPPER` allocates positive local integers in contiguous ranges
and translates them to repository-specific remote integers. Range changes and
remote mappings produce replacement roots, and mappers can be restored by SHA.

`DISTRIBUTED-OBJECT` provides persistent repository-mapper and numeric-ID
identity. `CORE-USER` is its concrete user type; creating one immutably installs
or extends the repository's class mapper so its DID resolves after SHA reload.

## Versioned values

`NONLOGGED-VERSIONED-VALUE` and `LOGGED-VERSIONED-VALUE` retain one current
value and CID. `SCALAR-VERSIONED-VALUE` stores an ordered history of CID/value
pairs plus an optional default. `VERSIONED-VALUE/VIEW` selects the newest value
visible in a CID set, and `VERSIONED-VALUE/UPDATE` returns a new immutable root.

Use the CID list, set, membership, and most-recent-CID accessors to inspect
history. `VERSIONED-VALUE-FROM-SHA` restores the appropriate concrete subtype.

## Versioned objects

`VERSIONED-STANDARD-CLASS` adds a `:VERSION-TECHNIQUE` slot option for persistent
scalar, logged, nonlogged, CVI, and CVFILE values. `VERSIONED-STANDARD-OBJECT`
wraps initial slot values, resolves reads against the active transaction view,
and routes writes through update transactions while retaining raw-value access
through `SLOT-VALUE-UNVERSIONED`.

Objects created in update transactions retain their birth CID object. Slot and
object CID-set accessors aggregate the changes represented by their persistent
versioned values.

## CID master tables

`CID-MASTER-TABLE-ENTRY` persists a CID's basis set, author, timestamps, reason,
change information, and detail table. Detail logging and finish notification
return replacement entries while preserving earlier roots.

`CID-MASTER-TABLE` maintains sparse persistent-vector indexes for entries and
CID objects. It supports immutable insertion, CID lookup and metadata queries,
active-CID selection by comparison timestamp, and reload by SHA.

## Composite version indexes

`CVI` stores sequence changes as immutable, CID-tagged insertion and deletion
records. Inserted elements receive stable integer object numbers (IONs), so
`CVI/RECONSTRUCT-VALUE` can combine any active CID set while retaining ordering
anchors from inactive changes. Empty-but-bound and unbound views remain
distinct.

`CVI/UPDATE` accepts an explicit basis CID set for branch-aware updates.
`VERSIONED-VALUE/UPDATE` uses all prior CVI changes as its linear basis.
Individual insertion, deletion, and change records are Git trees and can be
reloaded independently by SHA.

## Composite version files

`CVFILE` stores immutable file snapshots together with persistent vectors that
map repository CIDs to file CIDs. Views select the newest snapshot visible in a
CID set; same-CID updates replace that mapping without changing prior roots.

Use `CVFILE/MAP-CID-SET` to inspect mapped file CIDs and `CVFILE/GUID` or
`CVFILE/FUID` for stable file identity. CVFILE objects implement the complete
versioned-value query, view, update, and SHA-reload protocol.

## Repositories

`REPOSITORY` is an immutable `REPOSITORY-PERSISTENT-INFORMATION` Git tree
containing repository identity and type, the canonical class dictionary, root,
local, and CID mappers, the CID master table, locally named roots, satellite
metadata, parent metadata, and anonymous-user identity. Repository updates
return replacement roots and preserve earlier SHAs.

CID allocation reserves an ordered-mapper entry and creates the corresponding
CID object. Successful update transactions later install its completed master
entry and CID-object index atomically. `REPOSITORY/SAVE` and `REPOSITORY/LOAD`
publish or recover a repository root through the low-level Git transaction.

## Repository transactions

`TXN` and its repository-transaction subclasses provide nonversioned,
versioned, comparison, and update contexts. Update transactions allocate a CID,
expose a CID set containing that CID, collect changed-object slot details, and
finalize timestamps, change-set metadata, and repository state on commit.
Aborted transactions discard the replacement repository root.

`CALL-WITH-REPOSITORY-TRANSACTION` manages the low-level Git transaction and
per-user LIFO transaction stack. `CALL-WITH-BEFORE-VIEW` and
`CALL-WITH-AFTER-VIEW` select comparison views, while
`REPOSITORY-TRANSACTION/UPDATE-VERSIONED-VALUE` applies the appropriate
versioning protocol and logs the change.
