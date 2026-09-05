# GitHack Project Instructions & Reference Context

## Project Overview
`GitHack` is a Common Lisp persistent object database built directly on top
of Git. It shells out to the `git` executable (via `uiop:run-program`) to
use Git's content-addressable object store as an immutable backing store
for persistent Lisp data structures, a CLOS metaobject-protocol
integration, and a transaction layer with configurable concurrency-conflict
handling.

### Main Technologies
- **Language**: Common Lisp (specifically targeting Steel Bank Common
  Lisp, **SBCL**).
- **Git Engine**: No `libgit2`/CFFI bindings — every Git operation shells
  out to the `git` executable via `uiop:run-program` (`git-io.lisp`,
  `git-branch.lisp`).
- **Key Dependencies**: `alexandria`, `fold`, `function`, `named-let`,
  `series`, `sb-mop` (SBCL Metaobject Protocol), and `fiveam` (tests only).

---

## Architectural Breakdown & Core Components

1. **Git Shell-Out Layer (`git-io.lisp`, `git-branch.lisp`)**
   - `git-hash-object`/`git-type`/`git-cat-file` (`git-io.lisp`) write and
     read raw Git objects by shelling out to `git hash-object`/`git
     cat-file`.
   - `git-show-ref-sha`/`git-update-ref` (`git-branch.lisp`) read and
     compare-and-swap-write a branch ref via `git show-ref`/`git
     update-ref`.

2. **The `git-object` Proxy Layer**
   - **`git-object.lisp`**: the base `git-object` class — a lazily-loadable
     proxy keyed by a SHA, with `inflate-git-proxy` as the factory that
     turns a bare SHA into the right concrete proxy subclass (dispatching
     on `git-type`).
   - **`git-blob.lisp`**: a leaf value — any serializable Lisp atom.
   - **`git-tree.lisp`**: a directory node — an alist of filename →
     `git-object` entries, serialized/deserialized to/from Git's strict
     binary tree format.
   - **`git-commit.lisp`**: a snapshot — a root `git-tree`, parent commits,
     author/committer/timestamp/message, serialized to/from Git's
     plain-text commit format.
   - **`git-branch.lisp`**: a mutable pointer (no SHA of its own) onto a
     `git-commit`, backed by `refs/heads/<name>`.

3. **Persistent Data Structures** (each a `git-tree` subtype)
   - **`persistent-cons.lisp`**: a cons cell as a tree (`car` blob, `cdr`
     tree).
   - **`persistent-vector.lisp`**: built on persistent cons chains
     (buckets) plus a length entry.
   - **`persistent-array.lisp`**: a flattened row-major persistent vector
     plus its dimensions.
   - **`persistent-hash-table.lisp`**: a `persistent-vector` of buckets,
     each bucket a `persistent-cons` alist; defined via
     `define-persistent-struct`, with no custom Git serialization of its
     own.
   - **`atomic-wrapper.lisp`**: wraps a bare atomic `git-blob` root in a
     three-entry `git-tree` (since a commit must always point at a tree).

4. **MOP Integration (`persistent-standard-class.lisp`, `persistent-struct.lisp`)**
   - `persistent-standard-class` is a `standard-class` subclass with custom
     direct/effective slot definitions supporting a `:transient` slot
     option (excluded from persistence). `slot-value-using-class`
     transparently resolves any `git-proxy` found in a slot and caches the
     result.
   - `define-persistent-struct` (`persistent-struct.lisp`) gives this the
     syntactic convenience of `defstruct`, generating the `defclass`,
     `make-<name>` constructor, and `<name>-p` predicate.

5. **Repository and Transaction System**
   - **`git-repository.lisp`**: `call-with-repository`/`with-repository`
     bind `*repository*` (a `git-repository` — pathname plus default
     branch/author/committer/message/mode) around a receiver.
   - **`git-transaction.lisp`**: `call-with-git-transaction`/
     `with-git-transaction` resolve a branch to its head commit, invoke a
     receiver with a transient `git-transaction` and that head, and (for
     `:read-write`) automatically persist the receiver's returned root,
     create a new commit, and advance the branch.
   - **`transaction-lock.lisp`**: `with-repository-transaction-lock`, a
     portable OS-level lock-file mechanism (`CL:OPEN` with `:if-exists
     nil`).
   - **`transaction.lisp`**: `call-with-transaction`/`with-transaction`,
     the user-facing wrapper that hides SHAs/`git-blob`/`git-tree`/
     `git-commit` entirely behind a single plain Lisp value per
     transaction.
   - All three of `call-with-git-transaction`/`call-with-transaction`
     accept a `:conflict-resolution` keyword (`:error` default, `:retry`,
     `:lock`) resolving the "Lost Update" problem via `git update-ref`'s
     own compare-and-swap check and the `concurrent-modification-error`
     condition.

---

## Prerequisites, Building, and Running

### Prerequisites
1. **Steel Bank Common Lisp (SBCL)**: the codebase is highly optimized for
   and depends on SBCL-specific extensions (`sb-ext` and `sb-mop`).
2. **The `git` executable** on `PATH`: GitHack has no `libgit2`/CFFI
   bindings; every Git operation shells out to `git` via
   `uiop:run-program`.
3. **Quicklisp**: used for managing dependencies (`alexandria`, `fold`,
   `function`, `named-let`, `series`, `fiveam`).

### System Loading
Load the ASDF system by running inside SBCL:
```lisp
(asdf:load-asd (truename "githack.asd"))
(ql:quickload :githack)
```

### Running Tests
All tests are implemented as a FiveAM suite in the `githack/test` ASDF
system (a second `defsystem` inside `githack.asd`).
```lisp
(ql:quickload :fiveam)
(asdf:load-asd (truename "githack.asd"))
(ql:quickload :githack)
(asdf:test-system :githack)   ; delegates to githack/test via :in-order-to
```
This prints a FiveAM report and signals a Lisp error if any test fails.
Tests live in `test-package.lisp` (package `GITHACK-TEST`, suite
`GITHACK-SUITE`), `test-helpers.lisp` (shared fixtures/mocks), and one
`<topic>-tests.lisp` file per area under test, each adding a sub-suite via
`(def-suite ... :in githack-suite)`. To run just one sub-suite or test
interactively: `(fiveam:run! 'git-tree-suite)` or `(fiveam:run! 'git-branch-instantiates)`.

---

## Development & Contribution Conventions

- **SBCL Specifics**: Do not introduce non-SBCL constructs or abstract
  away MOP interactions unless carefully wrapping them under appropriate
  feature-flags. Stick strictly to standard `sb-mop` and `sb-ext` patterns
  already used.
- **Immutability and Pure Updates**: Nearly all operations are functionally
  pure, producing replacement SHAs/roots rather than in-place mutations.
  Ensure any new data structures follow this persistent paradigm; the only
  acceptable mutation is a proxy's own lazy-load cache slot on the specific
  instance being read (see `git-object.lisp`'s `sha`/loaded-state
  protocol).
- **Naming**: use `GET-<slot-name>` for CLOS `:reader`/`:accessor` slot
  names (never a `<class-name>-<slot-name>` `defstruct`-style prefix). A
  leading `%` marks an internal/low-level helper not meant for use outside
  its defining file.
- **Format and Style**: keep the typical Emacs/Slime indentation, Lisp
  header comments, and declare precise dependency lists within
  `githack.asd` when adding new files.

See `TECHNICAL_DEBT.md` for a prioritized, actively-maintained list of
known gaps and their suggested remediation order.
