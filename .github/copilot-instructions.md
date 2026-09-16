# GitHack

GitHack is a Common Lisp (SBCL) persistent object-database built directly on Git's
content-addressable storage. It binds `libgit2` via CFFI and uses immutable Git blobs/trees
to implement persistent data structures, versioned values, and a full change-tracking/MOP
layer, so that ordinary CLOS slot updates become immutable, historied Git commits.

## Building and running tests

There is no separate build step and no lint tooling in this repo — everything is loaded and
exercised interactively in SBCL via ASDF/Quicklisp.

```lisp
(asdf:load-asd (truename "githack.asd"))
(ql:quickload :githack)
```

Requires a `libgit2` shared library available as `git2.dll` on `PATH`, or at the hardcoded
fallback path `d:\lib\git2.dll` (see `define-foreign-library libgit2` in `githack.lisp`).

### FiveAM test suite (`githack/test`)

New code should get FiveAM tests in the `githack/test` ASDF system (a second `defsystem`
inside `githack.asd`, per ASDF's rule that a `name/subsystem` must live in `name.asd`).
**Convention: run this after every change, and it must be 100% green (and everything must
compile with zero warnings/failures) before committing:**

```lisp
(ql:quickload :fiveam)
(asdf:load-asd (truename "githack.asd"))
(ql:quickload :githack)
(asdf:test-system :githack)   ; delegates to githack/test via :in-order-to
```

This prints a FiveAM report and signals a Lisp error if any test fails (see `:perform
(test-op ...)` in `githack.asd`), so a non-zero/error result from that form means the suite
failed. Tests live in `test-package.lisp` (package `GITHACK-TEST`, suite `GITHACK-SUITE`),
`test-helpers.lisp` (shared fixtures/mocks — kept in their own component so files that use a
macro are compiled after it, avoiding spurious same-image redefinition warnings), and one
`<topic>-tests.lisp` file per area under test (e.g. `git-object-tests.lisp`), each adding a
sub-suite via `(def-suite ... :in githack-suite)`. To run just one sub-suite or test
interactively: `(fiveam:run! 'git-object-suite)` or `(fiveam:run! 'git-object-is-abstract)`.
`GITHACK-TEST` only imports the specific `GITHACK` symbols it needs (not `:use`), since
`GITHACK` shadows several `COMMON-LISP` symbols (see Conventions below) that would conflict
with `FIVEAM`'s own `:use` of `COMMON-LISP`.

The older `tests.lisp` (raw `assert` forms, ~22+ tests, run via `(load "tests.lisp")` then
`(run-githack-tests)`) predates the FiveAM suite and still exercises the low-level
`libgit2`/persistent-structure code paths; it is not part of any ASDF system.

```lisp
(load "tests.lisp")
(run-githack-tests)
```

Tests are plain `assert` forms inside one large `run-githack-tests` defun — there is no test
framework, no test runner CLI, and no way to run a single test in isolation. To exercise one
scenario, either comment out the surrounding asserts temporarily or evaluate the relevant
`let`/`assert` block directly at the REPL. Tests create a temporary bare repo under
`uiop:default-temporary-directory` and clean it up afterward; some low-level tests also shell
out to the `git` executable via `uiop:run-program` to cross-check state.

Note: `githack.lisp` also references a hardcoded `*repository-pathname*` of `D:\GitHack\` for
some low-level repository-open helpers — this is Windows/dev-machine specific, not portable.

## Architecture

The system is a layered stack (see `githack.asd` for the authoritative `:depends-on` graph);
understanding a change usually requires reading the layer below and above it:

1. **`githack.lisp`** — raw CFFI bindings to `libgit2` (blobs, trees, oids, transactions at
   the branch-ref level). Everything above this file never touches `libgit2` directly.
2. **Persistent data structures**, each implemented as Git trees, each layer building on the
   last:
   - `persistent-wttree.lisp` — weight-balanced trees implementing the `table` protocol.
   - `persistent-vector.lisp` — built on wttrees (index tree + separate length tree).
   - `persistent-hash-table.lisp` — built on persistent vectors (buckets) + persistent cons
     chains (collisions).
   - Persistent cons cells live in `githack.lisp` itself (tree with `car` blob / `cdr` tree).
3. **Identity & change tracking**:
   - `identifier.lisp` / `mapper.lisp` / `integer-mapper.lisp` / `distributed-object.lisp` —
     hierarchical distributed identifiers (DIDs) and mappers that resolve them, deliberately
     avoiding cyclic Git links (a tree can't point to something that points back to it).
   - `cid-object.lisp` / `cid-set.lisp` / `cid-master-table.lisp` / `cid-detail-table.lisp` —
     "CID" = change identifier; every update allocates one, and persistent CID sets track
     which changes are visible from a given view.
4. **Versioned values** (`versioned-value.lisp`, `cvi.lisp`, `cvfile.lisp`) — the actual
   history-aware value containers (`nonlogged`, `logged`, `scalar`, composite-sequence via
   `CVI`, composite-file via `CVFILE`). All resolve "current value" by intersecting their
   internal history against a caller-supplied CID set (a "view").
5. **MOP integration** (`versioned-object.lisp`) — `VERSIONED-STANDARD-CLASS` (a
   `sb-mop:standard-class` subclass) lets ordinary `defclass` slots declare
   `:version-technique` (`:nonversioned`, `:scalar`, `:logged`, `:nonlogged`,
   `:composite-set`, `:composite-sequence`, `:composite-file`); slot reads/writes are
   transparently routed through the active transaction's CID view.
6. **`repository.lisp`** — ties everything together: one immutable Git tree holds the
   canonical class dictionary, root/local/CID mappers, the CID master table, named roots,
   satellite repos, and the anonymous user. Every repository operation returns a *new* root
   SHA rather than mutating in place.
7. **`txn.lisp`** — `CALL-WITH-REPOSITORY-TRANSACTION` and the transaction class hierarchy
   (nonversioned / versioned / comparison / update) that allocate CIDs, collect changed-slot
   details, and commit or discard the replacement repository root atomically.

Load order is dependency-driven and declared explicitly per-file in `githack.asd`'s
`:depends-on` lists — when adding a new file, add it there too.

## Conventions

- **Everything is persistent/immutable.** Operations that "update" a structure return a new
  SHA/root and leave prior roots valid and independently loadable — never mutate in place.
  Every persistent type follows the same round-trip triplet: `MAKE-<TYPE>` (construct),
  `<TYPE>-SHA` (accessor for its root SHA), `<TYPE>-FROM-SHA` (reload from a SHA).
- **Naming**: `/` separates a type name from an operation on it (e.g.
  `CID-SET/UNION`, `VERSIONED-VALUE/VIEW`, `REPOSITORY/ALLOCATE-CID`,
  `MAPPER/RESOLVE`). A leading `%` marks an internal/low-level helper not meant for use
  outside its defining file (e.g. `%stored-object`, `%loaded-object` in `githack.lisp`).
  Predicates use a trailing `?` (`CID-SET?`, `CID-SET/EMPTY?`) as well as the more standard
  `-P` suffix in places — check the specific type before assuming which is used.
- **Package**: everything lives in the single `"GITHACK"` package (`package.lisp`), which
  shadows symbols from `SERIES` (`DEFUN`, `FUNCALL`, `LET*`, `MULTIPLE-VALUE-BIND`),
  `NAMED-LET` (`LET`, `NAMED-LAMBDA`), and `FUNCTION` (`COMPOSE`, `INVERSE`). Every new public
  symbol must be added to the `:export` list in `package.lisp`.
- **CFFI safety**: raw C pointers/oids from `libgit2` calls must be freed under
  `unwind-protect` to avoid leaking native memory.
- **SBCL-specific**: the codebase intentionally relies on `sb-mop` and `sb-ext` rather than
  portable CL abstractions (e.g. `VERSIONED-STANDARD-CLASS` subclasses
  `sb-mop:standard-class` directly). Don't introduce portability shims for other
  implementations.
