# GitHack

GitHack is a Common Lisp (SBCL) persistent object database built directly on
Git's content-addressable storage. It uses
immutable Git blobs/trees/commits to implement persistent data structures,
a CLOS metaobject-protocol integration, and a transaction layer with
configurable concurrency-conflict handling — so that ordinary CLOS slot
updates become immutable, historied Git commits.

## Building and running tests

There is no separate build step and no lint tooling in this repo —
everything is loaded and exercised interactively in SBCL via ASDF/Quicklisp.

```lisp
(asdf:load-asd (truename "githack.asd"))
(ql:quickload :githack)
```

Requires the `git` executable to be present on `PATH`: GitHack does not
bind `libgit2` directly; every Git operation shells out to the `git` CLI
via `uiop:run-program` (see `git-io.lisp`, `git-branch.lisp`).

### FiveAM test suite (`githack/test`)

New code should get FiveAM tests in the `githack/test` ASDF system (a second
`defsystem` inside `githack.asd`, per ASDF's rule that a `name/subsystem`
must live in `name.asd`). Run this after every change; it must be 100% green
(and everything must compile with zero warnings) before committing:

```lisp
(ql:quickload :fiveam)
(asdf:load-asd (truename "githack.asd"))
(ql:quickload :githack)
(asdf:test-system :githack)   ; delegates to githack/test via :in-order-to
```

This prints a FiveAM report and signals a Lisp error if any test fails (see
`:perform (test-op ...)` in `githack.asd`), so a non-zero/error result from
that form means the suite failed. Tests live in `test-package.lisp`
(package `GITHACK-TEST`, suite `GITHACK-SUITE`), `test-helpers.lisp`
(shared fixtures/mocks), and one `<topic>-tests.lisp` file per area under
test (e.g. `git-tree-tests.lisp`), each adding a sub-suite via `(def-suite
... :in githack-suite)`. To run just one sub-suite or test interactively:
`(fiveam:run! 'git-tree-suite)` or `(fiveam:run! 'git-branch-instantiates)`.

`GITHACK-TEST` only imports the specific `GITHACK` symbols it needs (not
`:use`), since `GITHACK` shadows several `COMMON-LISP` symbols (see
Conventions below) that would conflict with `FIVEAM`'s own `:use` of
`COMMON-LISP`.

## Architecture

The system is a layered stack (see `githack.asd` for the authoritative
`:depends-on` graph); understanding a change usually requires reading the
layer below and above it:

1. **`git-io.lisp`** — low-level shell-outs to the `git` executable
   (`git-hash-object`, `git-type`, `git-cat-file`) plus shared helpers
   (`%unique-temporary-pathname`, plist serialization). Everything above
   this file never shells out to `git` directly.
2. **The `git-object` proxy layer** — `git-object.lisp` defines the base
   `git-object` class (a lazily-loadable proxy keyed by SHA, with
   `inflate-git-proxy` as the factory for turning a bare SHA into the
   right concrete proxy class) and `sha`/loaded-state protocol. Concrete
   proxies:
   - `git-blob.lisp` — a leaf value (any serializable Lisp atom).
   - `git-tree.lisp` — a directory node, an alist of filename → `git-object`
     entries, serialized/deserialized to Git's binary tree format.
   - `git-commit.lisp` — a snapshot: a root `git-tree`, parent commits,
     author/committer/timestamp/message, serialized to Git's plain-text
     commit format.
   - `git-branch.lisp` — a mutable pointer (not a `git-object`, has no SHA
     of its own) onto a `git-commit`, backed by `refs/heads/<name>`;
     `resolve-branch`/`update-branch` read/write it, and `update-branch`
     supports Git's own `update-ref <ref> <new> <old>` compare-and-swap.
3. **Persistent data structures**, each implemented as `git-tree` subtypes,
   each layer building on the last:
   - `persistent-cons.lisp` — a cons cell as a tree (`car` blob, `cdr` tree).
   - `persistent-vector.lisp` — built on persistent cons chains (buckets)
     plus a length entry.
   - `persistent-array.lisp` — a flattened row-major persistent vector plus
     its dimensions.
   - `persistent-hash-table.lisp` — a `persistent-vector` of buckets, each
     bucket a `persistent-cons` alist; built via `define-persistent-struct`
     (see below), not any custom Git serialization of its own.
   - `atomic-wrapper.lisp` — wraps a bare atomic `git-blob` root in a
     three-entry `git-tree` (`.meta`/`README.md`/`value`) since a Git
     commit must always point at a tree; `wrap-atomic-commit-root`/
     `resolve-commit-root` make this transparent to callers.
4. **MOP integration** (`persistent-standard-class.lisp`) —
   `persistent-standard-class` (a `standard-class` subclass) with custom
   direct/effective slot definitions supporting a `:transient` slot option
   (never persisted). Serializing a `persistent-object` writes one Git-tree
   entry per non-transient initarg; `slot-value-using-class` transparently
   resolves any `git-proxy` value found in a slot, caching the result.
   `persistent-struct.lisp`'s `define-persistent-struct` macro gives this
   the syntactic convenience of `defstruct` (generating the `defclass`,
   `make-<name>` constructor, and `<name>-p` predicate).
5. **`git-repository.lisp`** — `git-repository` wraps a repository's
   pathname plus its default branch/author/committer/message/mode;
   `call-with-repository`/`with-repository` bind `*repository*` around a
   receiver.
6. **`git-transaction.lisp`** — `git-transaction` and
   `call-with-git-transaction`/`with-git-transaction`: resolve a branch to
   its head commit, invoke a receiver with a transient transaction and
   that head, and (for `:read-write`) automatically persist the receiver's
   returned root, create a new commit, and advance the branch —
   accepting a `:conflict-resolution` argument (`:error`/`:retry`/`:lock`,
   see below) to resolve concurrent writers racing on the same branch.
7. **`transaction-lock.lisp`** — `with-repository-transaction-lock`, a
   portable OS-level lock-file mechanism (`CL:OPEN` with `:if-exists nil`)
   backing `:conflict-resolution :lock`.
8. **`transaction.lisp`** — `call-with-transaction`/`with-transaction`: the
   user-facing wrapper around `call-with-git-transaction` that hides SHAs/
   `git-blob`/`git-tree`/`git-commit` entirely behind a single plain Lisp
   value per transaction.

Load order is dependency-driven and declared explicitly per-file in
`githack.asd`'s `:depends-on` lists — when adding a new file, add it there
too.

### Concurrency and conflict resolution

`call-with-git-transaction`/`call-with-transaction` accept a
`:conflict-resolution` keyword governing what happens when some other
writer has already advanced a branch between this transaction's own read
of its head commit and its own commit (Git's "Lost Update" problem),
detected via `git update-ref`'s own compare-and-swap check:

- `:error` (the default) lets `concurrent-modification-error` propagate
  immediately; nothing is written.
- `:retry` catches that condition and re-attempts the entire transaction
  from scratch (re-resolving the branch and re-invoking the receiver)
  until it succeeds. Because the receiver may thus run more than once, it
  must be free of any side effect other than reading and returning
  `git-object`s/persistent proxies — no network calls, no file I/O outside
  of Git itself, no mutation of shared Lisp state.
- `:lock` acquires an exclusive, repository-wide OS-level lock (see
  `transaction-lock.lisp`) before even reading the branch's head commit,
  serializing concurrent `:lock`-mode transactions against the same
  repository instead of racing.
- `:rebase` also detects the race via Git's own compare-and-swap check, but
  does not discard the receiver's already-computed work: it replays the
  transaction's own candidate commit onto the branch's new head via `git
  merge-tree` (a real, working-tree-free three-way content merge), retrying
  the replay against an ever-fresher head for as long as other writers keep
  winning the race, and only falls back to `:retry` (re-running the receiver
  from scratch) or `:error` (signaling `merge-conflict-error`) — governed by
  a separate `:rebase-fallback` keyword — if `git merge-tree` ever reports a
  genuine, unresolvable content conflict (both writers touched the exact
  same content) rather than a merely-detected-but-mergeable race.

See `TECHNICAL_DEBT.md` for a prioritized list of known gaps and their
suggested remediation order.

## Advanced features

### Nested transactions

`with-transaction`/`with-git-transaction` nest automatically: opening a new
transaction against the *same* repository from inside an already-running
transaction's own body (rather than from top-level code) is detected via
the dynamically-bound `*transaction*`/`*git-transaction*`, and produces a
nested transaction instead of a second, independent one. A nested
transaction never creates its own `git-commit` or advances the branch
itself — on a normal return, its own computed root simply *percolates
upward*, becoming its immediately enclosing transaction's own current root;
only the outermost transaction's own eventual normal return actually
creates a commit and advances the branch. Nesting can go arbitrarily deep,
and each level's contribution percolates all the way up to the outermost
transaction:

```lisp
(with-transaction (balance) (bank :read-write)
  ;; Nested transaction: computes a new balance, then percolates it
  ;; upward into the outer transaction's own current root -- no commit
  ;; of its own is ever created.
  (with-transaction (b) (bank :read-write)
    (+ b 100))
  ;; The outer transaction's own return value is what actually gets
  ;; committed once it returns.
  (get-payload (get-current-root *transaction*)))
```

Calling `abort-git-transaction` from inside a nested transaction discards
only that nested transaction's own contribution — its immediately
enclosing transaction's current root is left completely untouched — but a
*genuine error* signaled from inside a nested transaction instead unwinds
every enclosing transaction in turn, all the way out, exactly as it would
for a single, non-nested transaction: nothing at any level is written. See
`end-to-end-nested-transactions-commit-abort-and-retry`
(`end-to-end-tests.lisp`) for a comprehensive, real (non-mocked) walk
through multi-level nesting, explicit abort, and `:retry` together.

### Line-oriented string serialization

When a Lisp string is serialized into an atom's blob (`git-blob.lisp`), any
embedded `#\Newline` characters are written to the blob as literal line
feeds — via plain `prin1-to-string`, which already prints a Lisp string
argument with its own newlines intact rather than escaping them — never as
a single-line, escaped token. This is deliberate: it lets Git's own
line-oriented diff/merge machinery (in particular the three-way content
merge `:rebase`'s own conflict resolution relies on, via `git merge-tree`)
treat two concurrent edits to *different lines* of the same long string as
a clean, automatic merge, rather than an unavoidable conflict merely
because both edits happened to touch "the same token" on a single physical
line. See `end-to-end-rebase-mode-auto-merges-concurrent-edits-to-
different-lines-of-the-same-string` (`end-to-end-tests.lisp`).

### Distributed (multi-repository) transactions: Two-Phase Commit

`with-githack-transaction`/`call-with-githack-transaction`
(`distributed-transaction.lisp`) let a single logical transaction span any
number of distinct repositories (or distinct orphan branches within one
repository), committing all of them together, atomically, or none at all —
built entirely on standard Git plumbing (`hash-object`, `cat-file`,
`update-ref`, `update-ref --stdin`, `mktag`, `rev-parse`, `for-each-ref`),
with no external coordination service of any kind:

```lisp
(with-githack-transaction ()
  (with-repository (checking) (checking-repo-path :branch "main" :mode :read-write)
    (with-transaction (v) (checking :read-write) (- v 100)))
  (with-repository (savings) (savings-repo-path :branch "main" :mode :read-write)
    (with-transaction (v) (savings :read-write) (+ v 100))))
```

Ordinary `with-transaction`/`with-git-transaction` calls made inside a
`with-githack-transaction` body work completely unmodified — including
their own nested-transaction support, described above — except that their
final branch update is transparently *deferred* rather than applied
immediately, becoming a pending write against the enclosing distributed
transaction. Once the body returns normally, that transaction's pending
writes decide what happens next:

- **Zero participants** (the body opened no ordinary transaction against
  any repository at all): a no-op.
- **One participant** (the "Fast Path"): its branch is advanced directly,
  via a single atomic `git update-ref --stdin` call — no coordination
  overhead at all.
- **More than one participant**: a full Two-Phase-Commit protocol runs:
  1. **Prepare**: the first participant encountered is elected the
     *Ledger*. For every participant, an annotated tag (`git mktag`) is
     created targeting its own already-persisted-but-not-yet-committed
     commit, carrying a Transaction Manifest (the transaction's ID, the
     Ledger's own location, and every participant's own branch/original
     head) as its message, and a tracking ref —
     `refs/githack/prepare/<tx-id>/<branch-name>` — is pointed at it. If
     any participant's own Prepare step fails, every prepare ref already
     created for this transaction is rolled back and
     `distributed-transaction-error` is signaled; nothing is committed
     anywhere.
  2. **Point of No Return**: once every participant has been prepared, a
     single `refs/githack/ledger/<tx-id>` ref is written in the Ledger
     repository. The instant that ref exists, the transaction is
     permanently committed, no matter what happens next — even a process
     crash.
  3. **Roll Forward**: every participant's real branch is advanced to its
     own prepared commit and its own prepare ref is deleted, both via one
     single atomic `git update-ref --stdin` batch per participant.

If the body signals an error instead of returning normally, nothing is
committed anywhere: every participant's own already-persisted commit is
simply left as harmless, unreferenced Git garbage for `git gc` to
eventually collect — exactly the same free rollback guarantee an ordinary,
single-repository transaction already provides, now extended transparently
across every participant at once.

**Crash recovery.** If the Lisp process dies between Prepare and Roll
Forward, some participants are left with a stranded
`refs/githack/prepare/<tx-id>/<branch-name>` ref. `run-githack-exorcist`,
run against any single repository (e.g. on process boot, or lazily on
first access), finds every such stranded ref, reads its own annotated tag's
Manifest to find the Ledger, and asks it directly whether
`refs/githack/ledger/<tx-id>` exists: if so, the transaction had already
passed its Point of No Return, so the stranded participant is rolled
forward exactly as step 3 above would; if not, the crash happened during
Prepare, so the stranded ref is simply deleted, leaving the branch
untouched.

**Scope limitation:** `:rebase` conflict-resolution is not supported for a
transaction participating in a `with-githack-transaction` (its own
auto-merge/retry semantics don't compose with cross-repository 2PC);
participants must use `:error`, `:retry`, or `:lock`.

See `distributed-transaction-tests.lisp` (no-op/Fast-Path/2PC dispatch,
Phase 1 failure rollback, and both Exorcist recovery outcomes) and
`distributed-nested-transaction-tests.lisp` (distributed transactions
composed with ordinary nested-transaction support, including two
`with-githack-transaction`s nested inside one another) for comprehensive,
real (non-mocked) end-to-end coverage. `examples/bank.lisp` and
`examples/concurrency-demo.lisp` demonstrate nested transactions and every
`:conflict-resolution` strategy interactively.


## Conventions

- **Everything is persistent/immutable.** Operations that "update" a
  structure return a new SHA/root and leave prior roots valid and
  independently loadable — never mutate in place. Every persistent type
  follows the same round-trip: a serialization function (writing the
  in-memory state to Git and setting the object's `sha`), and a factory
  (`inflate-git-proxy`) that lazily reconstructs an object from a SHA.
- **Naming**: `GET-<slot-name>` is used for CLOS `:reader`/`:accessor` slot
  names (never a `<class-name>-<slot-name>` `defstruct`-style prefix,
  which would break polymorphism across classes sharing a slot name). A
  leading `%` marks an internal/low-level helper not meant for use outside
  its defining file (e.g. `%persist-git-object`, `%unique-temporary-
  pathname`). Predicates use a trailing `-P`/`-p` suffix (the standard
  Common Lisp convention) for the current, active codebase; the
  Scheme-style trailing `?`/`!` conventions apply going forward to *new*
  predicates/side-effecting functions per project preference, without
  requiring existing `-P` names to be renamed.
- **Package**: everything lives in the single `"GITHACK"` package
  (`package.lisp`), which shadows symbols from `SERIES` (`DEFUN`,
  `FUNCALL`, `LET*`, `MULTIPLE-VALUE-BIND`), `NAMED-LET` (`LET`,
  `NAMED-LAMBDA`), and `FUNCTION` (`COMPOSE`, `INVERSE`). Every new public
  symbol must be added to the `:export` list in `package.lisp`.
- **CFFI safety**: N/A for the current architecture — GitHack has no CFFI
  bindings; every Git operation shells out to the `git` executable via
  `uiop:run-program` instead. Temporary files created to shuttle data to/
  from those subprocesses (see `git-io.lisp`'s `%unique-temporary-
  pathname`) must be cleaned up under `unwind-protect`/`ignore-errors` even
  if the subprocess call fails.
- **SBCL-specific**: the codebase intentionally relies on `sb-mop` and
  `sb-ext` rather than portable CL abstractions (e.g.
  `persistent-standard-class` subclasses `sb-mop:standard-class` directly).
  Don't introduce portability shims for other implementations.
