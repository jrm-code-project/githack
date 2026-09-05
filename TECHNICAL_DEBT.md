# GitHack Technical Debt

This document tracks known technical debt in GitHack, prioritized so
contributors can decide what to tackle next. Items are grouped by priority
(P0 = most urgent/highest impact, P3 = low-impact polish), and each includes
concrete file:line citations gathered from the current `main` branch.

Severity legend: **High** (correctness/data-safety/major confusion risk),
**Medium** (real cost, not urgent), **Low** (cosmetic/minor).

---

## P0 — Correctness and data-safety risks

### 1. No optimistic-concurrency check on branch updates (Resolved)

**Resolved:** `call-with-git-transaction` (and `call-with-transaction`) now
accept a `:conflict-resolution` keyword argument (`:error`, the default;
`:retry`; or `:lock`). `git-update-ref`/`update-branch` perform a real
compare-and-swap via `git update-ref refs/heads/<name> <new> <old>`,
signaling `concurrent-modification-error` if some other writer already
advanced the branch. `:error` propagates that condition immediately;
`:retry` catches it and re-attempts the whole transaction (re-resolving the
branch and re-invoking the receiver) until it succeeds — which required
auditing the entire proxy/serialization pipeline for purity, since a
retried receiver may run more than once (see `git-transaction.lisp`'s
`call-with-git-transaction` docstring for the audit's conclusion and the
purity requirement this places on `:retry` callers); `:lock` instead
acquires an exclusive, repository-wide lock file
(`transaction-lock.lisp`) before even reading the branch's head, so
concurrent `:lock`-mode transactions serialize instead of racing.

### 2. Documentation describes an architecture that no longer exists (Resolved)

**Resolved:** `README.md` and `GEMINI.md` are rewritten to describe the
current `git-object` proxy architecture (`git-blob`/`git-tree`/
`git-commit`/`git-branch`, the persistent-data-structure layer, the
`persistent-standard-class` MOP integration, and the
`git-repository`/`git-transaction`/`transaction` layer with its
`:conflict-resolution` modes), and both now correctly note that GitHack has
no `libgit2`/CFFI bindings at all — every Git operation shells out to the
`git` executable via `uiop:run-program`.

---

## P1 — Architectural/robustness gaps

### 3. Every Git operation shells out to a fresh `git` process (Resolved)

**Resolved:** `git-io.lisp` now maintains a cache of long-lived `git`
subprocesses (`*git-io-sessions*`) instead of spawning a brand-new process
per call. `git-type` talks to a persistent `git cat-file --batch-check`
session (one round-trip per SHA: write the SHA, read back its `<type>
<size>` header); `git-cat-file` talks to its own persistent `git cat-file
--batch` session, which returns the header *and* the object's content body
in the same round-trip, eliminating the old "two subprocesses per read"
pattern entirely (`git-cat-file` no longer calls `git-type` first);
`git-hash-object` talks to a persistent, per-TYPE `git hash-object -w -t
<type> --stdin-paths` session (Git only supports one `-t` per invocation,
so a session is cached per (repository, type) pair), still staging content
through a temporary file (as before) but feeding that file's path to the
long-lived process instead of spawning a fresh `git hash-object` each time.
Every persistent-session call path transparently falls back to the
original one-shot `uiop:run-program` implementation (`%git-type-one-shot`,
`%git-cat-file-one-shot`, `%git-hash-object-one-shot`) if its session
cannot be started, or dies/misbehaves mid-conversation (broken pipe,
unexpected EOF); the fallback discards the broken session first so the
next call gets a fresh one. `close-git-io-sessions` terminates and forgets
some (or, if called with no argument, every) repository's cached sessions;
`with-temporary-git-repository` (the test suite's real-repository fixture)
now calls it before deleting a temporary repository's directory, so no
session's pipes outlive the directory they were opened against, and an
`sb-ext:*exit-hooks*` entry defensively closes any sessions still cached
when the Lisp image exits. New tests in `git-io-tests.lisp` cover session
reuse across repeated calls, `close-git-io-sessions`'s per-repository
scoping, and the one-shot fallback after a session has died.

### 4. No custom condition hierarchy (Resolved)

**Resolved:** `conditions.lisp` now defines a `githack-error` base condition
(itself a `simple-error` subclass, so most subtypes reuse `simple-error`'s
`:format-control`/`:format-arguments`-based `:report`) with five leaf
subtypes: `malformed-git-object-error` (corrupt/unexpected Git object bytes
or text), `unpersisted-object-error` (an operation needs a SHA or other
required state that has not been set yet), `transaction-state-error` (a
`git-transaction`/`git-repository` operation attempted in the wrong
status/mode), `invalid-argument-error` (a public entry point called with a
malformed argument), and `branch-not-found-error` (`resolve-branch` found no
such branch, with `repository`/`name` slots and a custom `:report`). The
previously-existing `concurrent-modification-error` (`git-branch.lisp`) and
`transaction-lock-timeout-error` (`transaction-lock.lisp`) were retrofitted
to inherit from `githack-error` as well, so every condition GitHack itself
signals is now `typep githack-error`. All ~50 real (non-test) `cl:error`
call sites across `git-object.lisp`, `git-tree.lisp`, `git-commit.lisp`,
`git-blob.lisp`, `git-branch.lisp`, `git-transaction.lisp`,
`persistent-cons.lisp`, `persistent-vector.lisp`, `persistent-array.lisp`,
`persistent-standard-class.lisp`, and `persistent-hash-table.lisp` were
migrated to signal the appropriate subtype instead of a bare `cl:error`.
Test-only call sites simulating arbitrary failures (in `test-helpers.lisp`
and various `*-tests.lisp` files) were deliberately left as plain `cl:error`,
since they are not part of GitHack's own public API surface.

### 5. No test file for `git-io.lisp` (Resolved)

**Resolved:** `git-io-tests.lisp` now exercises `git-hash-object`/`git-type`/
`git-cat-file` against a real, temporary, bare Git repository (via
`with-temporary-git-repository`, genuinely shelling out to `git`, not the
`with-fake-git-*`/`with-recording-git-*` fixtures used elsewhere). It covers
a blob round-trip, `git-type` correctly distinguishing blob/tree/commit,
binary/non-UTF-8-safe content round-tripping through `git-cat-file`,
subprocess non-zero exit-status propagation as a Lisp error for both
`git-type` and `git-cat-file` on a nonexistent SHA and for `git-hash-object`
given an invalid object type, and that both `git-hash-object` and
`git-cat-file` clean up their own temporary files (via `unwind-protect`)
even when the underlying `git` subprocess itself fails.

### 6. Minimal input validation on public entry points (Resolved)

**Resolved:** `phash-make` now signals `invalid-argument-error` if `size`
is not a positive integer, and `%normalize-hash-test`'s symbol method now
signals `invalid-argument-error` if `test` does not name a callable
function (both previously deferred failure to a much later, harder-to-
diagnose point -- a division/mod error inside `%phash-hash`, or an
undefined-function error from `fdefinition`, respectively).
`call-with-repository` now signals `invalid-argument-error` for a `nil`
`repository-specifier`, a `mode` other than `:read-only`/`:read-write`, or
a `receiver` that is not a callable function. `call-with-git-transaction`
now signals `invalid-argument-error` for a `repository` that is not a
`git-repository`, an invalid `mode`, a non-callable `receiver`, or an
effective `branch` name (after cascading from the repository's own
default) that is not a non-empty string, in addition to its pre-existing
`transaction-state-error` for a `:read-write` transaction against a
`:read-only` repository. All four validations are covered by new tests in
`git-repository-tests.lisp`, `git-transaction-tests.lisp`, and
`persistent-hash-table-tests.lisp`.

---

## P2 — Consistency and maintainability

### 7. Generated `define-persistent-struct` APIs are undocumented (Resolved)

**Resolved:** `define-persistent-struct` (`persistent-struct.lisp`) now
templates a docstring for every generated form: the `defclass` itself,
the `make-<name>` constructor, the `<name>-p` predicate, and every
generated slot accessor (the latter three via `(setf (documentation ...))`,
since `defclass`'s per-slot `:documentation` option does not propagate to
the implicitly-created accessor's own function documentation). This
directly documents `persistent-hash-table`/`persistent-hash-table-p`/
`-test`/`-count`/`-buckets` and every other `define-persistent-struct`
consumer for free.

### 8. No mechanical check that exported symbols are documented (Resolved)

**Resolved:** `documentation-tests.lisp` adds a FiveAM test
(`every-exported-symbol-has-documentation-for-each-of-its-roles`) that
iterates `do-external-symbols` on the `GITHACK` package, infers each
symbol's role(s) (type/class via `find-class`, function/macro/generic-
function via `fboundp`, special variable via `sb-int:info`), and asserts
`documentation` is non-nil for each applicable role. Running this test
against the pre-existing codebase surfaced ~37 real gaps beyond item #7's
narrower scope — mostly ordinary `get-*` CLOS reader functions (e.g.
`get-repository`, `get-tree`, `get-status`) whose slot's own
`:documentation` option (already present almost everywhere) does not
transfer to the accessor's function documentation. Every gap was fixed via
`(setf (documentation 'name 'function) "...")` forms placed once per
symbol (immediately after the owning `defclass`), including for symbols
legitimately reused, per this codebase's `get-<slot>` naming convention,
across several unrelated classes (e.g. `get-repository` is used by
`git-object`, `git-branch`, and `branch-not-found-error` alike — one
comprehensive docstring covers all three). This test is a permanent
regression guard: any future exported symbol without documentation now
fails the suite.

### 9. Leftover ETYPECASE-era naming on now-generic functions (Low)

`git-transaction.lisp:116-140`, `persistent-vector.lisp:109-132`, and
`persistent-cons.lisp:101-120` still name their generic functions
`%persist-git-object-etypecase`, `%persist-vector-component-etypecase`, and
`%persist-cons-component-etypecase`, respectively — a naming leftover from
before these were converted from `etypecase` forms into `defgeneric`/
`defmethod` dispatch. The names are accurate about *why* the split exists
but no longer describe *what* the function is (a CLOS generic dispatch).

**Suggested fix:** rename to something like `%persist-git-object-by-type`,
`%persist-vector-component-by-type`, `%persist-cons-component-by-type` (or
similar) the next time any of these files are touched for another reason —
not worth a dedicated commit on its own.

---

## P3 — Cosmetic / out of scope for now

### 10. No documented concurrency policy (Medium, mostly a documentation gap)

`*repository*`, `*git-transaction*`, and `*transaction*`
(`package.lisp:13-15`) are ordinary dynamic variables, correctly rebound
per-call via `let` in `git-repository.lisp:8-18,97-103`,
`git-transaction.lisp:32-45,263-268`, and `transaction.lisp:62-84,122-137`
— thread-local dynamic binding itself is not a bug. However:

- Lazily-loaded proxy caches (`persistent-vector.lisp:252-260`,
  `persistent-array.lisp:231-260`, and the shared `atomic-wrapper.lisp`
  lazy-load helpers) mutate slots with no locking; two threads touching the
  same hollow proxy concurrently can race harmlessly (idempotent re-fetch)
  or, in the worst case, observe a partially-populated object.
  Practically low-risk today since nothing in the codebase currently
  spawns worker threads, but undocumented.
- No README/docstring anywhere states whether concurrent use (multiple
  threads, or multiple OS processes sharing one Git repository) is
  supported, and to what extent.

**Suggested fix:** at minimum, document the current single-writer,
single-thread-per-transaction assumption explicitly (e.g. in
`git-transaction.lisp`'s file-level comment), so this isn't discovered the
hard way. A real fix depends on item #1 being addressed first.

### 11. Hardcoded/ambient dependence on `git` being on `PATH` (Low)

Every Git-shelling call site invokes the bare executable name `"git"` with
no configurable path, version check, or startup diagnostic:

- `git-io.lisp:35-40`, `git-io.lisp:49-53`, `git-io.lisp:75-80`
- `git-branch.lisp:42-49`, `git-branch.lisp:58-63`

A misconfigured `PATH`, multiple installed Git versions, or an unusual
shell environment fails at first use with whatever raw error
`uiop:run-program` produces, rather than a clear diagnostic. Low priority
since this is standard practice for tools that shell out to `git`, but
worth a `(uiop:run-program '("git" "--version") ...)` sanity check with a
clear error message if it's ever worth the complexity.

### 12. Untested pathname portability edge cases (Low)

`git-io.lisp:37-40,49-53,75-78` and `git-branch.lisp:44-47,59-63` rely on
`uiop:native-namestring`/`uiop:default-temporary-directory` rather than
hardcoded separators, which is the right call, but there are no tests
covering paths with spaces, non-ASCII characters, or (on Windows) UNC
paths.

---

## Suggested order of remediation

Priority alone (P0-P3) says *how much it matters*; this section says *what
order to actually do the work in*, accounting for which items block or
cheaply set up others. Numbers refer back to the item numbers above.

1. **#2 (rewrite README.md/GEMINI.md)** — do this first, and by itself.
   It's pure documentation, has zero interaction with any other item, costs
   nothing to get wrong, and every other change below reads a lot easier
   once the docs describe the architecture that actually exists.
2. **#1 (optimistic-concurrency check on branch updates)** — the single
   highest-impact correctness fix, and it's self-contained (touches
   `update-branch`/`call-with-git-transaction` only). Do this next, before
   anything that adds more callers of the transaction path (like #6's
   validation work), so those new call sites inherit the safe behavior for
   free instead of needing a follow-up pass.
3. **#4 (custom condition hierarchy)** — do this before #6 and #5, not
   after. Both of those items are about signaling *more* errors in *more*
   places; introducing the condition types first means the validation work
   in #6 and the new tests in #5 are written once, against the final
   condition classes, instead of being written against bare `CL:ERROR` and
   then re-touched later.
4. **#6 (input validation on public entry points)** and **#5 (git-io-tests.lisp)**
   — do these together, in either order; they're independent of each other
   but both consume the condition hierarchy from #4. #5 in particular
   should also directly exercise the retry/conflict condition added in #1.
5. **#3 (batch/cache Git subprocess calls)** *(done)* — deliberately
   sequenced after #1, #4, #5, and #6. It was the largest, riskiest, most
   invasive change in this document (a new long-lived-process protocol
   underneath `git-io.lisp`), and was safer to attempt once the
   transaction layer above it already had conflict detection (#1), a real
   condition hierarchy to report subprocess failures through (#4), and
   direct test coverage of the original one-shot-process behavior (#5) to
   diff against.
6. **#7 (docstrings on generated `define-persistent-struct` APIs)** and
   **#8 (mechanical exported-symbol documentation check)** *(done)* — done
   together as planned: the #8 test was written first, immediately
   surfacing every gap (including #7's, plus ~30 more ordinary `get-*`
   reader functions), then all gaps were fixed until the test passed.
7. **#9 (rename leftover `-etypecase` function names)** and **#10/#11/#12**
   (concurrency-policy docs, `git` PATH sanity check, pathname-portability
   tests) — lowest priority; pick these up opportunistically whenever
   another change already has you touching the same file, rather than as
   dedicated work. **This is the next item to pick up.**

---

## Explicitly *not* debt (verified, no action needed)

- **Old prototype system removal**: `githack.lisp`, `tests.lisp`, all
  `cid-*.lisp`, `mapper.lisp`, `integer-mapper.lisp`,
  `versioned-object.lisp`, `versioned-value.lisp`, and
  `persistent-wttree.lisp` are confirmed absent from the working tree and
  from `githack.asd`; only stale *documentation* (item #2) still refers to
  them.
- **TODO/FIXME/XXX/HACK/KLUDGE markers**: none found in any active `*.lisp`
  or `*.asd` file.
- **Raw `assert`-based testing**: confined to the deleted `tests.lisp`; the
  active FiveAM suite (`githack/test`, driven by `asdf:test-system
  :githack`) is the sole test system and is not duplicated.
- **`-P` predicate naming**: the active codebase consistently uses standard
  Common Lisp `-P`-suffixed predicates (e.g. `persistent-hash-table-p`,
  generated `<name>-p` from `define-persistent-struct`); the `?`-suffixed
  convention only ever applied to the deleted prototype system, and the
  user has separately clarified `?`/`!` are the preferred Scheme-style
  conventions going forward for *new* predicates/side-effecting functions.
