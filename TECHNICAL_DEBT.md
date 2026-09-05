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

### 2. Documentation describes an architecture that no longer exists (High)

`README.md` and `GEMINI.md` extensively document the old CFFI/libgit2-based
CID/mapper/versioned-object prototype system (persistent WTTrees,
`CID-OBJECT`, `CID-SET`, `CID-DETAIL-TABLE`, `CID-MASTER-TABLE`,
distributed identifiers, mappers, `CVI`/`CVFILE`) — all of which was
deliberately deleted from the codebase (see `git log` for the deletion of
`githack.lisp`, `tests.lisp`, `cid-*.lisp`, `mapper.lisp`,
`integer-mapper.lisp`, `versioned-object.lisp`, `versioned-value.lisp`,
`persistent-wttree.lisp`, etc.) in favor of the current `git-object` proxy
architecture (`git-blob`/`git-tree`/`git-commit`/`git-branch`,
`call-with-repository`/`call-with-git-transaction`).

- `README.md:47-113` (CID/mapper/versioned-value sections, none of which
  exist in the current tree)
- `GEMINI.md` (describes `githack.lisp`, `git2.dll`/`d:\lib\git2.dll` CFFI
  loading, and the old `tests.lisp`/`run-githack-tests` raw-assert suite,
  none of which are accurate for the current architecture)

This actively misleads new contributors (and any AI coding assistant reading
these files) about what files exist, what to load, and how to run tests.
This should be rewritten (or clearly marked "historical/superseded") as a
priority, since it is pure onboarding risk with no upside.

**Suggested fix:** rewrite `README.md` to describe the current `git-object`
proxy architecture (mirroring the accurate description already present in
this repo's Copilot custom instructions), and either delete `GEMINI.md` or
mark it explicitly as a historical snapshot that predates the rewrite.

---

## P1 — Architectural/robustness gaps

### 3. Every Git operation shells out to a fresh `git` process (High, perf)

`git-io.lisp` performs each object read/write by launching a brand-new
`git` subprocess via `uiop:run-program`, with no batching, caching, or
persistent session:

- `git-io.lisp:26-45` (`git-hash-object` writes a temp file, then spawns
  `git hash-object -w --path=... <tmpfile>`)
- `git-io.lisp:47-55` (`git-type` spawns `git cat-file -t <sha>`)
- `git-io.lisp:71-84` (`git-cat-file` spawns `git-type` *and then* a second
  `git cat-file <type> <sha>` process — two subprocesses per read)

There is no `git cat-file --batch`/`--batch-check` long-lived pipe, no SHA
existence/type cache, and no reuse of a repository-level command session.
Persisting a single moderately-sized persistent structure (e.g. a
multi-hundred-entry `persistent-vector` or a deep `persistent-cons` chain)
can spawn dozens to hundreds of `git` processes, each paying full process
start-up and temp-file I/O cost. This is almost certainly the dominant
runtime cost of the system today and will not scale well.

**Suggested fix:** introduce a long-lived `git cat-file --batch` (reads) and
`git hash-object --stdin-paths -w` or `git fast-import`-style batching
(writes) session per repository/transaction, falling back to today's
one-shot subprocess calls only when unavailable.

### 4. No custom condition hierarchy (Medium)

No `define-condition` forms exist anywhere in the codebase. Every error
path signals a plain `cl:error` with a format string, e.g.:

- `git-object.lisp:35-38`, `git-object.lisp:137`
- `git-tree.lisp:23`, `git-tree.lisp:29`
- `git-branch.lisp:79`
- `git-transaction.lisp:206-208`
- `persistent-vector.lisp:250`
- `persistent-array.lisp:116-122`

Callers cannot programmatically distinguish "malformed Git object data" from
"invalid API argument" from "missing branch" from "transaction already
committed" except by pattern-matching the error message string. This makes
building any retry/recovery logic (see item #1 above) or friendlier
end-user error handling brittle.

**Suggested fix:** introduce a small condition hierarchy rooted at a
`githack-error` (or `git-object-error`) base condition, with a handful of
specific subtypes (e.g. `malformed-git-object`, `unpersisted-object-error`,
`transaction-state-error`, `branch-not-found`), and migrate the call sites
above incrementally.

### 5. No test file for `git-io.lisp` (Medium)

`githack.asd` registers a `git-io` component but there is no
`git-io-tests.lisp` in either the main or `/test` subsystem:

- `githack.asd:7-37` (main system file list — no `git-io-tests`)
- `githack.asd:43-63` (test system file list — no `git-io-tests`)

`git-io.lisp`'s behavior is only exercised indirectly, through fakes
(`test-helpers.lisp:107-216`) and the end-to-end suite
(`end-to-end-tests.lisp:58-246`). Untested directly: subprocess non-zero
exit-status handling, a missing `git` executable, non-UTF-8/malformed
subprocess output, and temp-file cleanup when a subprocess fails partway.

**Suggested fix:** add a `git-io-tests.lisp` exercising `git-hash-object`/
`git-type`/`git-cat-file` against a real temporary repository (as
`end-to-end-tests.lisp` already does elsewhere), including at least one
test per failure mode above.

### 6. Minimal input validation on public entry points (Medium)

Several widely-used constructors accept structurally invalid input without
complaint, deferring failure to a much later, harder-to-diagnose point:

- `persistent-hash-table.lisp:278-288` (`phash-make`'s `SIZE` is not
  validated; a zero or negative size can surface later as a division/mod
  error inside `%phash-hash`, `persistent-hash-table.lisp:68-72`)
- `persistent-hash-table.lisp:43-52` (`%normalize-hash-test` accepts any
  symbol without checking it names a callable two-argument predicate;
  failure surfaces later via a generic undefined-function error from
  `fdefinition`)
- `git-repository.lisp:83-103` (`call-with-repository` does not validate
  its `repository-specifier` pathname, `mode`, or that `receiver` is a
  callable)
- `git-transaction.lisp:271-273` (`call-with-git-transaction` validates only
  the read-only/read-write combination — not branch-name shape, missing
  receiver, etc.)

**Suggested fix:** add `check-type`/explicit validation at these
boundaries, ideally signaling the new condition types from item #4.

---

## P2 — Consistency and maintainability

### 7. Generated `define-persistent-struct` APIs are undocumented (Medium)

`define-persistent-struct` (`persistent-struct.lisp:53-99` roughly) macro-
expands into a `defclass` plus a generated `make-<name>` constructor and
`<name>-p` predicate, but none of the generated forms attach a docstring.
Every exported hand-written function in this codebase has an unusually
thorough docstring; the generated public API is the one conspicuous gap.
`package.lisp:78-88` exports several of these generated symbols (e.g.
`persistent-hash-table-p`, `phash-make`), so this is a real, user-facing
documentation hole, not just an internal one.

**Suggested fix:** have the `define-persistent-struct` expansion emit a
docstring for the generated constructor and predicate (can be templated
from the struct name and slot list).

### 8. No mechanical check that exported symbols are documented (Low)

`package.lisp:6-111` exports a large public API, but nothing in the test
suite asserts `(documentation 'symbol 'function)` (or `'type`/`'variable`)
is non-nil for every exported symbol. This is how the gap in item #7 went
unnoticed. A cheap FiveAM test iterating `do-external-symbols` on the
`GITHACK` package and checking for documentation would catch regressions
like this automatically going forward.

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
5. **#3 (batch/cache Git subprocess calls)** — deliberately sequenced after
   #1, #4, #5, and #6. It's the largest, riskiest, most invasive change in
   this document (a new long-lived-process protocol underneath
   `git-io.lisp`), and it is much safer to attempt once the transaction
   layer above it already has conflict detection (#1), a real condition
   hierarchy to report subprocess failures through (#4), and direct test
   coverage of the current one-shot-process behavior (#5) to diff against.
6. **#7 (docstrings on generated `define-persistent-struct` APIs)** and
   **#8 (mechanical exported-symbol documentation check)** — do these
   together: write the #8 test first (it will immediately fail and enumerate
   every gap, including #7's), then fix #7 until #8 passes. Cheap, low-risk,
   no dependency on anything above.
7. **#9 (rename leftover `-etypecase` function names)** and **#10/#11/#12**
   (concurrency-policy docs, `git` PATH sanity check, pathname-portability
   tests) — lowest priority; pick these up opportunistically whenever
   another change already has you touching the same file, rather than as
   dedicated work.

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
