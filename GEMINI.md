# GitHack Project Instructions & Reference Context

## Project Overview
`GitHack` is a Common Lisp object-relational/persistent database system built directly on top of Git. It uses Git's low-level content-addressable storage (via `libgit2` bindings) as an immutable object database to persist complex Lisp objects, persistent data structures, and standard object slot states with full transactional integrity, change tracking, and historical/branching views.

### Main Technologies
- **Language**: Common Lisp (specifically targeting Steel Bank Common Lisp, **SBCL**).
- **Git Engine**: Bindings to `libgit2` via **CFFI**, alongside some usage of the `git` command-line executable via `uiop:run-program` in tests.
- **Key Dependencies**: `alexandria`, `cffi`, `fold`, `named-let`, `table`, and `sb-mop` (SBCL Metaobject Protocol).

---

## Architectural Breakdown & Core Components

1. **Git Integration (`githack.lisp`)**
   - Directly interacts with `libgit2` via CFFI to open repositories, read and write blobs, and create and read tree objects.
   - Provides low-level serialization/deserialization to simple octet vectors via UTF-8 string conversion (`sb-ext:string-to-octets` / `sb-ext:octets-to-string`).

2. **Persistent Data Structures**
   - **Persistent Cons Cells (`githack.lisp`)**: Stored as Git trees with `car` (blob) and `cdr` (tree) entries. Supports full round-trips from proper Lisp lists to persistent Git-backed lists.
   - **Persistent Weight-Balanced Trees (`persistent-wttree.lisp`)**: Wraps immutable child trees (`left`, `right`) with weights and key-value contents. Implements the `table` protocol.
   - **Persistent Vectors (`persistent-vector.lisp`)**: Implemented via persistent weight-balanced trees containing indices, combined with a separate length tree.
   - **Persistent Hash Tables (`persistent-hash-table.lisp`)**: Uses a persistent vector for bucket arrays and persistent cons chains for collision handling.

3. **Change Tracking & Distributed Identifiers**
   - **Change Identifiers / CIDs (`cid-object.lisp`, `cid-set.lisp`, `cid-master-table.lisp`, `cid-detail-table.lisp`)**: Associates changes with unique integer change IDs (CIDs) and maintains repository-scoped persistent CID sets with immutable operations (union, intersection, remove, etc.).
   - **Distributed Identifiers & Mappers (`identifier.lisp`, `mapper.lisp`, `integer-mapper.lisp`, `distributed-object.lisp`)**: Implements hierarchical namespace and entity identity resolution without cycle-inducing Git links.

4. **Versioned Values & MOP Integration (`versioned-value.lisp`, `versioned-object.lisp`, `cvi.lisp`, `cvfile.lisp`)**
   - Defines a custom metaclass `VERSIONED-STANDARD-CLASS` (subclass of `sb-mop:standard-class`) and custom direct/effective slot definition classes.
   - Allows slots to be declared with a `:version-technique` (options: `:nonversioned`, `:scalar`, `:logged`, `:nonlogged`, `:composite-set`, `:composite-sequence`, `:composite-file`).
   - Slot reads and writes automatically resolve against the current active transaction's CID view.

5. **Transaction System (`txn.lisp`)**
   - Manages nonversioned, versioned, comparison, and update transactions using the `call-with-repository-transaction` protocol.
   - Updates allocate a new CID, execute code within that context, track changed slot details, and commit atomically or abort upon errors/throws.

---

## Prerequisites, Building, and Running

### Prerequisites
1. **Steel Bank Common Lisp (SBCL)**: The codebase is highly optimized for and depends on SBCL-specific extensions (`sb-ext` and `sb-mop`).
2. **libgit2**: A shared library or DLL of `libgit2` (e.g., `git2.dll` on Windows) must be installed.
   - *Windows Note*: The CFFI loader looks for `"git2.dll"` or `"d:\\lib\\git2.dll"`. Ensure this DLL is in your system `PATH` or located at `d:\lib\git2.dll`.
3. **Quicklisp**: Used for managing dependencies (`alexandria`, `cffi`, etc.).

### System Loading
Load the ASDF system by running inside SBCL:
```lisp
(asdf:load-asd (truename "githack.asd"))
(ql:quickload :githack)
```

### Running Tests
All tests are implemented in `tests.lisp`.
To load and execute the test suite:
1. Start SBCL and load the test file:
   ```lisp
   (load "tests.lisp")
   ```
2. Run the tests function:
   ```lisp
   (run-githack-tests)
   ```
The test suite automatically handles the creation of a temporary bare repository, executes 22+ tests (from transactions to complex CVI/CVFILE and MOP integrations), and cleans up.

---

## Development & Contribution Conventions

- **SBCL Specifics**: Do not introduce non-SBCL constructs or abstract away MOP interactions unless carefully wrapping them under appropriate feature-flags. Stick strictly to standard `sb-mop` and `sb-ext` patterns already used.
- **Immutability and Pure Updates**: Nearly all repository operations are functionally pure, producing replacement roots/SHAs rather than in-place mutations. Ensure any new data structures follow this persistent paradigm.
- **Low-Level CFFI Calls**: Ensure raw C pointers are freed safely under `unwind-protect` forms to prevent memory leaks in the underlying C-based `libgit2` engine.
- **Format and Style**: Keep the typical Emacs/Slime indentation, Lisp header comments, and declare precise dependency lists within `githack.asd`.
