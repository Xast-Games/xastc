# Memory management in Xast

**Status:** design draft. Nothing here is implemented yet.
**See also:** [uniqueness.md](uniqueness.md), which defines ownership, moves and clones.

Each item is marked:

- **[decided]**: agreed in the design discussion.
- **[proposed]**: follows from the decided parts, not yet confirmed.
- **[open]**: needs a decision before it can be built.

## 1. What is decided

- **No garbage collector.** [decided]
- **Static ownership.** Every heap value has exactly one owner at any point, and the compiler knows it from liveness. There are no run-time counters in values. [decided]
- **Lambdas that capture values must not escape** (be returned or stored). [decided]
- **Memory for indirection comes from `Std.Alloc`.** Whatever creates a pointer (a recursive type, mutually recursive types) allocates and frees through `Std.Alloc` (`alloc`, `free`, `ptrRead`, ...), never through an allocator private to the backend. [decided]
- **The compiler inserts the pointer for recursive types**; nothing is added to the source (section 8). [decided]

## 2. The idea: one owner

Because two live names never share storage (`uniqueness.md`, 2.2), every heap value has one owner: a name, or a part of a value another name owns. A "count" would always be 0 or 1, so none is kept. The earlier idea of static reference counting collapses into static ownership.

The consequences are the ones Rust gets from ownership: freeing at a known point, in-place mutation, and no work at run time for tracking who holds what. Unlike Rust, there are no lifetimes and no borrow checker, because nothing can store or return a reference.

## 3. What ownership is used for

| Use | What it does | Status |
|---|---|---|
| **Freeing** | Free the storage at the last use of its owner | proposed |
| **Memory reuse** | A cell that dies is reused for a new value of the same layout | proposed, later |
| **In-place mutation** | A `*` parameter owns its argument and mutates it | decided, in `uniqueness.md` |
| **Stack allocation** | A value that never escapes does not need the heap | open, later |
| **Parallel safety** | No two names share storage, so two systems cannot write the same value; values that are only read can be read concurrently | proposed |
| **No cycles** | Values are trees of owned parts, so they cannot form cycles. `RawPtr` can break this, and it is outside the model | proposed |

## 4. Freeing [proposed]

At the last use of a name, its storage is freed there unless ownership is handed on (a move, a `*` argument, a return).

```haskell
let v = newVec,
let n = len v,         -- last use of v: it is freed right after this call
in n
```

- Branches may free in different places: each arm frees what dies in it.
- A record whose fields were partly moved out frees the rest of its fields when the record dies.
- A **non-copyable resource** is not freed implicitly. It must be consumed (`uniqueness.md` 2.5), and the consuming function (`close`) is its destructor.
- An **opaque type that owns memory** (`Vec`, `String`) needs a declared way to free it. [open]
- The free itself is an `Std.Alloc` call. The `free` intrinsic is not defined in the compiler yet.

## 5. Reuse [proposed, later]

When a `match` takes apart a value it owns and that dies there, and builds a new value of the same layout, the old cell becomes the new one with no allocation. Ownership makes this always valid, with nothing to check at run time:

```haskell
fn mapOpt (fn(a) -> b, *Option a) -> Option b;
fn mapOpt f opt = match opt with
    Some x -> Some (f x),     -- the old `Some` cell is reused when a and b have the same layout
    _      -> None;
```

The same applies to `map` and `filter` over a container, record updates and string concatenation.

## 6. Sharing without copying [open]

With one owner per value, giving the same large immutable structure to many holders costs a copy each. Rust has `Rc` and `Arc` for this; Xast has nothing yet. The principled answer is an **opt-in** library type, `Shared a`, built on `Std.Alloc`:

- the counter exists only in values of that type, so everything else stays free of counting;
- it is immutable inside; making a mutable copy is explicit and copies unless the count is 1;
- persistent data structures (which share nodes) would be built on it.

The questions are whether it needs an atomic counter for use across threads, and how it fits the ownership rules (a `Shared a` is cloned by incrementing). Decide together with persistent data structures. Nothing else in this document depends on it.

## 7. Where the cost shows up

| Situation | Cost |
|---|---|
| a name used twice, copyable type | one clone, reported by `--report-clones` |
| a name used twice, non-copyable type | a compile error |
| a read-only alias `let b = a` while `a` is used later | one clone that a later optimization may remove (`uniqueness.md`, P17) |
| several holders of one large value | a copy each, until the opt-in shared type exists |

## 8. Recursive types [decided]

`type List a = Cons a (List a) | Nil` has no pointer in its signature, and a type that contains itself by value has infinite size. **The compiler inserts the pointer**, and allocates and frees the nodes through `Std.Alloc` (section 1). The source is unchanged.

**Which fields.** A field is indirect exactly when its type is in the same strongly connected component of the type graph as the type that declares it. The rule is local and does not depend on the order of fields or declarations. It is applied to concrete types, after monomorphization, so a cycle that goes through a type parameter is found too (`type Tree a = Node a (List (Tree a))`). For mutual recursion it may box more than the minimum, both `A.b` and `B.a`, which is the price of a rule that cannot change when code is reordered.

**Ownership.** A boxed child is owned by its parent: copying the parent copies the nodes (`@Copyable` needs the boxed type to be copyable), and freeing the parent frees them, with the compiler generating that code.

**Visibility and modes.** By default the allocation is implicit, so it must be reportable, as clones are: a report of which fields are boxed and therefore which constructors allocate. [open: a separate flag, or part of `--report-clones`] A package can instead require the annotation: with `explicit_boxing = true` in its `xast.toml`, every field the rule boxes must be marked `boxed`, and a missing mark is an error; in the default mode `boxed` is allowed but redundant. Both modes produce the same layout. See `uniqueness.md`, 2.6.

A type with no constructor that avoids the recursion (`type Bad = Bad Bad`) has no finite value. A warning for it is proposed.

Not chosen: an explicit `Box` type (construction and patterns would have to wrap and unwrap). The marker on the field exists, but only as the optional `boxed` keyword of the explicit mode.

## 9. Open questions

1. **The opt-in shared type** (section 6), together with persistent data structures.
2. **Destructors for opaque types** that own memory: a way to declare one.
3. **Reporting the implicit allocation of recursive types**: the flag (section 8).
4. **Free at the last use, or at the end of the scope?** For plain memory the difference cannot be observed; for resources it can. The proposal is the last use.
5. **Arena or region allocation**, and stack allocation of values that never escape.
6. **The backend's code for freeing**, and the `Std.Alloc` intrinsics it needs. `alloc` and `free` are declared but have no definition in the compiler, and `RawPtr` and the meaning of its offsets are not defined yet.
7. **`RawPtr`**: it is unsafe, outside this model, and the one way to create cycles.

## 10. Plan

1. The ownership analysis in `uniqueness.md` (its step 3).
2. Insert frees after the analysis, once the `Std.Alloc` intrinsics exist.
3. Box recursive fields by the rule in section 8, in the layout of type definitions.
4. Reuse of dead cells.
5. The opt-in shared type, with persistent data structures.
