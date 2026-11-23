# Uniqueness and ownership in Xast

The ownership model of Xast: who owns a value, what `*` in a signature means, and when the compiler moves, clones or rejects.
This file holds the rules. The reasoning behind them is in [uniqueness-questions.md](uniqueness-questions.md),
resources and systems in [uniqueness-ecs.md](uniqueness-ecs.md), the conventions of the standard library in
[uniqueness-std.md](uniqueness-std.md), the compiler's side in [uniqueness-implementation.md](uniqueness-implementation.md),
and the rejected designs in [uniqueness-alternatives.md](uniqueness-alternatives.md).
See also [memory.md](memory.md) for freeing, reuse and sharing.

Marks on a rule: **[implemented]** is checked by the compiler today, **[later]** is planned, **[out of scope]** is left to another document.
A rule without a mark is part of the design that the compiler does not check yet. The state of the project is in [../README.md](../README.md).

## 1. Goals

- Values are immutable by default: a program reads like ordinary functional code.
- Performance close to Rust's: values are moved, not copied; mutation happens in place; no garbage collector, no run-time counters, no lifetimes, no borrow checker.
- A copy happens only where a name is used again, exactly where Rust would need `.clone()`, and every such copy is reported.
- Things that must not be copied (a console, a graphics context, an ECS command buffer) are protected by the same rules, with errors instead of copies.
- Side effects are explicit in signatures.

## In short

- A value has one owner. A bare parameter reads it; a `*` parameter takes it.
- When a name is handed on and is still used afterwards, the compiler clones the value if its type is `@Copyable`, and reports an error otherwise. If the name is not used again, the value is moved.
- `-> *T` in a result means that the very object given in a `*T` parameter is handed back, in place.
- A copy is never made silently for a non-copyable type, and every inserted copy is listed by `--report-costs`.

## 2. The model

### 2.1 Ownership and modes

Types are ordinary. Every value has exactly **one owner** at any point: a name, or a part of the value another name owns. The compiler knows from liveness (is the name used later on this path?) whether a use hands the value over or needs a copy.

`*` is an annotation on a **function parameter**: "this function takes ownership of the argument and may mutate it". A bare parameter is a read.

`*` is **not a type**. Mutation is behavior, and a signature describes behavior; a type describes data. So there is no `Option *T`, no `Vec *File`, no `**T`, and no `*` in record fields. A container of non-copyable things is just `Vec File`, and it is non-copyable itself.

### 2.2 The invariant

**Two live names never share storage.** That includes parts: a field belongs to its record's owner.

A *second live name* for a value appears when the program:

1. binds it again, `let b = a`, and `a` is still used afterwards;
2. returns a bare parameter, or a result that holds it;
3. stores it in a record, list or other value, and the old name is still used afterwards;
4. captures it in a lambda, and the old name is still used afterwards;
5. passes it to a `*` parameter, and the old name is still used afterwards;
6. binds or hands on a field, `let g = s.items`, and `s.items` is still used afterwards, directly or through `s`.

At each such point exactly one of these happens:

| The old name is... | The type is... | Result |
|---|---|---|
| never used again | any | **move**: no copy, the new name takes over |
| used again | copyable | **clone**: the new name gets its own copy (implicit, reported by `--report-costs`) |
| used again | not copyable | **compile error** |

Shadowing is prohibited: a name is bound once, so the rules above never have to tell two bindings of one name apart. Use fresh names, or split a long function.

Passing a name to a bare (read) parameter creates no second name: the callee only reads.

**Fields.** A field is treated like a whole value. Passing `s.items` to a bare parameter is a read. Binding it, or handing it on in any of the ways above, is a second name: a move if `s.items` is not used again, otherwise a clone or an error by the table. A record update refills a field that was moved out:

```haskell
type State = State { items : Grid, count : Int };
fn add (Int, *State) -> State;
fn add x s = s { items = set x 0 s.items, count = s.count + 1 };
```

Here `s` is owned (`*`). `s.items` moves out into `set` and is refilled by the update, and `s.count` is only read. The compiler tracks liveness per **field path**, which is plain liveness and needs no counts.

### 2.3 Parameters and results

| Parameter | Meaning |
|---|---|
| bare `T` | a **read**. The caller keeps its value. The body may not return, store or capture it without a clone, which for a non-copyable type is an error |
| `*T` | a **take**. The function owns the argument, including everything reachable from it: it may mutate it, return it or store it. The argument must be at its last use; otherwise see the table above |

| Result | Meaning |
|---|---|
| `-> U` | an ordinary value. For an extern function it never aliases a parameter (see P7) |
| `-> (…, *T)` | hands back a `*T` parameter: **the same object**, not just another value of type `T` |
| `-> *T` with no matching `*T` parameter | **illegal**. Nothing is being handed back; for a fresh or different value write `-> T` |

A tuple in a result position is a list of results, not data.

**Hand-back is identity [decided].** A `*T` result is not "a unique `T`": it is the same ownership chain as one of the `*T` parameters, with the same storage. That makes `*Console` one linear chain from the caller's `console` to the `console2` it gets back.

- **Pairing.** The k-th `*T` result is the k-th `*T` parameter of type `T`, in order. `fn(*A, *A) -> (*A, *A)` hands back the first to the first and the second to the second. One parameter is never handed back twice, and a `*T` result with no unused `*T` parameter is the error above.
- **Checking a body.** In a function, system or lambda, the result slot must be one of four forms, and the parameter must not be used afterwards (a move, never a clone):
  1. the parameter itself: `fn id x = x`;
  2. a record update of it: `s { items = set x 0 s.items }` (2.2);
  3. a **refill**: a tuple or a single-constructor value built from the parts the parameter was destructured into, in the same branch: `fn bump (a, b) = (f a, g b)`, `fn step (State a b) = State (f a) b`. The backend overwrites the fields in the old storage.
  4. a **call that hands it back**: a call whose result slot is paired (by the rule above) with a `*` parameter, and the argument at that position is the parameter itself: `fn g x = f x` with `f : *A -> *A`. The identity passes through the call, because `f` hands back the object it was given.

  Anything else in the slot is an error: a fresh value, another parameter, an alias (`let y = x in y` after `x` was used again) or a clone. Such a function takes the value with `*T` and returns `-> T`: it consumes the argument and returns a new value, and the compiler may reuse the storage as an optimization, but the caller cannot rely on it.
- **Lambdas.** A lambda has no declaration, so its result marks are inferred from the body by the same four forms: a slot is `*T` only if the body hands back one of the lambda's own `*` parameters, written as a plain name, which occurs once in the result. Otherwise it is `T`, which is always a valid description.
- **Branches.** Every branch of an `if` or a `match` must hand back the paired parameter, each in one of the forms above. If one branch hands it back and another returns a fresh value, it is an error.
- **Refill is limited to types of fixed size** for now: tuples, records and types with one constructor. For types with several constructors, and for recursive types whose variants live on the heap with different sizes, `-> *T` is rejected and the function is written `-> T`.
- **Checking an extern.** The signature is trusted (P7), as before.
- **Calls.** The caller may rely on the identity: the result lives where the argument lived, so the backend may pass a pointer in and out instead of copying the value (P7).
- **Function types.** Marks are compared strictly. A `fn(*a) -> *a` is not usable where `fn(*a) -> a` is expected, because the two are called differently.

Function types and signatures carry the same marks: each argument is a type plus a uniqueness mark, and so is each result (`fn(*a, b) -> *a`). Two function types are equal only if their marks match.

### 2.4 What a signature says

| Signature | Meaning |
|---|---|
| `T -> U`, `U` unrelated to `T` | a read |
| `T -> T` | reads its argument and returns a **new** value: `reverse`, `sorted`, `cloneVector : FFIVector -> FFIVector` |
| `*T -> *T` | a write: takes the value and hands **the same one** back changed |
| `*T -> (U, *T)` | a write that also returns something, e.g. `readInt : *Console -> (Int, *Console)` |
| `*T -> U` | consumes the value: `close : *File -> ()`, `finish : *StringBuilder -> String` |
| `() -> T` | builds a fresh value: `newVec : () -> Vec a` |

Side effects are part of this: anything that changes the world takes its handle with `*` and hands it back. A plain read `T -> U` is pure, so the compiler is free to merge or reorder such calls.

### 2.5 Copyable types

- **`@Copyable`** is an explicit opt-in on a regular type: the programmer decides that it may be cloned implicitly. All its fields must be copyable, or it is an error. Tuples are copyable when their elements are. A generic type is copyable **conditionally**: `@Copyable type Option a` makes `Option Int` copyable and `Option File` not, as Rust's `#[derive(Clone)]` does. Nothing is copyable without an annotation, enums included: `@Pod type Dir = Up | Down;` is short, and an automatic exception would make the rule inconsistent. [implemented]
- **`@Copyable(fn)`** names the function that copies the type, `fn : (T) -> T`. It is required for a type with `@Drop`, because such a type owns memory through a pointer, and copying its fields would give two owners of one buffer (`memory.md`, 9). The compiler calls `fn` wherever a clone happens, including `copy e`. Plain `@Copyable` on a type with `@Drop` is an error.
- **`@Pod`**: bit-copyable. On a type defined in Xast it is **checked**: a single-constructor product of numbers and other `@Pod` types, or an enum of unit constructors. No ADTs, no generics, no zero-sized fields (they are removed from the layout). On an `extern` type it is a **promise** of the author: `@Pod extern type Int;`. `@Pod` implies `@Copyable`. **`@Pod` and `@Drop` exclude each other**: a type that frees something owns it, so a bit copy is never a valid copy of it. `RawPtr` is `@Pod`: its copy is a copy of the address, and it owns nothing.
- **Extern types are not copyable by default.** A copy of an opaque type is made by an ordinary extern function the programmer calls: `extern fn cloneVector (FFIVector) -> FFIVector`, and a second name for one is an error. The author can opt in with `@Copyable(fn)`, promising that `fn` really copies, as `@Pod` is a promise on an extern type: `@Drop(freeVector) @Copyable(cloneVector) extern type FFIVector;`. Plain values such as `Int` are `@Pod`, not `@Copyable(fn)`: a bit copy needs no function, and it lets a `Vec Int` be copied in one go.
- A type with a non-copyable field is non-copyable.
- A type with **`@Drop(fn)`** is dropped at its last use: the compiler calls `fn` there (`memory.md`, 4).
- A non-copyable type without `@Drop` must be consumed **exactly once**: by a consuming function, by being returned, or by being handed on. Dropping it silently is an error.

### 2.6 Implicit and explicit mode

Everything above says where a clone **happens**. Whether the programmer must also **write** it down is a setting of the package, in its `xast.toml`:

```toml
[dataflow]
explicit_copy   = false    # default: clones are inserted and reported
explicit_boxing = false    # default: recursive fields are boxed implicitly (memory.md, 8)
```

Two keywords carry the annotations:

- **`copy e`** is an expression: a fresh copy of the value of `e`, whose type must be `@Copyable`. It is a keyword, not a function, so a wrapper (`fn clone2 x = copy x`) or an alias cannot stand in for it: a clone is explicit only when `copy` is written at that exact place. When a function value is needed, write a lambda: `.\x -> copy x`.
- **`boxed`** marks a recursive field in a type definition (`memory.md`, 8).

| | implicit (`false`) | explicit (`true`) |
|---|---|---|
| a clone is needed, `copy` is missing | the clone is inserted and reported | **error**: "write `copy a`" |
| `copy` is written where no clone is needed (the old name is dead: a move) | warning: redundant | warning: redundant |
| a recursive field has no `boxed` | boxed implicitly, reported | **error**: "mark it `boxed`" |
| `boxed` is written | warning: could be omitted | required |

Rules:

- **The modes change what is required, never what a program means.** Both compile a valid program to the same code; only the number of required annotations differs. This must stay true, or the modes become dialects.
- **Per package.** Each package, Std included, is checked under the mode of its own `xast.toml`. A strict package may use a lenient one: the clone happens in the lenient package's code, written implicitly there, and no discrepancy arises.
- **`copy` on a non-copyable value is an error** in both modes.
- **Warnings are not mandatory.** Whether a redundant `copy` or `boxed` is an error, a warning or silent is a lint setting, to be added later, also in `xast.toml`. [later]
- **Migration.** A tool that inserts or removes the annotations to match a package's mode, for example `xastc fix`, would make switching one command. [idea; much else comes first]

## 3. Worked examples

A copyable record: a clone only where a name is used again.

```haskell
@Copyable type Grid = Grid { cells : List Int };
fn newGrid () -> Grid;
fn size    (Grid) -> Int;
fn set     (Int, Int, *Grid) -> Grid;

let a = newGrid,
let n = size a,            -- a read
let c = set 0 5 a          -- a is at its last use: moved in, written in place
in (c, n)

let a = newGrid,
let b = set 0 5 a,         -- a is used again below: the call gets a clone, and a keeps the original
let n = size a
in (b, n)
```

An opaque vector, used only through `*` parameters:

```haskell
extern fn newVec () -> Vec a;
extern fn push   (Int, a, *Vec a) -> *Vec a;
extern fn len    (Vec a) -> Int;

fn range (Int) -> Vec Int;
fn range n = fill n newVec;                -- newVec is a temporary: moved in, zero clones

let v1 = newVec,
let v2 = push 1 x v1,       -- v1 is at its last use: mutated in place
let n  = len v2,            -- a read: v2 stays alive
let v3 = push 2 y v2        -- v2 is at its last use: in place
in v3
```

A shared opaque value must be copied by hand, because the compiler never clones an opaque type:

```haskell
fn sorted (*Vec a) -> Vec a;
let a = sorted shared               -- error if `shared` is used again: Vec is not copyable
let b = sorted (cloneVec shared)    -- explicit copy through the extern function
```

A resource that must not be copied:

```haskell
fn readInt (*Console) -> (Int, *Console);
fn width   (Console)  -> Int;                 -- a read; legal, but Console has no read functions to call

let (n, console2) = readInt console           -- console is consumed; console2 continues
let again = readInt console                   -- error: `console` was already consumed
```

## 4. Open questions

1. **A counted, opt-in shared type** (P15): later, with persistent data structures.
2. **Lint levels** (error, warning, silent) for redundant `copy` and `boxed`, in `xast.toml`: later.
3. **`xastc fix`** to switch a package between the modes (2.6): an idea.

## 5. Related documents

- *Memory management* is in [memory.md](memory.md): no garbage collector, one owner per value, freeing at the last use of the owner, `@Drop` for opaque types that own memory, no escaping lambdas that capture values, indirection through `Std.Alloc`.
- The `Std.Alloc` intrinsics (`alloc`, `realloc`, `free`, `ptrRead`, `ptrWrite`, `ptrOffset`, `ptrCopy`, `ptrMove`) and `sizeOf` are where in-place mutation of raw storage is implemented. They are unsafe and outside this model.
