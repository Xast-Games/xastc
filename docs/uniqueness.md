# Uniqueness and ownership in Xast

**Status:** design draft. Nothing here is implemented yet.
**Supersedes:** the earlier drafts with `*T` as a type, with inference only, and with lazy cloning. The alternatives are in [section 9](#9-alternatives-considered).
**See also:** [memory.md](memory.md) for freeing, reuse and sharing.

Each item is marked:

- **[decided]**: agreed in the design discussion.
- **[proposed]**: follows from the decided parts, not yet confirmed.
- **[open]**: needs a decision before it can be built.

## 1. Goals

- Values are immutable by default: a program reads like ordinary functional code.
- Performance close to Rust's: values are moved, not copied; mutation happens in place; no garbage collector, no run-time counters, no lifetimes, no borrow checker.
- A copy happens only where a name is used again, exactly where Rust would need `.clone()`, and every such copy is reported.
- Things that must not be copied (a console, a graphics context, an ECS command buffer) are protected by the same rules, with errors instead of copies.
- Side effects are explicit in signatures.

## 2. The model

### 2.1 Ownership and modes [decided]

Types are ordinary. Every value has exactly **one owner** at any point: a name, or a part of the value another name owns. The compiler knows from liveness (is the name used later on this path?) whether a use hands the value over or needs a copy.

`*` is an annotation on a **function parameter**: "this function takes ownership of the argument and may mutate it". A bare parameter is a read.

`*` is **not a type**. Mutation is behavior, and a signature describes behavior; a type describes data. So there is no `Option *T`, no `Vec *File`, no `**T`, and no `*` in record fields. A container of non-copyable things is just `Vec File`, and it is non-copyable itself.

### 2.2 The invariant [decided]

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

Passing a name to a bare (read) parameter creates no second name: the callee only reads.

**Fields.** A field is treated like a whole value. Passing `s.items` to a bare parameter is a read. Binding it, or handing it on in any of the ways above, is a second name: a move if `s.items` is not used again, otherwise a clone or an error by the table. A record update refills a field that was moved out:

```haskell
type State = State { items : Grid, count : Int };
fn add (Int, *State) -> State;
fn add x s = s { items = set x 0 s.items, count = s.count + 1 };
```

Here `s` is owned (`*`). `s.items` moves out into `set` and is refilled by the update, and `s.count` is only read. The compiler tracks liveness per **field path**, which is plain liveness and needs no counts.

### 2.3 Parameters and results [decided]

| Parameter | Meaning |
|---|---|
| bare `T` | a **read**. The caller keeps its value. The body may not return, store or capture it without a clone, which for a non-copyable type is an error |
| `*T` | a **take**. The function owns the argument, including everything reachable from it: it may mutate it, return it or store it. The argument must be at its last use; otherwise see the table above |

| Result | Meaning |
|---|---|
| `-> U` | an ordinary value. For an extern function it never aliases a parameter (see P7) |
| `-> (…, *T)` | hands back a `*T` parameter: the same object |
| `-> *T` with no matching `*T` parameter | **illegal**. Nothing is being handed back, and semantically it would be just `-> T` |

A tuple in a result position is a list of results, not data.

Function types and signatures carry the same marks: each argument is a type plus a uniqueness mark, and so is each result (`fn(*a, b) -> *a`). Two function types are equal only if their marks match.

### 2.4 What a signature says [decided]

| Signature | Meaning |
|---|---|
| `T -> U`, `U` unrelated to `T` | a read |
| `T -> T` | reads its argument and returns a **new** value: `reverse`, `sorted`, `cloneVector : FFIVector -> FFIVector` |
| `*T -> *T` | a write: takes the value and hands it back changed |
| `*T -> (U, *T)` | a write that also returns something, e.g. `readInt : *Console -> (Int, *Console)` |
| `*T -> U` | consumes the value: `close : *File -> ()`, `finish : *StringBuilder -> String` |
| `() -> T` | builds a fresh value: `newVec : () -> Vec a` |

Side effects are part of this: anything that changes the world takes its handle with `*` and hands it back. A plain read `T -> U` is pure, so the compiler is free to merge or reorder such calls.

### 2.5 Copyable types [decided]

- **`@Copyable`** is an explicit opt-in on a regular type: the programmer decides that it may be cloned implicitly. All its fields must be copyable, or it is an error. Tuples are copyable when their elements are. A generic type is copyable **conditionally**: `@Copyable type Option a` makes `Option Int` copyable and `Option File` not, as Rust's `#[derive(Clone)]` does. Nothing is copyable without an annotation, enums included: `@Pod type Dir = Up | Down;` is short, and an automatic exception would make the rule inconsistent. [implemented]
- **`@Copyable(fn)`** names the function that copies the type, `fn : (T) -> T`. It is required for a type with `@Drop`, because such a type owns memory through a pointer, and copying its fields would give two owners of one buffer (`memory.md`, 9). The compiler calls `fn` wherever a clone happens, including `copy e`. Plain `@Copyable` on a type with `@Drop` is an error.
- **`@Pod`**: bit-copyable. On a type defined in Xast it is **checked**: a single-constructor product of numbers and other `@Pod` types, or an enum of unit constructors. No ADTs, no generics, no zero-sized fields (they are removed from the layout). On an `extern` type it is a **promise** of the author: `@Pod extern type Int;`. `@Pod` implies `@Copyable`. **`@Pod` and `@Drop` exclude each other**: a type that frees something owns it, so a bit copy is never a valid copy of it. `RawPtr` is `@Pod`: its copy is a copy of the address, and it owns nothing.
- **Extern types are not copyable by default.** A copy of an opaque type is made by an ordinary extern function the programmer calls: `extern fn cloneVector (FFIVector) -> FFIVector`, and a second name for one is an error. The author can opt in with `@Copyable(fn)`, promising that `fn` really copies, as `@Pod` is a promise on an extern type: `@Drop(freeVector) @Copyable(cloneVector) extern type FFIVector;`. Plain values such as `Int` are `@Pod`, not `@Copyable(fn)`: a bit copy needs no function, and it lets a `Vec Int` be copied in one go.
- A type with a non-copyable field is non-copyable.
- A type with **`@Drop(fn)`** is dropped at its last use: the compiler calls `fn` there (`memory.md`, 4).
- A non-copyable type without `@Drop` must be consumed **exactly once**: by a consuming function, by being returned, or by being handed on. Dropping it silently is an error.

### 2.6 Implicit and explicit mode [decided]

Everything above says where a clone **happens**. Whether the programmer must also **write** it down is a setting of the package, in its `xast.toml`:

```toml
[ownership]
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

## 4. Problems and solutions

### P1. Mutating without copying [decided]

**Problem.** `push` on a vector should not copy it every time.

**Solution.** The operation has a `*` parameter, so the callee owns the argument and never clones. The caller decides, per call: **move** at the last use, **implicit clone** (copyable type, reported), **explicit clone function** (opaque type), or **error** (non-copyable). One function serves both a unique and a shared argument; no separate wrappers are needed.

### P2. A second name for the same value [decided]

**Problem.** After `let b = a`, mutating `a` would also change `b`.

**Solution.** The invariant in 2.2, applied to all six situations in the list, not only to function results. If `a` is dead afterwards it is a move and nothing is copied.

### P3. Reading must not cost anything [decided]

**Problem.** `get 0 v` only looks inside `v`. Copying would be absurd, and returning the vector in a tuple is awkward.

**Solution.** A bare parameter is a read, and a read creates no second name. For an extern function the signature is trusted under the contract in P7. For a function with a body, the check is the same clone-or-error rule: if the body would have to return, store or capture the parameter, it must clone it, and for a non-copyable type that is an error inside that function. A read-only parameter needs no escape rules of its own.

### P4. A function that returns its own argument [decided]

**Problem.** `fn id v = v` would give the caller a second name for the same storage.

**Solution.** For a copyable type the clone goes inside the function, on the path that returns the parameter. The caller does nothing. For a non-copyable type it is an error.

```haskell
fn id (Grid) -> Grid;             -- clone placed at `v`
fn id2 (Grid) -> Grid;
fn id2 v = copy v;                -- the same, written explicitly with the `copy` keyword (2.6)
```

A function that builds a new value by reading (`reverse`) never clones.

### P5. A bare parameter passed on to a `*` parameter [decided]

**Problem.** `fn f (Grid) -> ...` passes its parameter `g0` to `set`, which takes ownership. The function does not own `g0`.

**Solution.** It must copy `g0` at that point: **one clone per call**, not per mutation. For a non-copyable or opaque type it is an error, so the function should take `*Grid` itself. Declaring the parameter with `*` moves the decision to the caller (P1).

### P6. Values that must never be copied [decided]

**Problem.** A console, a graphics context or a builder cannot be cloned.

**Solution.** The type is non-copyable (extern types are by default). A bare parameter is a read; a `*` parameter is a take; a result of such a type is simply a value the caller now owns. Consuming functions are fine on their own: the error appears at the **next use**, with two markers: where the value was consumed, and where it was needed.

```haskell
fn finish (*StringBuilder) -> String;

let s = finish builder            -- fine
let t = finish builder            -- error: `builder` was already consumed
```

### P7. Extern functions and opaque types [decided]

**Problem.** An extern function has no Xast body, so the compiler cannot check what it does.

**Solution.**
- **Contract.** The extern signature is trusted. A result never aliases a parameter, except a `*` parameter handed back. A function that mutates its argument must declare it with `*`. Breaking the contract is the author's bug.
- `T -> T` is fine: it reads and returns a new value (`cloneVector`).
- **Mapping to native calls** (C ABI):

| Xast | Native |
|---|---|
| `*T` parameter handed back in the result | passed as a pointer, changed in place; the "returned" value is the same object |
| `*T` parameter not handed back (consumed) | passed by value; the native side takes ownership |
| bare `T` parameter | by value |
| a fresh result | returned by value |

```haskell
extern fn readInt (*Console) -> (Int, *Console);      -- int32_t readInt(Console* console);
extern fn finish  (*StringBuilder) -> String;         -- String finish(StringBuilder b);
```

- `@Pod` types can be passed by value: `extern fn f (A)` with `@Pod type A = A Int Float`.

### P8. Loops written as tail recursion [decided]

**Problem.** A recursive function that mutates its parameter would clone on every iteration if it did not own it.

**Solution.** With a `*` parameter there is nothing to do: every call hands over a fresh result, so nothing is cloned.

```haskell
fn paint (Int, *Grid) -> Grid;
fn paint n g = if n == 0 then g else paint (n - 1) (set n 0 g);
```

With a bare parameter of a copyable type, the function owns nothing at the start, so the clone goes on the **entry edge** of the loop, once per call, and both edges of the loop header are then owned:

```
fn paint(n, g):
    g = clone(g)           -- once per call, at entry
    loop:
        if n == 0: return g
        g = set(n, 0, g)   -- in place
        n = n - 1
```

A self tail call is compiled as a loop, and tail calls are guaranteed [decided]. This needs a loop construct in the intermediate representation, planned for the tail-call optimization.

### P9. Helpers and callbacks in loops [decided]

**Problem.** A helper or a lambda that takes a bare parameter and passes it on clones on every call:

```haskell
fn step (Int, Grid) -> Grid;
fn step x g = set x 0 g;                                  -- clones g: it is a read parameter
foldl (.\acc x -> set x 0 acc) newGrid xs                 -- acc is a read parameter of the lambda: a clone for every element
```

**Solution.** Declare the parameter with `*`: `fn step (Int, *Grid) -> Grid`. A function type carries the same marks (2.3), so `foldl` can take a callback that owns its accumulator. Small helpers can also be inlined (`@Inline` exists). A function that needs both behaviours has two variants (P19).

### P10. Reading or changing a part of a container [decided as a direction]

**Problem.** `get : Int, Vec a -> Maybe a` with `a = Vec Int` returns an inner vector that lives in the outer one. Mutating the outer vector later could change it.

**Solution.**
- For an extern container, the contract in P7 makes the result independent: the native side returns a copy, or the function does not exist.
- For a container defined in Xast, access through a callback, so the part never escapes:

```haskell
fn withAt (Int, Vec a, fn(a) -> r) -> Maybe r;
let total = withAt 0 outer (.\inner -> sum inner)      -- inner is not cloned
```

The callback is a read, so `r` does not contain `inner`. If it does (`.\inner -> inner`), P4 applies: the clone happens at that return.

- To change a part in place, take it out of its slot while the callback runs:

```haskell
fn modifyAt (Int, *Vec (Vec a), fn(*Vec a) -> *Vec a) -> *Vec (Vec a);
```

### P11. Branches [decided]

**Problem.** A name that appears in both arms of an `if` is used twice in the text but once on each path.

**Solution.** Liveness is computed backward, per path, and per field path. For each instruction, a name used here that is not needed afterwards is at its last use (a move); otherwise it is a second name. A `match` or `if` analyses each arm separately, starting from the set needed after the whole expression; what is needed before it is the scrutinee plus the union of the arms. Using a name twice in one instruction (`f v v`) makes the second use a second name. Loops need a fixpoint (P8).

```haskell
fn g (*Vec Int, Bool) -> Vec Int;
fn g v flag = if flag then push 1 v else push 2 v;      -- v is dead after each arm: both move
```

If `v` is used after the `if`, both arms need a clone (copyable) or it is an error.

### P12. Lambdas [decided]

**Solution.** Capturing a name in a lambda is a use at the point where the lambda is created: a move or a clone like any other second name. A lambda that captures values must not be returned or stored.

### P13. Generic functions [decided]

**Problem.** `fn dup (a) -> (a, a)` creates two names for one value: fine for `Int`, impossible for `Console`. Reading every body again at monomorphization would be too much.

**Solution.** During analysis, compute and cache a **summary per function**: for each parameter, whether it is only read, whether it can flow into the result or into storage, and whether it is duplicated. For user functions the summary comes from the body's data flow; for externs from the signature. "Needs a copyable `a`" is an inferred requirement, reported at the call site with the reason ("`dup` uses its argument twice, at line 3"). Monomorphization only consults the summaries. Mutually recursive functions need a fixpoint. A generic parameter may carry `*`: `fn id (*a) -> a`.

### P14. Hidden costs [decided]

**Problem.** An edit that uses a name later can turn a move into an implicit clone, silently.

**Solution.** One report, `xastc check --report-costs`, lists what the compiler inserted, in two sections. **Clones** lists every clone, with the reason and where the cost lands. **Allocations** lists every field the compiler boxes, and so which constructors allocate (`memory.md`, 8).

```
clones

Main.id  (Main.xst:3)
  clone of `v` : Grid           at 4:13   `v`
    why:  returned while the caller may still be using it
    cost: copies the whole Grid once per call

Main.paint  (Main.xst:8)
  clone of `g` : Grid           at 9:38   `set n 0 g`
    why:  `g` is a read parameter, so it may be shared, and `set` takes ownership of it
    cost: copies the Grid on EVERY call; paint calls itself, so once per iteration

2 clones in 2 functions, 1 of them inside a recursive call

allocations

Main.List  (Main.xst:12)
  boxed: second field of `Cons` : List a
    why:  `List` contains itself
    cost: every `Cons` allocates one node

1 boxed field in 1 type
```

A function that is not listed under clones has no hidden copies. `why` names the line that forced the clone. `cost` is the part to watch: "once per call" is usually fine, "once per iteration" is worth fixing, usually by declaring the parameter with `*`.

### P15. Sharing one large value among many holders [out of scope]

Because every value has one owner, sharing a large immutable structure costs a copy. A counted, opt-in shared type is a library and memory-model question. See [memory.md](memory.md).

### P16. ECS resources and the old effect list [decided]

**Problem.** Systems should not return heterogeneous effect lists such as `((compA, compB), List Effect)`.

**Solution.** Resources are non-copyable values, passed and handed back explicitly. In a system, each `#( … )` is a query group (one entity), and resources follow after `|`:

```haskell
@Singleton extern type CommandBuffer;
@Singleton extern type Console;
@Singleton type Gravity = Gravity Float;
fn spawn   (Components, *CommandBuffer) -> *CommandBuffer;
fn pending (CommandBuffer) -> Int;                        -- a read

system Move #(Pos, Vel), #(Camera) | *Console, Gravity -> (
    Pos,          -- group 1: Pos is written, Vel is read
    (),           -- group 2: Camera is only read
    *Console
);
```

- **Components** carry no `*`: a component is written if it is in the result and read otherwise.
- **The result** is a tuple: one slot per query group, in order, then one slot per `*R`. A slot holds the components its group writes (a tuple if several), or `()` for none. Each slot may have a `--` comment. A tuple of one slot is the slot itself, so a system with one group and no `*R` keeps the current form: `system Move #(Pos, Velocity) -> Pos;`.
- **Resources** after `|` keep `*`: `*R` is a write and a bare `R` a read. A resource type must be `@Singleton`; otherwise it is a compile error.
- A system that receives `*R` **must hand it back**. If the body consumes it (`finish builder`), that step is fine, but the result then asks for a value that is gone: the use-after-move error of P6, at the result.
- A system may create its own local non-copyable values and consume them (a `*File` opened and closed within one run).
- A system could consume a received resource and return a *fresh* one of the same type. The rules accept that; whether it is acceptable is an ECS policy.
- The signature tells read from write: two systems that only read a resource do not conflict, so they can run in parallel; any `*R` conflicts with every other user of `R`.
- **Overlapping groups.** If the components of one query group are all contained in another group, every entity that matches the larger group also matches the smaller one, so two groups can denote the same entity. That is a compile error: `#(A), #(A)` or `#(A, B), #(A)`. Tell the groups apart with tags: `#(Tag1, A), #(Tag2, A)`. An entity that carries both tags still matches both groups; that case cannot be seen at compile time and is the programmer's responsibility.
- **Writes in a multi-group system.** A system with several query groups runs over their combinations, so a written component of one group can be written once per entity of another group (each `Enemy`'s `Transform` once per `Player`). That is the programmer's responsibility, but `@Parallel` on a system with several groups that writes any of them is an error, since such writes could race.

### P17. Fewer clones: lazy cloning [later]

**Problem.** The eager rule clones at a second name even when the clone turns out to be wasted: the alias is only read, or it dies before anything writes.

```haskell
let b = a,                 -- a is used below: eager rule clones here
let n = size b,            -- b is only read, and dead afterwards
let c = set 0 5 a          -- a could have been written in place
```

**Solution.** A later analysis may postpone the clone to the first write while another name is still live, tracking, per program point, how many other names share the storage (none, one, several or unknown; unknown means shared). It only **removes** clones and never changes what a program means. Because shared storage makes a wrong count a correctness bug, it must be conservative, and the eager rule stays as the reference to test it against. First step: a dry run in `--report-costs` that lists the clones it would remove ("`b` is only read: this clone could be a share"), so the gain is measured before anything is built. [later]

### P18. Operators and `<>` on collections [decided]

**Problem.** `<>` should work on the Std collections, both on a value at its last use and on one still in use, and every new collection should not need a new mechanism. Letting user types define operators (`@Concat(fn)`, `@Add(fn)`) would amount to traits, which the language avoids.

**Solution.**
- **Operators belong to Std.** `number` and `concatenative` stay closed kinds, as in Java or Go. Std types are added to them; user types never are. Math types (`Vec3`, `Mat4`) use named functions and pipes: `Mat.identity |> Mat.mul m2 |> Mat.addVec v2`.
- **`concatenative`** holds `String`, `List`, `Vec`, `RRB` and `ChunkedVec` (`memory.md`, 9). Maps, sets and queues come later.
- **One signature for both cases**, taking both operands:

```haskell
@Intrinsic
extern fn opConcat (*concatenative, *concatenative) -> concatenative;
```

The caller moves an operand at its last use and clones it otherwise, as for any `*` parameter (P1). There are no separate operators for owned values:

| At `a <> b` | What happens |
|---|---|
| both dead afterwards | `b` is appended into `a`'s storage: O(\|b\|) for `Vec`, O(log n) relinking for `RRB` |
| `a` used later | `a` is cloned first, which costs what building a new value would |
| `b` used later | `b` is cloned, then its elements move |
| non-copyable elements | they move; a read-only right operand could only copy them |

The right operand is taken so that non-copyable elements can move. For `String` this costs a clone of `b` when it is still used, where a read would do; removing it is a later optimization (P17).

- **Chains** such as `a <> b <> c <> d` grow the left buffer several times. A later optimization can compute the total size and allocate once, as Java does for string concatenation. [later]
- **`|>` is syntax, not a function** [decided, implemented]: the parser turns `x |> f` into `f x`. A function `opPipe (a, fn(a) -> b) -> b` would accept only reading functions, since the marks are part of function types (2.3), so `v |> push 1` could not type-check, and its bare `a` passed on to a `*` parameter would clone (P5). As plain application, the ordinary rules apply, and `x |> f 1` needs no partial application.

### P19. Unique and shared variants [decided]

**Problem.** One function cannot both read its argument and take it: `map (fn(a) -> b, Vec a) -> Vec b` builds a new vector, while `map' (fn(*a) -> *a, *Vec a) -> *Vec a` changes it in place. The marks are part of the signature (2.3), so neither replaces the other.

**Solution.**
- **Two functions, by convention `f` and `f'`.** `'` is allowed in function names. A single `'` at the end marks the unique variant; `'` anywhere else (`map'wrong`, `map''`) is a warning. [implemented] Later, a name ending in `'` should also have a `*` parameter.
- **`@SharedVariant(f)`** on the unique variant links it to its shared one, whatever the names (`sort'` and `sorted`):

```haskell
@SharedVariant(map)
fn map' (fn(*a) -> *a, *Vec a) -> *Vec a;
```

The compiler checks that `f` exists and has the same signature without the marks (`*Vec a` becomes `Vec a`). Its use is a hint where the unique variant is called on an argument that is still used: with the error for a non-copyable type, or next to the clone in `--report-costs`:

```
error: `xs` is used after it was given to `map'`, and `Vec File` is not copyable
  hint: `map` is the shared variant of `map'`: it reads `xs` instead of taking it
```

The opposite hint (an argument at its last use given to `map`, where `map'` would avoid a copy) is possible later.

- **Partial application becomes a lambda**, in a desugaring pass, so the lowerer only sees full calls and lambdas: `f a` with `f : fn(A, B) -> C` becomes `let x = a in .\y -> f x y`. The arguments are evaluated once, when the lambda is created, and capturing them is a move or a clone (P12).
- **Closures can live on the stack.** A lambda that captures values never escapes (`memory.md`, 1), so a closure is an environment and a function, with no allocation. Later, a higher-order function can be specialized per lambda through monomorphization, so its calls become direct and can be inlined, as Rust does with closures. [later]

## 5. What the compiler does

For each function body, after type checking and desugaring, a new pass:

1. **Liveness**, per path and per field path (P11).
2. **Second names**: find the six situations in 2.2. Choose move, clone or error with the table there.
3. **Call classification** from the parameter marks (2.3) and, for user functions, from their summaries (P13).
4. **Loops**: place the entry-edge clone for a bare parameter (P8).
5. **Report**: record every inserted clone with its reason and whether it is inside a loop.

The result is a table keyed by the source position of each second name ("move", "clone" or "error") plus the summaries. Lowering reads the table and emits the clones. The intermediate representation carries no ownership logic, and errors have source locations.

## 6. Settled and open

**Settled in the discussion** (and reflected above): `*` is a parameter mode and not a type; function types and signatures carry the marks, results too (2.3); **clones are eager**, resolved at the second name, and lazy cloning is a later optimization (P17); a field is treated like a whole value (2.2); no run-time counters; shadowing is prohibited (use fresh names, split long functions); tail calls will be guaranteed; `@Copyable` is an explicit opt-in; `@Pod` is allowed for enums of unit constructors; `@Drop(fn)`, or else consumed exactly once (2.5); an extern `T -> T` is legal and means "returns a new value"; opaque types are cloned by an explicit extern function; the native mapping (P7); branches (P11); the system syntax and `@Singleton` resources (P16); one report, `--report-costs` (P14); implicit and explicit mode per package, with `copy` and `boxed` as keywords (2.6); `@Copyable(fn)` for types with `@Drop`, and `@Pod` excluding `@Drop` (2.5); operators only on Std types, and `<>` taking both operands, `|>` as plain application (P18); `@Copyable(fn)` as an opt-in on extern types (2.5); unique and shared variants `f'` and `f`, linked by `@SharedVariant`, and partial application desugared into lambdas (P19).

**Not decided:**

1. **A counted, opt-in shared type** (P15): later, with persistent data structures.
2. **Lint levels** (error, warning, silent) for redundant `copy` and `boxed`, in `xast.toml`: later.
3. **`xastc fix`** to switch a package between the modes (2.6): an idea.

## 7. Related decisions

- *Memory management* is in [memory.md](memory.md): no garbage collector, one owner per value, freeing at the last use of the owner, `@Drop` for opaque types that own memory, no escaping lambdas that capture values, indirection through `Std.Alloc`.
- The `Std.Alloc` intrinsics (`alloc`, `realloc`, `free`, `ptrRead`, `ptrWrite`, `ptrOffset`, `ptrCopy`, `ptrMove`) and `sizeOf` are where in-place mutation of raw storage is implemented. They are unsafe and outside this model.

## 8. Implementation plan

1. **Copyability** in the type checker: `@Copyable` as an explicit opt-in with the field check, `@Copyable(fn)` with its signature check, `@Pod` checked on Xast types and trusted on extern types, and `@Pod` with `@Drop` rejected. [implemented: `SemAnalyzer/Ownership.hs`; Std types annotated; `@Drop(fn)` must be `fn(*T) -> ()`] Annotate the Std types (`Int`, `Float`, `Char`, `Bool`, `Option`, `Result`, `List`, ...).
2. **Parameter modes**: parse `*` on parameters and on a handed-back result. Function types and signatures carry the marks, and results do too. Check the rules in 2.3. [implemented: parameters and results are `MarkedType`s (`Read` or `Take`) in `TyFn`, `FuncDef` and `ExternFunc`; unification compares marks; a `*` result needs a `*` parameter of its type. Resources in systems (`| *R`) belong to step 9]
3. **The analysis pass**: liveness per path and field path, second names, summaries, the table, errors with two markers. The main piece.
4. **Clone insertion** in lowering for `@Copyable` types, plus a clone operation in the intermediate representation and the backend.
5. **A loop construct** in the intermediate representation and self tail calls compiled to it.
6. **`--report-costs`**: the clones section (the allocations section is in `memory.md`).
7. **Std**: `Vec` with `*` parameters, `withAt` and `modifyAt`.
8. **The native mapping** in the backend (P7).
9. **ECS resources and the system syntax** (P16): the result slots, the `@Singleton` check, the overlapping-groups error, the `@Parallel` check. Today `@Singleton` is parsed but unused, `SrcSingleton` is never produced, and the lowerer accepts only one written binding.
10. **The modes** (2.6): the `[ownership]` table in `xast.toml`, the keywords `copy` and `boxed`, and their errors and warnings.
11. **Operators** (P18): `opConcat` with `*` operands; `concatenative` extended with the Std collections, through a Std-only marker instead of type names in the compiler.
12. **Variants** (P19): the `@SharedVariant` signature check and its hint; partial application desugared into lambdas, and lambdas lowered as closures on the stack.
13. **Later:** the lazy-clone dry run and, if it pays off, the optimization (P17); concatenation chains with one allocation (P18).

Existing gaps that examples will hit: records, `if`, string literals and lambdas are not yet lowered.

## 9. Alternatives considered

| Alternative | Why it was not chosen |
|---|---|
| `*T` as a type | Mutation is behavior, not data: `Option *T` or `Box *T` make no sense. It also needs conversions between `T` and `*T` and wrapper functions. |
| Inference only, no `*` in the source | Cannot say that a function takes ownership, cannot mark side effects, and the cost of an edit is invisible. The `*` parameter mode and the cost report keep both. |
| A separate read-only reference mode (`&`) | A bare parameter is already the read, checked by the rules in 2.2. |
| Threading every read through the result: `get : …, *Vec a -> (Maybe a, *Vec a)` | Only needed for non-copyable types, where reads are effectful anyway. Copyable types read with a plain `T -> U`. |
| A `@NoClone` function modifier | A `*` parameter already gives the guarantee. |
| Cloning inside every function at entry | Costs one clone per call even when the caller could hand the value over. Owned (`*`) parameters let the caller decide. |
| Lazy cloning with static counts as the default | Needs an alias analysis through fields, containers, closures and calls, and a wrong count corrupts data instead of slowing the program. Kept as a later optimization that can only remove clones (P17). |
| Run-time reference counts (as in Koka or Lean) | A counter in every value and counter updates on every copy and drop. Not Rust-like performance. A counted type only where the programmer asks for sharing (P15). |
| Clean-style: an error instead of a clone, copies written by hand | Smallest and fully static, but loses the functional feel. Kept for opaque types, where the clone is an explicit extern function. |

Related designs: Hylo (mutable value semantics with parameter conventions) is the closest; Clean and Futhark have uniqueness annotations.

What carried over from the earlier drafts: the signature alone tells read from take, which gives the dependency information that parallel ECS systems need.

## 10. Appendix: correspondence with Rust

| Xast | Rust | Meaning |
|---|---|---|
| `() -> T` | `() -> T` | a new value |
| `T -> T/U` | `&T -> T/U` | reading, or cloning |
| `*T -> *T` | `&mut T -> ()` or `mut T -> T` | mutating, or threading |
| `*T -> T/U` | `T -> T/U` | consuming, and maybe returning something |
| `*T -> (U, *T)` | `&mut T -> U` or `mut T -> (U, T)` | mutating and reading, or threading and reading |

### Rust shapes with no Xast counterpart

| Rust | In Xast |
|---|---|
| `&T -> &U`, a borrow into the argument | Not expressible. Use a copy, or `withAt` with a callback (P10) |
| `&'a mut T -> &'a mut U`, reborrows | `modifyAt` with a callback |
| references stored in structs, lifetimes | Not expressible: a read parameter cannot be returned, stored or captured |
| `Rc<T>` and shared mutation | An opt-in counted type, later (P15) |
