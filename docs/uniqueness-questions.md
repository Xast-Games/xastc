# Uniqueness: design questions

Each question is a situation the model in [uniqueness.md](uniqueness.md) has to handle, with the rule that answers it. Section numbers such as 2.3 refer to [uniqueness.md](uniqueness.md); numbers such as P7 refer to the problems in [uniqueness-questions.md](uniqueness-questions.md) (P1-P15, P17), [uniqueness-ecs.md](uniqueness-ecs.md) (P16) and [uniqueness-std.md](uniqueness-std.md) (P18, P19).

## P1. Mutating without copying

**Problem.** `push` on a vector should not copy it every time.

**Solution.** The operation has a `*` parameter, so the callee owns the argument and never clones. The caller decides, per call: **move** at the last use, **implicit clone** (copyable type, reported), **explicit clone function** (opaque type), or **error** (non-copyable). One function serves both a unique and a shared argument; no separate wrappers are needed.

## P2. A second name for the same value

**Problem.** After `let b = a`, mutating `a` would also change `b`.

**Solution.** The invariant in 2.2, applied to all six situations in the list, not only to function results. If `a` is dead afterwards it is a move and nothing is copied.

## P3. Reading must not cost anything

**Problem.** `get 0 v` only looks inside `v`. Copying would be absurd, and returning the vector in a tuple is awkward.

**Solution.** A bare parameter is a read, and a read creates no second name. For an extern function the signature is trusted under the contract in P7. For a function with a body, the check is the same clone-or-error rule: if the body would have to return, store or capture the parameter, it must clone it, and for a non-copyable type that is an error inside that function. A read-only parameter needs no escape rules of its own.

## P4. A function that returns its own argument

**Problem.** `fn id v = v` would give the caller a second name for the same storage.

**Solution.** For a copyable type the clone goes inside the function, on the path that returns the parameter. The caller does nothing. For a non-copyable type it is an error.

```haskell
fn id (Grid) -> Grid;             -- clone placed at `v`
fn id2 (Grid) -> Grid;
fn id2 v = copy v;                -- the same, written explicitly with the `copy` keyword (2.6)
```

A function that builds a new value by reading (`reverse`) never clones.

## P5. A bare parameter passed on to a `*` parameter

**Problem.** `fn f (Grid) -> ...` passes its parameter `g0` to `set`, which takes ownership. The function does not own `g0`.

**Solution.** It must copy `g0` at that point: **one clone per call**, not per mutation. For a non-copyable or opaque type it is an error, so the function should take `*Grid` itself. Declaring the parameter with `*` moves the decision to the caller (P1).

## P6. Values that must never be copied

**Problem.** A console, a graphics context or a builder cannot be cloned.

**Solution.** The type is non-copyable (extern types are by default). A bare parameter is a read; a `*` parameter is a take; a result of such a type is simply a value the caller now owns. Consuming functions are fine on their own: the error appears at the **next use**, with two markers: where the value was consumed, and where it was needed.

```haskell
fn finish (*StringBuilder) -> String;

let s = finish builder            -- fine
let t = finish builder            -- error: `builder` was already consumed
```

## P7. Extern functions and opaque types

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

## P8. Loops written as tail recursion

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

## P9. Helpers and callbacks in loops

**Problem.** A helper or a lambda that takes a bare parameter and passes it on clones on every call:

```haskell
fn step (Int, Grid) -> Grid;
fn step x g = set x 0 g;                                  -- clones g: it is a read parameter
foldl (.\acc x -> set x 0 acc) newGrid xs                 -- acc is a read parameter of the lambda: a clone for every element
```

**Solution.** Declare the parameter with `*`: `fn step (Int, *Grid) -> Grid`. A function type carries the same marks (2.3), so `foldl` can take a callback that owns its accumulator. Small helpers can also be inlined (`@Inline` exists). A function that needs both behaviours has two variants (P19).

## P10. Reading or changing a part of a container

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

## P11. Branches

**Problem.** A name that appears in both arms of an `if` is used twice in the text but once on each path.

**Solution.** Liveness is computed backward, per path, and per field path. For each instruction, a name used here that is not needed afterwards is at its last use (a move); otherwise it is a second name. A `match` or `if` analyzes each arm separately, starting from the set needed after the whole expression; what is needed before it is the scrutinee plus the union of the arms. Using a name twice in one instruction (`f v v`) makes the second use a second name. Loops need a fixpoint (P8).

```haskell
fn g (*Vec Int, Bool) -> Vec Int;
fn g v flag = if flag then push 1 v else push 2 v;      -- v is dead after each arm: both move
```

If `v` is used after the `if`, both arms need a clone (copyable) or it is an error.

## P12. Lambdas

**Solution.** Capturing a name in a lambda is a use at the point where the lambda is created: a move or a clone like any other second name. A lambda that captures values must not be returned or stored.

## P13. Generic functions

**Problem.** `fn dup (a) -> (a, a)` creates two names for one value: fine for `Int`, impossible for `Console`. Reading every body again at monomorphization would be too much.

**Solution.** During analysis, compute and cache a **summary per function**: for each parameter, whether it is only read, whether it can flow into the result or into storage, and whether it is duplicated. For user functions the summary comes from the body's data flow; for externs from the signature. "Needs a copyable `a`" is an inferred requirement, reported at the call site with the reason ("`dup` uses its argument twice, at line 3"). Monomorphization only consults the summaries. Mutually recursive functions need a fixpoint. A generic parameter may carry `*`: `fn id (*a) -> a`.

## P14. Hidden costs

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

## P15. Sharing one large value among many holders [out of scope]

Because every value has one owner, sharing a large immutable structure costs a copy. A counted, opt-in shared type is a library and memory-model question. See [memory.md](memory.md).

## P17. Fewer clones: lazy cloning [later]

**Problem.** The eager rule clones at a second name even when the clone turns out to be wasted: the alias is only read, or it dies before anything writes.

```haskell
let b = a,                 -- a is used below: eager rule clones here
let n = size b,            -- b is only read, and dead afterwards
let c = set 0 5 a          -- a could have been written in place
```

**Solution.** A later analysis may postpone the clone to the first write while another name is still live, tracking, per program point, how many other names share the storage (none, one, several or unknown; unknown means shared). It only **removes** clones and never changes what a program means. Because shared storage makes a wrong count a correctness bug, it must be conservative, and the eager rule stays as the reference to test it against. First step: a dry run in `--report-costs` that lists the clones it would remove ("`b` is only read: this clone could be a share"), so the gain is measured before anything is built. [later]
