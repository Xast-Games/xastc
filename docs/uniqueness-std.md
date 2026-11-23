# Uniqueness: conventions of the standard library

Operators on collections, `|>`, unique and shared variants of a function, partial application. Section numbers such as 2.3 refer to [uniqueness.md](uniqueness.md); numbers such as P7 refer to the problems in [uniqueness-questions.md](uniqueness-questions.md) (P1-P15, P17), [uniqueness-ecs.md](uniqueness-ecs.md) (P16) and [uniqueness-std.md](uniqueness-std.md) (P18, P19).

## P18. Operators and `<>` on collections

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

## P19. Unique and shared variants

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
