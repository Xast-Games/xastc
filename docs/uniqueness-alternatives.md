# Uniqueness: alternatives and related work

Designs that were considered and not chosen, and how the model corresponds to Rust. Section numbers such as 2.3 refer to [uniqueness.md](uniqueness.md); numbers such as P7 refer to the problems in [uniqueness-questions.md](uniqueness-questions.md) (P1-P15, P17), [uniqueness-ecs.md](uniqueness-ecs.md) (P16) and [uniqueness-std.md](uniqueness-std.md) (P18, P19).

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

| Xast | Rust | Meaning |
|---|---|---|
| `() -> T` | `() -> T` | a new value |
| `T -> T/U` | `&T -> T/U` | reading, or cloning |
| `*T -> *T` | `&mut T -> ()` or `mut T -> T` | mutating, or threading |
| `*T -> T/U` | `T -> T/U` | consuming, and maybe returning something |
| `*T -> (U, *T)` | `&mut T -> U` or `mut T -> (U, T)` | mutating and reading, or threading and reading |

## Rust shapes with no Xast counterpart

| Rust | In Xast |
|---|---|
| `&T -> &U`, a borrow into the argument | Not expressible. Use a copy, or `withAt` with a callback (P10) |
| `&'a mut T -> &'a mut U`, reborrows | `modifyAt` with a callback |
| references stored in structs, lifetimes | Not expressible: a read parameter cannot be returned, stored or captured |
| `Rc<T>` and shared mutation | An opt-in counted type, later (P15) |
