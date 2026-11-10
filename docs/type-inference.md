# Type inference in xastc

This document describes how the xastc type checker infers and checks types, which classical algorithm it follows, and why it is built the way it is. It describes the code as it is in `src/Xast/SemAnalyzer/`; section 9 lists the files in reading order.

## 1. Overview

Xast uses Hindley–Milner type inference [1, 2] with let-polymorphism, extended in three ways:

- **Top-level functions and systems must have signatures.** Inference runs inside bodies; signatures are checked, not inferred.
- **Patterns are checked against an expected type**, which is passed down to them.
- **Constrained type variables** stand for `number` and `concatenative`, the two built-in kinds used by the operators of `Std.Ops`.

The checker follows **Algorithm J** [1]: a single substitution is shared by the whole run and extended by unification, while inference returns only types. The substitution is an immutable map inside a `State` monad rather than a set of mutable references, and it is applied to the whole syntax tree once inference is finished. Section 6 compares this with Algorithms W, J and M in detail.

## 2. Background

### 2.1 Hindley–Milner

The Hindley–Milner (HM) type system [1, 2, 5] gives every well-typed expression a **principal type scheme**: a most general type, of which every other valid type is an instance. A type scheme `∀a. fn(a) -> a` stands for all its instances (`fn(Int) -> Int`, `fn(Bool) -> Bool`, …). Two operations move between them:

- **generalization** turns the type of a `let`-bound name into a scheme, quantifying the type variables that are not fixed by the surrounding scope;
- **instantiation** replaces the quantified variables of a scheme with fresh type variables at each use.

This is what lets `let id = .\x -> x` be used at `Bool` and at `Int` in the same function. Damas and Milner [2] proved that Algorithm W is sound and complete for this system: it finds a principal type exactly when one exists.

### 2.2 Algorithms W, J and M

| | **W** [1, 2] | **J** [1] | **M** [3] |
|---|---|---|---|
| Result of inferring an expression | a substitution and a type, `(S, τ)` | a type `τ` | none: it checks against an expected type |
| Substitution | returned by every step and composed: `S₂ ∘ S₁` | one shared substitution, extended by unification as a side effect | one shared substitution |
| Direction | bottom-up | bottom-up | top-down: the expected type is passed in |
| Typical implementation | pure functions | mutable references, often union-find | mutable references |

W and J infer the same types; J avoids building and composing substitutions, which makes it the usual choice in practice. M finds the same types top-down and tends to report errors closer to their cause [3]. Modern checkers often combine inference with checking against known types, an approach known as **bidirectional typing** [4].

## 3. Representation

**Types** (`Type` in `AST.hs`):

| Constructor | Meaning |
|---|---|
| `TyCon name` | a named type, e.g. `Int`, `Option` |
| `TyApp f a` | type application, e.g. `Option Int` |
| `TyTuple [τ…]` | a tuple; `()` is the empty tuple |
| `TyFn [τ…] τ` | a function with several parameters |
| `TyGnr a` | a generic written in a signature, rigid inside the function's body |
| `TyVar n` | a type variable introduced by inference |
| `TyInt k` | `number` or `concatenative` in a signature |
| `TyInvalid` | the type of an expression that already produced an error |

**Substitution.** `SymTable.tySubst :: Map Int Type` maps type variables to types. A binding may refer to other variables (`t0 ↦ fn(t1) -> t1`), so the map is a *triangular* substitution: it is applied by following bindings until an unbound variable or a non-variable type is reached.

**Environment.** Local names live in `Env.vars`. Each `VarInfo` holds a type and a list of `quantified` type variables, which together form the name's type scheme.

**Fresh variables** come from the counter `SymTable.tyVarSupply`.

## 4. The algorithm

### 4.1 Core operations (`Unify.hs`)

| Operation | What it does |
|---|---|
| `freshTyVar` | a new unbound `TyVar n` |
| `bindVar n τ` | adds `n ↦ τ` to the substitution; the only place it is extended |
| `resolve τ` | applies the current substitution to `τ` |
| `unify l r` | resolves both sides and compares them structurally. An unbound variable is bound to the other side through `bindOrFail`, which first performs the **occurs check** (no `t0 ↦ fn(t0) -> t0`) and the constraint check of section 4.6 |
| `instantiate` | replaces the generics of a function signature with fresh variables, and each kind (`number`, `concatenative`) with one fresh constrained variable |
| `generalize τ` | the free type variables of `τ` that are neither free in the environment nor constrained |
| `zonkProgram` | applies the final substitution to every node of the typed tree |

A failed unification is **reported, not thrown**: checking continues so that more errors can be found in one run.

### 4.2 Expressions (`InferExpr.hs`)

`inferType` traverses an expression bottom-up and returns a typed copy of it. Every node records the type it had at the moment it was inferred; that type may still contain variables bound later, which zonking resolves (section 4.7).

| Expression | Rule |
|---|---|
| literal | its built-in type (`Int`, `Double`, `String`, …) |
| local name | its scheme, instantiated with fresh variables |
| top-level or extern function | its declared signature, instantiated |
| constructor | its signature (`ctorType`), with fresh variables for the type's generics |
| application `f x` | infer `f` and `x`, then `applyTypes`: if `f : fn(a, b…) -> r`, unify `a` with the type of `x`, giving `fn(b…) -> r` or `r`; if `f`'s type is still a variable, bind it to `fn(type of x) -> fresh` |
| lambda `.\x -> e` | a fresh variable per parameter; the type is `fn(parameters) -> type of e` |
| `let p = e` | infer `e`, check `p` against its type, **generalize** every name `p` binds, continue with those names in scope |
| `if c then a else b` | `c` is unified with `Bool`; `a` with `b` |
| `match e with …` | each pattern is checked against the type of `e`; the arms are unified with the first arm |
| list `[a, b…]` | the elements are unified with the first element |
| records | field types come from the constructor's signature and are unified with the field values |

Because `TyFn` has a list of parameters and `applyTypes` consumes one argument at a time, partial application is typed without currying function types.

### 4.3 Let-polymorphism

`typeLetBinds` generalizes every name a pattern binds, each separately (`generalizeVar`). In `let (g, h) = (.\x -> x, .\y -> y)`, both `g` and `h` become polymorphic. This is safe because the language is pure: each component of such a value is polymorphic on its own. OCaml treats such patterns the same way.

### 4.4 Patterns (`InferPattern.hs`)

`inferPattern` receives the **expected type** and checks the pattern against it:

- a name receives the expected type;
- a literal and a constructor pattern are unified with it;
- a tuple or a list pattern unifies it with a tuple or list of fresh element variables, then checks the parts against those.

It returns the typed pattern and the names it binds. Passing the expected type down is the top-down direction of Algorithm M [3].

### 4.5 Top-level functions and systems (`TypeCheck.hs`)

Each clause of a function is checked against the declared signature:

1. the parameter patterns are checked against the declared parameter types;
2. the body is inferred with the names bound by the patterns in scope;
3. the body's type is compared with the declared result type (`compareTypes`).

Generics in the signature (`TyGnr a`) are **rigid** inside the body: `a` unifies only with `a`. A function declared `fn f (a) -> a` cannot treat its argument as an `Int`. At each call site `instantiate` replaces the generics with fresh variables. This matches checking against an annotated polymorphic type in other HM-based languages, and it is the "checking" half of bidirectional typing [4].

Because every top-level function has a signature, a recursive or mutually recursive call uses the declared type. No grouping of definitions into strongly connected components is needed, unlike HM implementations that infer top-level types.

Systems are checked the same way, against their declared component and result types.

### 4.6 Constrained type variables

The operators in `Std.Ops` are declared over kinds: `opAdd (number, number) -> number`. `instantiate` gives each kind in a signature one fresh variable and records a constraint for it (`tyVarConstraints`). When such a variable is bound, `checkConstraint` verifies the type: `number` accepts the numeric types, `concatenative` accepts `String` and `List a`. When a constrained variable is bound to another variable, the constraint moves to that variable. Constrained variables are never generalized, since each must end up as one concrete type. After checking, `reportAmbiguousConstraints` reports those that never received a type. This is a restricted form of the constrained types used for type classes, with two fixed classes.

### 4.7 Finalization (`Pass.hs`)

When the type-checking phase is complete, `zonkProgram` applies the final substitution to every node of the typed tree. A node whose type was recorded as `t3` before `t3 ↦ Int` was learned now carries `Int`. The tree is then desugared and lowered.

## 5. Example

The following function is accepted (it is also a test in `test/SemAnalyzer.hs`):

```haskell
fn f () -> Int;
fn f = let id = .\x -> x, let a = id True, let b = id 12 in b;
```

The variable numbers below are illustrative.

| Step | Effect | Substitution afterwards |
|---|---|---|
| `.\x -> x` | `x : t0`, so the lambda has type `fn(t0) -> t0` | `{}` |
| `let id = …` | `t0` is not free in the environment: `id : ∀t0. fn(t0) -> t0` | `{}` |
| `id` in `id True` | instantiated as `fn(t1) -> t1` | `{}` |
| `id True` | `t1` is unified with `Bool`; `a : Bool` | `{t1 ↦ Bool}` |
| `id` in `id 12` | instantiated again as `fn(t2) -> t2` | unchanged |
| `id 12` | `t2` is unified with `Int`; `b : Int` | `{t1 ↦ Bool, t2 ↦ Int}` |
| result `b` | compared with the declared result type `Int` | unchanged |

Without generalization, `id True` would bind `t0 ↦ Bool`, and `id 12` would fail with "expected `Bool`, but got `Int`".

## 6. Classification: W, J or M

| Property | Like | Where |
|---|---|---|
| One shared substitution, extended by unification as a side effect | J | `bindVar` updates `tySubst`; `unify` returns nothing |
| Inference returns a type, not a substitution | J | `inferType` returns the typed expression |
| The substitution is an explicit immutable map, applied on demand | W, in representation | `resolve` corresponds to applying `S` to `τ` |
| Pure code instead of mutable references or union-find | W, in style | a `State` monad over `Map Int Type` |
| The final substitution is applied to the whole tree at the end | common in J-style checkers | `zonkProgram` |
| The expected type is passed down | M, bidirectional | `inferPattern`, function bodies against signatures |
| Mandatory signatures with rigid generics | bidirectional | `TypeCheck.hs`, `TyGnr` |

The algorithm is **J**. The resemblance to W is in how the substitution is stored, not in how it is computed: substitutions are never returned or composed. The resemblance to M comes from the parts that check against known types, patterns and top-level signatures.

## 7. Properties and design decisions

- **Principal types for local definitions.** Inside a body, the checker performs standard HM inference with let-polymorphism, so local definitions get their most general types [2]. No formal proof covers xastc's extensions (signatures, constraints, rigid generics); the argument rests on the classical results.
- **Signatures are required at top level.** This keeps inference local to each definition, makes error messages refer to code the programmer wrote, and removes the need to infer recursive groups together. It also documents every function's type.
- **Occurs check.** Binding a variable to a type that contains it is an error (`SEInfiniteType`), which rules out infinite types.
- **Error recovery.** An expression whose type cannot be determined (an unknown name, a non-function being applied) gets `TyInvalid`, which unifies with every type. One mistake is therefore reported once rather than at every place its value reaches; rustc and GHC suppress knock-on errors in a similar way. Any error still fails the phase as a whole.
- **Worst-case complexity.** HM type inference is DEXPTIME-complete in the worst case, because nested `let`s can produce types of exponential size. Such programs do not occur in practice, and inference on real programs is close to linear.

## 8. Limitations and possible improvements

- **Chains in the substitution.** Without union-find or path compression, `resolve` walks a chain `t0 ↦ t1 ↦ … ↦ Int` on every lookup. Path compression, rewriting `t0 ↦ Int` once it is resolved, is a small change if it becomes necessary.
- **Generalization scans the environment.** `generalize` resolves the type of every local name to find the variables that must stay free. OCaml avoids this scan with *levels*. It is not needed for current program sizes.
- **Clauses are checked separately**, before desugaring merges them into one `match`. This works because the signature fixes the types; it would have to change if signatures became optional.

## 9. Reading order in the code

1. `Unify.hs`: `freshTyVar`, `bindVar`, `resolve`, `unify`, `bindOrFail`, `instantiate`, `generalize`, `zonkProgram`.
2. `TypeUtils.hs`: `applyTypes`, `compareTypes`, `literalType`.
3. `InferPattern.hs`: `inferPattern`.
4. `InferExpr.hs`: `inferType`, `typeLetBinds`, `generalizeVar`.
5. `TypeCheck.hs`: `typeCheckStmt`.
6. `Pass.hs`: the order of the phases and the final zonk.

## 10. Questions and answers

**Is it Algorithm W or J?**
J. There is one substitution for the whole run, extended by unification; inference returns types, and no substitution is ever returned or composed. It is stored as an immutable map and applied explicitly, which resembles the way W is usually presented, but the control flow is J's.

**Why are generics in a signature rigid?**
A signature `fn f (a) -> a` promises that `f` works for every `a`. If `a` could be unified with `Int` inside the body, the function would only work for `Int` and the signature would be false. Rigid generics make the checker reject such bodies.

**Why is a constrained variable never generalized?**
Each `number` must become one concrete type in the generated code. A generalized `number` could be instantiated differently at each use, and there would be no single type to generate.

**Why is there no separate inference for recursive functions?**
Every top-level function has a declared signature, and a recursive call is checked against it. The fixed point that HM needs for recursive definitions without annotations is never required.

**What guarantees that the inferred types are correct?**
For the core language, the soundness and completeness of HM inference [2]. The extensions are conservative: signatures only restrict what is accepted, and constraints only reject types that are not in their kind.

## References

1. Milner, R. *A theory of type polymorphism in programming.* Journal of Computer and System Sciences, 17(3), 348–375, 1978.
2. Damas, L., Milner, R. *Principal type-schemes for functional programs.* Proceedings of the 9th ACM Symposium on Principles of Programming Languages (POPL), 207–212, 1982.
3. Lee, O., Yi, K. *Proofs about a folklore let-polymorphic type inference algorithm.* ACM Transactions on Programming Languages and Systems, 20(4), 707–723, 1998.
4. Dunfield, J., Krishnaswami, N. *Bidirectional typing.* ACM Computing Surveys, 54(5), 1–38, 2021.
5. Pierce, B. C. *Types and Programming Languages.* MIT Press, 2002. Chapter 22, "Type Reconstruction".
