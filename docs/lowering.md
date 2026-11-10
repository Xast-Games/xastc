# Lowering in xastc

This document describes how xastc turns the typed, expression-based syntax tree into **KIRA** (_Khast Intermediate RepresentAtion_), a flat imperative representation that the backend translates directly. It covers how expressions are flattened into instructions, how `match` is compiled into decision trees, and the two passes that finish the IR: monomorphization and erasure of zero-sized values. It describes the code in `src/Xast/Lowerer/`; section 11 lists the files in reading order.

## 1. Overview

The stages between type checking and code generation are:

| Stage | Where | Input → output |
|---|---|---|
| Desugaring | `SemAnalyzer/Desugar.hs` | the clauses of a function or system → one `match` on the tuple of its parameters |
| Lowering | `Lowerer/Expr.hs`, `Match.hs`, `Definition.hs` | expressions → instructions on named values; `match` → decision tree → nested `KirMatch` |
| Monomorphization | `Lowerer/Mono.hs`, `Pass.hs` | generic types and functions → one copy per set of type arguments, done during lowering |
| Zero-size erasure | `Lowerer/EraseZero.hs` | removes values of types that carry no data; such ECS components become tags |

Lowering does two things at once:

- **Flattening.** Every intermediate result gets a name, every operand is a constant or a name, and the order of evaluation is written out. This is the idea of A-normal form (ANF) [3].
- **Match compilation.** A `match` with nested patterns is compiled into a decision tree [1]. In the tree, each node tests one value once, and the tree is then emitted as nested `KirMatch` instructions.

## 2. Background

### 2.1 A-normal form

A program is in **A-normal form** [3] when every argument of an operation is *atomic*, a constant or a variable, and every non-atomic computation is bound to a name. For example, `f (g x) (h y)` becomes:

```
let a = g x in
let b = h y in
f a b
```

ANF makes the evaluation order explicit and gives every intermediate value a name, so each step maps to one instruction of an imperative target. Compilers often reach it through two steps, "remove complex operands" and "explicate control" [4]: the first names the intermediate values, and the second turns nested conditionals into blocks and jumps. xastc does both in one traversal.

### 2.2 Compiling pattern matching

Arms of a `match` are tried in order, and a pattern can be nested: `Some Triangle`. There are two classical ways to compile this:

| | **Backtracking automata** [2] | **Decision trees** [1] |
|---|---|---|
| Idea | try the arms in order; on failure, jump back and try the next arm | each node tests one value and branches on its constructor |
| Testing | a value can be tested more than once | every value is tested at most once on any path |
| Code size | linear in the size of the patterns | can grow exponentially; small in practice with a good column heuristic |
| Control flow | needs jumps to shared failure points | nested branches only |

Both start from a **pattern matrix**: one row per arm and one column per value being matched. xastc builds decision trees following Maranget [1]. They lower to nested branches with no jumps, which suits a structured target, and no value is examined twice.

## 3. KIRA

KIRA (`Lowerer/Types.hs`) is an imperative IR:

- **Values** (`KirValue`) are atoms: a constant (`KirConst`), a named temporary (`KirVar`), a match result slot (`KirLocalRef`), or a system binding (`KirBindingRef`).
- **Instructions** (`KirInstruct`) take values and name their result:

| Instruction | Meaning |
|---|---|
| `KirCall` | `t = f(v₁, …, vₙ)`: calls a function or an intrinsic |
| `KirConstruct` | `t = C(v₁, …, vₙ)`: builds a value with constructor `C` |
| `KirField` | `t = v.C.field`: reads a field of `v`, known to be built by `C` |
| `KirMatch` | branches on the tag of a value; the value of the branch taken is written into a destination |
| `KirStore` | `l := v`: writes a value into a destination |
| `KirAssign` | writes a value into a system binding (a component) |

- A **branch** (`KirBranch`) is a list of instructions followed by its value.
- A **destination** (`KirDest`) is either a local result slot `l` (`DestLocal`) or a component that a system writes (`DestBinding`).
- A **function** is a list of instructions followed by `return`.

Temporaries `t` are assigned exactly once. A result slot `l` is declared before its `match` and assigned once on each path through it.

The listings below use a compact notation: names are shortened, and `match v into l` lists arms whose last line is the value stored into `l`. `xastc dump kira` prints the same structure as Haskell records.

## 4. Before lowering: desugaring

A function may have several clauses, each with its own patterns:

```
fn pick (Option Shape, Option Shape) -> Int;
fn pick (Some Triangle) _ = 1;
fn pick _ None = 2;
fn pick _ _ = 3;
```

`desugarFn` merges them into one clause whose body is a `match` on the tuple of fresh parameters:

```
fn pick p0 p1 = match (p0, p1) with
    (Some Triangle, _) -> 1,
    (_, None)          -> 2,
    (_, _)             -> 3;
```

`desugarSys` does the same for systems, with one parameter per component of every entity. After desugaring, every function and system body is a `match` on its parameters. The lowerer relies on this: `lowerFunction` and `lowerSystem` only accept that shape.

## 5. Expressions into instructions

### 5.1 The core function

```haskell
lowerExpr :: M.Map LocalId KirValue -> Expr Desugared -> Lowerer ([KirInstruct], KirValue)
```

`lowerExpr` takes an environment from local names to KIRA values and returns the instructions that compute an expression together with the atom that holds its result. Lowering a compound expression concatenates the instructions of its parts, in evaluation order:

| Expression | Instructions | Result |
|---|---|---|
| literal `12` | none | `KirConst 12` |
| local variable `x` | none | the value of `x` in the environment |
| constructor `None` | `t = None()` | `t` |
| application `f a b` | instructions of `a`, then of `b`, then `t = f(va, vb)` | `t` |
| constructor application `Some a` | instructions of `a`, then `t = Some(va)` | `t` |
| `let x = e in body` | instructions of `e`, then of `body`, with `x ↦ ve` | the result of `body` |
| `match e with …` | see section 6, writing into a fresh slot `l` | `l` |

`applicationSpine` turns the curried `f a b`, `ExpApp (ExpApp f a) b`, into a callee and a list of arguments, so a fully applied call becomes a single `KirCall`. Operators are calls too: `x + x` calls the intrinsic `opAdd` at the type of its operands (section 7).

### 5.2 `let` emits nothing

`let x = e` creates no instruction of its own. `e` is lowered to some atom, and the environment maps `x` to that atom, so every use of `x` refers to the atom directly. `let y = x` costs nothing, and `x` is never copied into a new variable.

### 5.3 `match` in the middle of an expression

KIRA has no conditional expression, only the `KirMatch` instruction. When a `match` occurs inside a larger expression, the lowerer creates a fresh slot `l`, lowers the match so that every arm writes into `l`, and returns `KirLocalRef l` as the atom for the rest of the expression. This is destination passing: each branch knows where its result goes.

A system uses the same mechanism with a different destination. Its body writes into the component binding that the system declares as written (`DestBinding`), so the result goes straight into the component.

### 5.4 Example

```
fn scale (Int, Int) -> Int;
fn scale x k = let doubled = x + x in doubled * k + 1;

fn clampSign (Int) -> Int;
fn clampSign x = 1 + match x > 0 with
    True  -> x,
    False -> 0;
```

```
scale(p0, p1):                 clampSign(p0):
  t5 = opAdd(p0, p0)             t2 = opGt(p0, 0)
  t6 = opMul(t5, p1)             match t2 into l1
  t7 = opAdd(t6, 1)                True  -> p0
  l4 := t7                         False -> 0
  return l4                      t3 = opAdd(1, l1)
                                 l0 := t3
                                 return l0
```

Several things show here:
- `x` and `k` are bound by the desugared `match` to the parameters themselves, so they are not copied.
- `doubled` has no instruction; it is simply `t5`.
- The inner `match` writes into `l1`, which then appears as an ordinary operand.

## 6. Match compilation

### 6.1 Pattern matrix and occurrences

`lowerMatchTo` turns a `match` into a matrix (`DecisionTree.hs`):

- **Columns.** A `match` on a tuple, which is what desugaring produces, gets one column per element; any other `match` has a single column.
- **Rows.** There is one row (`Row`) per arm: its patterns, the arm's index, and the variables bound so far.
- **Patterns** (`Pat`) are reduced to three forms: `PWildCard`, `PVar` and `PCon`. A literal is a constructor with no fields.

Each column refers to an **occurrence** (`Occ`): a path from one of the matched values to a part of it. `Occ 0 []` is the first value itself, and `Occ 0 [0]` is field 0 of whatever constructor it was built with. Occurrences say *what* is tested, independently of the column's current position.

Each constructor (`Con`) carries its tag, its arity, and its **span**, the number of constructors of its type. The span tells when a set of constructors is complete. Literals have an unbounded span, because no finite set of literals covers a type.

### 6.2 The algorithm

`compilePatterns occurrences matrix` follows Maranget's scheme [1]:

1. **No rows:** `DTFail`. No arm matches.
2. **The first row has only wildcards and variables:** `DTLeaf`. The first arm matches, and its variables are bound to their occurrences.
3. **Otherwise:**
   - Choose a column whose pattern in the first row is a constructor.
   - If it isn't column 0, swap it there, together with its occurrence (`DTSwap`).
   - Let Σ be the set of constructors at the head of that column. Build `DTSwitch` on its occurrence `o`, with one case per constructor `c` in Σ. Each case compiles the **specialized matrix** `S(c, P)`:

     | Row starts with | In `S(c, P)` |
     |---|---|
     | `c(q₁, …, qₐ)` | `q₁ … qₐ` followed by the rest of the row |
     | a wildcard or variable | `a` wildcards followed by the rest of the row (a variable is bound to `o`) |
     | another constructor | the row is removed |

     The new columns refer to the occurrences `o.0 … o.(a-1)`, the fields of `c`.
   - If Σ isn't complete (fewer constructors than the span), add a default case. It compiles the **default matrix** `D(P)`: the rows that start with a wildcard or variable, without their first column.

Rows keep their order in `S` and `D`, so the first row of a submatrix is always the earliest arm that can still match. This makes the top-to-bottom semantics of `match` hold.

### 6.3 Choosing a column

xastc chooses the leftmost column in which the first row has a constructor. This is the simplest heuristic studied in [1], called *first row*. The choice affects only the size of the tree, never the result. Maranget evaluates heuristics that produce smaller trees, such as needed columns and small branching factor; they aren't implemented yet.

### 6.4 From the tree to KIRA

`lowerTreeInto` walks the tree with the matched values for the root occurrences, writing the result into a destination:

- **`DTSwitch`** becomes a `KirMatch` on the value of its occurrence, with one arm per case and the default as the fallback.
- **`DTLeaf`** binds the arm's variables to the values of their occurrences, lowers the arm's body with `lowerExpr`, and stores the result.
- **`DTFail`** produces nothing. An arm that leads only to `DTFail` is dropped.
- **`DTSwap`** produces nothing, because the occurrences were swapped together with the columns.

There are four further details:

- **Lazy field reads.** The value of an occurrence such as `o.0` is read with `KirField` only when a switch tests it or a leaf binds it (`resolveOcc`). It is then cached for the rest of that path, so a field is read at most once on each path, and never if no one needs it.
- **Single-constructor types.** A switch on a type with only one constructor, such as a record, has nothing to test. No `KirMatch` is emitted; only the fields are made available.
- **The `exhaustive` flag.** It is set when a switch covers every constructor and has no default. The backend uses it to mark the missing default as unreachable.
- **Nested switches.** A switch inside a case is lowered into its own fresh slot, and the outer case returns that slot as its value.

### 6.5 Example

For `pick` from section 4, the matrix is:

```
           p0              p1
arm 0:     Some Triangle   _       -> 1
arm 1:     _               None    -> 2
arm 2:     _               _       -> 3
```

The tree is built as follows:

1. Row 0 has a constructor in column 0, so the root switches on `p0`. Σ is `{Some}`, but `Option` has two constructors, so a default is added.
2. **Case `Some`:** `S(Some, P)` has the columns `p0.0` and `p1`:
   - Row 0 starts with `Triangle`, so the next switch is on `p0.0`. Σ is `{Triangle}`, out of three shapes, so a default is added.
   - **Case `Triangle`:** the first row is now all wildcards, giving leaf 1.
   - **Default of `p0.0`:** rows 1 and 2 remain, giving a switch on `p1`: `None` leads to 2, and the default to 3.
3. **Default of `p0`:** `D(P)` keeps rows 1 and 2, which gives the same switch on `p1` again.

```
match p0 into l0
  Some ->
    t2 = field p0.Some._0
    match t2 into l1
      Triangle -> 1
      default  ->
        match p1 into l3
          None    -> 2
          default -> 3
        l3
    l1
  default ->
    match p1 into l4
      None    -> 2
      default -> 3
    l4
return l0
```

`p0` and its field are each tested once on every path. The price is that the switch on `p1` appears twice, and so do the bodies of arms 1 and 2. This is the code-size side of decision trees from section 2.2.

## 7. Monomorphization

KIRA has no generics: every type and every function in it has concrete types. Monomorphization happens on demand during lowering, with worklists in `LowerState`:

- **Types.** `monoType` replaces every use of a user type or tuple with its instance, for example `Option Shape` with `Std_Option_Option_Main_Shape`. The first request of an instance puts it on `typeQueue`.
- **Functions.** A function whose signature has a generic, `number` or `concatenative` is not lowered where it is declared. At each call, `requestFunction` matches the declared signature against the type of the call (`bindTypeParams`). The first time a combination is seen, it puts a request on `funcQueue`, for example `isSome` at `Option Int`.
- **Intrinsics.** `requestIntrinsic` records one instance per combination of types, for example `opAdd` at `Int`. It records only the signature, because the backend writes the body.

After all modules are lowered:
- `drainFunctions` lowers queued functions until the queue is empty. Each instance may request new instances.
- `drainTypes` builds the queued types.

Functions go first because function instances request types. Every instance is placed in the module that declares the function, not in the module that requested it.

Names are made by `mangle`: the module-qualified name followed by the type arguments, for example `Std_Option_isSome_Std_Num_Int`.

## 8. Zero-size erasure

A type with one constructor whose fields are all zero-sized, such as `type Player = Player`, carries no data. `EraseZero.hs` runs after lowering and removes such values:

- **Finding them.** `zstSet` computes the zero-sized types as a fixed point. A single-constructor type becomes zero-sized once all its fields are, and a counter per type tracks how many fields are still unknown.
- **Signatures.** Zero-sized parameters are dropped from every function, and a zero-sized result becomes unit. All signatures are computed before any body is rewritten, because every call must know which of its callee's parameters were dropped.
- **Bodies.** Constructing, storing or reading a zero-sized value disappears, and so does every later use of the result. A `match` on an erased value has only one possible branch, which is inlined.
- **ECS.** Zero-sized types marked `@Tag` are returned separately, because in the ECS they become tags, not components with storage.

## 9. Properties and design decisions

- **Every value is tested at most once on any path.** This follows from the decision tree, since each occurrence leaves the matrix once it is switched on. Field reads are shared along a path for the same reason.
- **Arm order is preserved.** Specialization and default keep row order, and a leaf is always the first row that still matches. A `match` therefore behaves as if its arms were tried top to bottom.
- **Evaluation order is explicit.** Arguments are evaluated left to right, then the call; `let` bindings are evaluated in order, before the body. Since the instructions are a list, this order is visible in KIRA and carried over to the generated code.
- **Destinations instead of φ-nodes.** A `match` result is a slot assigned in each branch, not a value merged by φ as in SSA form. This maps directly to structured targets, and it lets a system write its result straight into a component.
- **Exhaustiveness is checked before lowering.** `Exhaustive.hs` in the semantic analyzer uses the usefulness algorithm of [5] on the same kind of pattern matrix: a match is exhaustive when a row of wildcards added after its arms would match nothing new. Unlike the decision tree, it reports the missing values, e.g. `Some Square` or `(True, True)`. So for an accepted program, `DTFail` is unreachable, except for partial matches on a `@NonExhaustive` type.
- **One traversal.** Flattening, match compilation and monomorphization requests happen in one pass over each body. Desugaring and erasure are separate because they need the whole program: all clauses of a function, and all signatures, respectively.

## 10. Limitations and possible improvements

- **Duplicated code.** Subtrees, and arm bodies with them, can appear several times (section 6.5). The usual fix is to lower each arm once as a shared block that receives its bound variables, and have leaves jump to it.
- **Simple column choice.** Only the *first row* heuristic is used (section 6.3).
- **Unsupported forms.** The lowerer stops with a `todo__` on:
  - lambdas and partial application;
  - tuples as values, outside a `match` scrutinee;
  - `let` with a constructor or tuple pattern;
  - systems with `with`, with several entities, or with other than one written component.
- **No dead code removal.** An unused `let` is still evaluated.

## 11. Reading order in the code

1. `SemAnalyzer/Desugar.hs`: `desugarFn`, `desugarSys`.
2. `Lowerer/Types.hs`: KIRA, and `LowerState`.
3. `Lowerer/Expr.hs`: `lowerExpr`, `applicationSpine`, `lowerLetBindings`, `lowerCall`, `lowerConstruct`.
4. `Lowerer/DecisionTree.hs`: `compilePatterns`, `specializeS`, `defaultD`.
5. `Lowerer/Match.hs`: `lowerMatchTo`, `toPat`, `lowerTreeInto`, `lowerBranch`, `resolveOcc`.
6. `Lowerer/Definition.hs`: `lowerFunction`, `lowerSystem`.
7. `Lowerer/Mono.hs`: `monoType`, `requestFunction`, `requestIntrinsic`, `mangle`.
8. `Lowerer/Pass.hs`: `lowerPrograms`, `drainFunctions`.
9. `Lowerer/EraseZero.hs`: `zstSet`, `eraseFunction`, `eraseMatch`.

## 12. Questions and answers

**Why decision trees and not backtracking automata?**
A decision tree never tests a value twice, and it lowers to nested branches without jumps, which keeps both the lowering and the generated code simple. The cost is code size. Matches in game logic are usually small, and a better column heuristic or shared arm bodies (section 10) keep the size down.

**Is KIRA in A-normal form?**
In the sense that matters: every operand is an atom, every intermediate result has a name, and the evaluation order is explicit. It differs from textbook ANF in how branches return: ANF keeps `if` as an expression bound by `let`, while KIRA's `KirMatch` is an instruction that writes into a destination. That is the "explicate control" step of [4], done in the same traversal.

**Why does `let` not produce an instruction?**
The bound expression is already lowered to an atom, a name or a constant, so the variable can refer to that atom directly. An instruction would only copy it.

**What happens when no arm matches?**
The tree reaches `DTFail`, which produces no code. For an exhaustive `match` this point is unreachable, and a switch that covers every constructor is marked `exhaustive`, so the backend can say so. The checker rejects every other `match` (section 9), except a partial `match` on a `@NonExhaustive` type, where the result of an unmatched value is undefined.

**Why is monomorphization done during lowering?**
Which instances exist is only known once calls are seen with their concrete types. Lowering is where those types are needed anyway, so instances are requested there and lowered from a worklist. Each instance is lowered once, and the instances it needs in turn are found the same way.

## References

1. Maranget, L. *Compiling pattern matching to good decision trees.* Proceedings of the 2008 ACM SIGPLAN Workshop on ML, 35–46, 2008.
2. Le Fessant, F., Maranget, L. *Optimizing pattern matching.* Proceedings of the 6th ACM SIGPLAN International Conference on Functional Programming (ICFP), 26–37, 2001.
3. Flanagan, C., Sabry, A., Duba, B. F., Felleisen, M. *The essence of compiling with continuations.* Proceedings of the ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI), 237–247, 1993.
4. Siek, J. G. *Essentials of Compilation: An Incremental Approach.* MIT Press, 2023.
5. Maranget, L. *Warnings for pattern matching.* Journal of Functional Programming, 17(3), 387–421, 2007.
