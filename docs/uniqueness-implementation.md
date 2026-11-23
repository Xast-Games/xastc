# Uniqueness: what the compiler does

The analysis pass and the plan for building it. Section numbers such as 2.3 refer to [uniqueness.md](uniqueness.md); numbers such as P7 refer to the problems in [uniqueness-questions.md](uniqueness-questions.md) (P1-P15, P17), [uniqueness-ecs.md](uniqueness-ecs.md) (P16) and [uniqueness-std.md](uniqueness-std.md) (P18, P19). For the current state see the checklist in [../README.md](../README.md).

For each function body, after type checking and desugaring, a new pass:

1. **Liveness**, per path and per field path (P11).
2. **Second names**: find the six situations in 2.2. Choose move, clone or error with the table there.
3. **Call classification** from the parameter marks (2.3) and, for user functions, from their summaries (P13).
4. **Loops**: place the entry-edge clone for a bare parameter (P8).
5. **Report**: record every inserted clone with its reason and whether it is inside a loop.

The result is a table keyed by the source position of each second name ("move", "clone" or "error") plus the summaries. Lowering reads the table and emits the clones. The intermediate representation carries no ownership logic, and errors have source locations.

1. **Copyability** in the type checker: `@Copyable` as an explicit opt-in with the field check, `@Copyable(fn)` with its signature check, `@Pod` checked on Xast types and trusted on extern types, and `@Pod` with `@Drop` rejected. [implemented: `SemAnalyzer/Ownership.hs`; Std types annotated; `@Drop(fn)` must be `fn(*T) -> ()`] Annotate the Std types (`Int`, `Float`, `Char`, `Bool`, `Option`, `Result`, `List`, ...).
2. **Parameter modes**: parse `*` on parameters and on a handed-back result. Function types and signatures carry the marks, and results do too. Check the rules in 2.3. [implemented: parameters and results are `MarkedType`s (`Read` or `Take`) in `TyFn`, `FuncDef` and `ExternFunc`; unification compares marks; a `*` result needs a `*` parameter of its type. Resources in systems (`| *R`) belong to step 9]
3. **The analysis pass**: liveness per path and field path, second names, summaries, the table, errors with two markers. It also checks hand-back identity (2.3): every `*` result slot of a function, system or lambda must be its paired `*` parameter at its last use. The main piece.
4. **Clone insertion** in lowering for `@Copyable` types, plus a clone operation in the intermediate representation and the backend.
5. **A loop construct** in the intermediate representation and self tail calls compiled to it.
6. **`--report-costs`**: the clones section (the allocations section is in `memory.md`).
7. **Std**: `Vec` with `*` parameters, `withAt` and `modifyAt`.
8. **The native mapping** in the backend (P7).
9. **ECS resources and the system syntax** (P16): the result slots, the `@Singleton` check, the overlapping-groups error, the `@Parallel` check. Today `@Singleton` is parsed but unused, `SrcSingleton` is never produced, and the lowerer accepts only one written binding.
10. **The modes** (2.6): the `[dataflow]` table in `xast.toml`, the keywords `copy` and `boxed`, and their errors and warnings.
11. **Operators** (P18): `opConcat` with `*` operands; `concatenative` extended with the Std collections, through a Std-only marker instead of type names in the compiler.
12. **Variants** (P19): the `@SharedVariant` signature check and its hint; partial application desugared into lambdas, and lambdas lowered as closures on the stack.
13. **Later:** the lazy-clone dry run and, if it pays off, the optimization (P17); concatenation chains with one allocation (P18).

Existing gaps that examples will hit: records, `if`, string literals and lambdas are not yet lowered.
