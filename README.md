# xastc

**xastc** is an official Xast language compiler, written in Haskell, which compiles to efficient C.

## Usage

```
xastc check [DIR]            parse and analyze the project
xastc build [DIR]            generate C and check it with a C compiler (--cc, --no-cc, --flecs, --out)
xastc dump STAGE [DIR]       print one stage: parsed, desugared, kira or c  (-m MODULE)
```

## ToDo List

- [ ] Parsing
  - [x] Modules, imports and exports
  - [x] Types: algebraic data types, enums, generics, extern types
  - [x] Functions with clauses, extern functions
  - [x] Systems and modifiers
  - [ ] Query modifiers on components: `?T`, `!T`, `$T`
  - [x] Expressions and patterns
  - [x] `*` marks on parameters and results in signatures
  - [ ] `*` on lambda parameters
  - [ ] `copy` and `boxed` keywords

- [ ] Semantic analysis
  - [x] Declarations
  - [x] Import resolution
  - [x] Type usages and name resolution
  - [x] Type inference
  - [x] Match exhaustiveness and integer bounds
  - [x] Desugaring of clauses into one `match`
  - [x] Checks of `@Copyable`, `@Pod` and `@Drop`
  - [ ] Ownership analysis (move, clone or error at each use)
    - [x] Variables, literals and constructors
    - [x] Application
    - [x] Tuples, lists and record construction
    - [x] `let`
    - [x] `if`
    - [x] `match`
    - [ ] Lambdas and captures
    - [ ] Partial application
    - [ ] Field access and record update
    - [ ] Summaries of user functions
    - [ ] Loops and tail calls
    - [ ] Driver that runs the analysis and reports its errors
  - [ ] Query modifiers in systems
    - [ ] `?T`: an optional component; it is an optional term of the query, and the body matches it as `Option T` (`Some`, `None`)
    - [ ] `!T`: a missing component; it is a term that must not match, and it is not an argument of the body
    - [ ] `$T`: a component of the parent entity
    - [ ] Warning for `Option T` written in a query: it is a component of that type, not an optional component; hint to write `?T`
    - [ ] Errors for conflicting or repeated modifiers
  - [ ] Compile-time modes for systems (Strict, Safe, Dynamic)
  - [ ] Suggest symbols to import

- [ ] IR (KIRA)
  - [x] A-normal form
  - [x] `match` compiled to decision trees
  - [x] Monomorphization
  - [x] Erasure of zero-sized values
  - [ ] Lowering of expressions
    - [x] Literals
    - [x] Constructors
    - [x] Variables and direct calls
    - [x] `match`
    - [x] `let` with variable and wildcard patterns
    - [ ] `let` with constructor and tuple patterns
    - [ ] `if`
    - [ ] Tuples with several elements
    - [ ] Lists
    - [ ] Records: construction, update and field access
    - [ ] Lambdas and partial application
  - [ ] Lowering of definitions
    - [x] Functions whose body is a `match`
    - [x] Systems with one entity and one written component
    - [ ] Other function bodies
    - [ ] Systems with `with`, several entities or several written components
    - [ ] Systems with `?T`, `!T` and `$T` in queries
  - [ ] Clone, drop and loop instructions

- [ ] Code generation
  - [ ] C
    - [x] Tagged unions and enums
    - [x] Constructors and field reads
    - [x] `match`
    - [x] Functions, with one header per module
    - [x] Intrinsics
    - [x] Systems, components and tags (Flecs)
    - [x] Check of the generated C with a C compiler
    - [ ] `Char`, `String`, list and tuple literals
    - [ ] Registering Flecs systems and running a world
    - [ ] Flecs terms for `?T`, `!T` and `$T`
    - [ ] Boxing of recursive types
    - [ ] Clones, drops and loops
    - [ ] Bundled `tcc` (Tiny C Compiler) for hot reload
  - [ ] GLSL
  - [ ] LLVM

- [ ] Command line
  - [x] `check`, `build` and `dump`
  - [x] Errors and warnings with source spans and hints
  - [ ] `build` produces a program
  - [ ] `--report-costs`
  - [ ] `run`, `new` and `test`
  - [ ] Choosing a target and a pointer size

- [ ] Packaging
  - [x] `xast.toml`
  - [x] Dependencies by path
  - [x] Finding Std
  - [x] `scripts/install.sh`
  - [ ] Use of the `[api]` table
  - [ ] `[dataflow]` table and per-package modes
  - [ ] Dependency versions, a lock file and remote dependencies
  - [ ] Linking the C output of dependencies

- [ ] Standard library
  - [x] `Prelude`, `Bool`, `Char`, `Num`, `Ops`, `Option`, `Result`, `String`, `Error`, `Function`, `Trace`
  - [ ] ECS singletons
    - [ ] `Console`
    - [ ] `FileSystem`
    - [ ] Registering them in the world for `| *R` resources
  - [ ] `Std.Alloc` intrinsics
  - [ ] `Vec`
  - [ ] `String` over bytes