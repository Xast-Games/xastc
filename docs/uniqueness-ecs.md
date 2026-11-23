# Uniqueness: ECS resources and systems

How resources such as the console, and systems, fit the ownership model. Section numbers such as 2.3 refer to [uniqueness.md](uniqueness.md); numbers such as P7 refer to the problems in [uniqueness-questions.md](uniqueness-questions.md) (P1-P15, P17), [uniqueness-ecs.md](uniqueness-ecs.md) (P16) and [uniqueness-std.md](uniqueness-std.md) (P18, P19).

## P16. ECS resources and the old effect list

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
- **A written component is an implicit `*T` parameter**, and its slot in the result is the hand-back (2.3). If the slot is one of the forms of 2.3 (the component itself, a record update of it, a refill), the system is compiled in place, as a write through the pointer to the component. Any other value in the slot, such as a fresh `Pos 0 0`, is assigned to the slot. That is not an error, as it would be for `-> *T` in a function: the storage belongs to the ECS and nobody holds another name for it, so replacing the value creates no second name. The mode changes the cost, never the meaning.
- **The result** is a tuple: one slot per query group, in order, then one slot per `*R`. A slot holds the components its group writes (a tuple if several), or `()` for none. Each slot may have a `--` comment. A tuple of one slot is the slot itself, so a system with one group and no `*R` keeps the current form: `system Move #(Pos, Velocity) -> Pos;`.
- **Resources** after `|` keep `*`: `*R` is a write and a bare `R` a read. A resource type must be `@Singleton`; otherwise it is a compile error.
- A system that receives `*R` **must hand it back**. If the body consumes it (`finish builder`), that step is fine, but the result then asks for a value that is gone: the use-after-move error of P6, at the result.
- A system may create its own local non-copyable values and consume them (a `*File` opened and closed within one run).
- A system may not swap a received resource for a fresh one of the same type: the slot of `*R` must hand back that same `R` (2.3).
- The signature tells read from write: two systems that only read a resource do not conflict, so they can run in parallel; any `*R` conflicts with every other user of `R`.
- **Overlapping groups.** If the components of one query group are all contained in another group, every entity that matches the larger group also matches the smaller one, so two groups can denote the same entity. That is a compile error: `#(A), #(A)` or `#(A, B), #(A)`. Tell the groups apart with tags: `#(Tag1, A), #(Tag2, A)`. An entity that carries both tags still matches both groups; that case cannot be seen at compile time and is the programmer's responsibility.
- **Writes in a multi-group system.** A system with several query groups runs over their combinations, so a written component of one group can be written once per entity of another group (each `Enemy`'s `Transform` once per `Player`). That is the programmer's responsibility, but `@Parallel` on a system with several groups that writes any of them is an error, since such writes could race.
