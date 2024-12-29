# Minc

Minc is a work-in-progress (WIP) minimalistic compiler for a simple, C-like,
low-level Meta Language, built with the help of Menhir and LLVM.

## Scoop

Help me familiarize with the process of building a compiler from the beginning
to the end.
- Use a parser generator
- Robust and convenient way to represent and transform program representations
    - Implement a simple type inference system
    - Simple optimizations
    - Closure conversion
- Use LLVM from OCaml
- Runtime for ARM64 and X86-64

### Non-Goals and Future Work

- More primitive types: fp16, bf16, fp32, int8, int16, int32, etc.

## The Syntax and Types of Minc

```
e ::= expressions
  c                                        # constants
  op(e₁, ..., eₙ)                          # primitive operations
  if e₁ then e₂ else e₃                    # conditional branches
  for x in e₁ to e₂ do e₃ done             # for loops
  e₁; ...; eₙ                              # sequence of expressions
  let x = e₁ in e₂                         # variable definitions
  x                                        # variables
  let rec x y₁ ... yₙ = e₁ in e₂           # function definitions (mutually recursive)
  e e₁ ... eₙ                              # function applications
  (e₁, ..., eₙ)                            # tuple creations
  let (x₁, ..., xₙ) = e₁ in e₂             # read from tuples
  Array.create e₁ e₂                       # array creations
  Array.free e                             # array deallocations
  e₁.(e₂)                                  # read from arrays
  e₁.(e₂) <- e3                            # write to arrays
```
It is expressed as ML data type Syntax.t in module syntax.ml. Expressions (such
as let and let rec) defining new variables also include their types, which are
not shown above. Those types are expressed by Type.t, defined in type.ml.

### primitive operations

- Arithmetic operations: `+, -, *, /, %, - (unary)` polymorphic function by
  compiler special treatment.
- Comparison operations: `<, <=, >, >=, ==, <>`
- Logical operations: `&&, ||, !`
- Bitwise operations: `&, |, ^, ~, <<, >>`
- Array operations: `Array.create, Array.free, Array.get, Array.set`
- IO: `print` polymorphic function by compiler special treatment.

```
T ::= types
  π                                        # primitive types (bool, int64, fp64, str)
  T₁ -> ... -> Tₙ -> T                     # function types
  T₁ * ... * Tₙ                            # tuple types
  T array                                  # array types
  α                                        # type variables
```
The last "type variables" will be used in type inference.

### Acknowledgements

- [MinCaml](https://esumii.github.io/min-caml/paper.pdf)
- [Adam Jones's HM series](https://youtube.com/playlist?list=PLoyEIY-nZq_uipRkxG79uzAgfqDuHzot-&feature=shared)
- [Proofs about a folklore let-polymorphic type inference algorithm](https://doi.org/10.1145/291891.291892)
