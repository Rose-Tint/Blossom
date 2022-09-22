# Design


## Table of Contents

- [Identifiers](#identifiers)
- [Primitive Types](#primitive-types)
- [Declarations and Definitions](#declarations-and-definitions)
- [Pattern-Matching](#pattern-matching)
- [Functions](#functions)
- [Purity](#purity)
<!-- - [Parametric Types](#parametric-types) -->
- [User-Defined Types](#user-defined-types)


## Identifiers
- Dataypes and Constructors
    - Begin with a capital letter
    - Match the regular expression "$\[A-Z\]\[a-z-A-Z0-9_'\]^"
- Functions
    - Begin with a lower-case letter or an underscore
    - Match the regular expression "$\[a-z_\]\[a-z-A-Z0-9_'\]^"


## Primitive Types
- `Boolean`
- integral
    - `Big` (arbitrary-precision integer)
    - `U8`, `U16`, `U32`, `U64`, `I8`, `I16`, `I32`, `I64`
- floating
    - `f16`, `f32`, `f64`
- `String`
- `Unit`

## Declarations and Definitions
## Pattern-Matching
## Functions
## Purity
<!-- ## Parametric Types -->
## User-Defined Types
