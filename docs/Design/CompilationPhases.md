# Compilation Phases
## Tokenization (Lexical Analysis)
Turns the input file into a list of tokens representing variable names, numbers, keywords, etc..

## Parsing (Syntactical Analysis)
Parses the list of tokens into an tree-like data structure, called an Abstract Syntax Tree (henceforth "AST"). This AST can represent the abstract constructs such as datatype-definitions, function-definitions, type-signatures, expressions, etc..

## Type-Checking/Resolution/Inference
At this point, a Symbol Table is constructed. Blossom (plans to) uses something along the lines of the Hindley-Milner Type-System. All symbols' respective types are inferred, and validated.

### Symbol Table
A symbol table is a mapping of names to information about what the names represent. For example, given the following C definition, querying the symbol table for the name `min` would tell you that `min` is a function that takes two parameters (`a` and `b`) and returns an `int`, that it has an inline hint, and that it is in the global scope. Furthermore, querying for `a` (only inside the scope of the function) would tell you that it is constant, of type `int`, and is limited to the scope of `min`.

```C
inline int min(const int a, const int b) {
    if (a < b) {
        return a;
    } else {
        return b;
    }
}
```

## Backend
Blossom's preferred backend is [LLVM](#llvm).

### Intermediate Representation
A low-level representation (for lack of better words) of the AST is constructed. This allows for more, easier, and better optimizations.

#### LLVM
When using LLVM for backend, The AST is converted to LLVM-IR
