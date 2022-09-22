# Datatypes'
## Syntax
### Function Application
### Goal
The primary goal of the function-application (`func-app`) syntax is to make partial application as visually inuitive as possible. The proverbial parenthetical arguments gives the inuitive impression that the arguments are completed by the closing parenthesis, and thus that the function is not 'accepting' additional arguments. To accomplish this, Blossom takes inspiration from Haskell's syntax by using whitespace to deliminate arguments.

### Function Definition
The goal of the function-definition (`func-def`) syntax is to mirror the function-application syntax as closely as possible, while still offering helpful information to the developer such as typing.

### Constructors
#### Goal
Constructors are required to be capitalized to distinguish them from a function at a glance.

The goal of the constructor definition syntax is to be similar to function definitions. The intent is to make it clear in the declaration that they are treated just like functions *in the context of a function body*.
