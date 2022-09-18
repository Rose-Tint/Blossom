# Memory

## Table of Contents





## Values

### How a Value Looks in Memory

In Blossom, values allocated on the heap are represented in memory in
the following arrangement:
```
+---------+-----------+
| header | fields... |
+---------+-----------+
```
where `header` is a machine word representing the constructor, and
`fields...` represent pointers to the fields as words.

lets take the following file for example:

```
import Numerics;

data IntTree {
    Node
        :: value: Int
        -> left: IntTree
        -> right: IntTree
        ;
    Empty;
}

newIntTree :: value: I32 -> IntTree
    => Node value Empty Empty;
```

The C equivalent to `IntTree` would be
```C
typedef union IntTree {
    enum Tag {
        Node,
        Empty
    } tag;

    struct Node {
        int value;
        struct Node* left;
        struct Node* right;
    } node;
} IntTree;
```


## GHC LLVM-IR Study Notes :)

Given:
```Haskell
data AB
    = A
    | B

ab :: AB -> AB
ab A = A
ab B = B
```

GHC generates the following LLVM IR ('paraphrased'):
`B`:
    closure struct: `type <{i64}>`
    closure $def: `global %B_closure_struct<{i64 ptrtoint (i8* @B_con_info to i64)}>`
    closure: `alias i8, bitcast (%B_closure_struct* @B_closure$def to i8*)`
`A`:
    closure struct: `type <{i64}>`
    closure $def: `global %A_closure_struct<{i64 ptrtoint (i8* @A_con_info to i64)}>`
    closure: `alias i8, bitcast (%A_closure_struct* @A_closure$def to i8*)`
`ab`:
    closure struct: `type <{i64}>`
    closure $def: `global %ab_closure_struct<{i64 ptrtoint (void (...)* @ab_info$def to i64)}>`
    closure: `alias i8, bitcast (%ab_closure_struct* @ab_closure$def to i8*)`
    info: `alias i8, bitcast (void (...)* @ab_info$def to i8*)`
    info $def: WAY too long.
`AB`:
    closure table struct: `type <{i64, i64}>`
    closure table $def: `global %AB_closure_tbl_struct<{i64 add (i64 ptrtoint (%A_closure_struct* @A_closure$def to i64),i64 1), i64 add (i64 ptrtoint (%B_closure_struct* @B_closure$def to i64),i64 2)}>`
    closure table: `alias i8, bitcast (%AB_closure_tbl_struct* @AB_closure_tbl$def to i8*)`
`A_con`:
    info: `alias i8, bitcast (void (...)* @Test_A_con_info$def to i8*)`
    info $def: WAY too long.
`B_con`:
    info: `alias i8, bitcast (void (...)* @Test_B_con_info$def to i8*)`
    info $def: WAY too long.
