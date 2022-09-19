# Datatypes

## Memory

### Multi-Constructor datatypes

Given the following:
```
data LList {
    Node
        :: value: I32
        -> next: LList
        ;
    End
        :: value: I32
        ;
}
```

The following psuedo-C-code would generate approximately the same LLVM-IR:
```C
struct LList {
    enum {
        Node,
        End
    } ctor_tag;

    union {
        struct LList_closure
    };
};

union LList_closure_table_struct {
    struct LList_Node_closure_struct* node_closure;
    struct LList_End_closure_struct* end_closure;
};

struct LList_Node_closure_struct {
    int32_t value;
    struct LList* next;
};

struct LList_End_closure_struct {
    int32_t value;
};
```

And the following psuedo-LLVM-IR:
```llvm
@
```
