# Functions


## Closures

### How Closures are Implemented

Each function generates a closure structure.

Given the following:
```
func clamp :: min: I32 -> max: I32 -> n: I32 -> I32
    | le n min => min
    | ge n max => max
    | _ => n
    ;
```

The following psuedo-C-code is approximately equivalent:
```C
struct clamp_closure_struct {
    int32_t* min;
    int32_t* max;
    int32_t* n;
    int32_t (*func)(struct clamp_closure_struct*);
};

int32_t clamp_caller(struct clamp_closure_struct* closure) {
    return closure->func(closure);
}
```