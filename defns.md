# Defining Tail Calls and Tail Recursion

## Tail Call

A function call is in a tail call or tail position if no further evaluation is done on the result of that function. Formally we can describe this as:

Suppose `f : t1 -> t` and `x : t1`, if `f x ==> g y`, where `g : t2 -> t` and `y : t2`, then `g` is a tail call.

Consider the following example:
  ```sml
  fun factCPS 0 k = k 1
    | factCPS n k = factCPS (n - 1) (fn res => k (n * res))
  ```
- In clause 1, `k` would be considered to be a tail call.
- In clause 2, `factCPS` and `k` would both be considered tail calls.
  - Importantly, `k` would be considered a tail call with respect to the lambda expression.

## Tail Recursion

A function is considered tail recursive if:
  - the function is recursive
  - all calls made to recursive functions (including itself) are in a tail position
  - all called recursive functions (besides itself) are tail recursive themselves

Importantly the last part of the defintion excludes the possibility of just eta-expanding a non-tail recursive function to make it tail recursive.
