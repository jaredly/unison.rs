
# Tests to write, about FFI

- a parameterized ability w/ two different type parameters, to make sure they're concretized in the right order.


# How do we deal with type variables in effects?
That is, how do we know what types to check in the input & output of `lambdas` etc.?

Because the `setTimeout` example, where I pass a lambda to ...
what if I make it explicit?

SOLUTION! You always have to make explicit the effects of any lambdas, and include that in the type of the effect.
Seems to have worked so far :D

# Todo:

- [ ] make a pack-term that takes a common name
- [ ] wrap/unwrap tuples correctly
- [ ] typecheck handler's return value, and if it's null/undefined, then raise an error
- [ ] make a little TODOmvc example? could be fun


# Bugs

- If a handler's return value fails to typecheck, it seems we're silently swolloeing the error