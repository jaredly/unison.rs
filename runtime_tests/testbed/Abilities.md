
Ok, new understanding:

If you have an abilities stack

[handler A]
[handler B]
[handler C]

and you check handler A, but it re-throws
then you hang on to handler A for when the continuation happens, because you want to put it back on the stack.

So, changes:
- rethrow-effect needs to have the "current handler" as a value.
  > which means my codegen needs to update I believe.

Currently
handle-ability:
    (inner: () => T, handler: (result: Effect | Pure<T>) => R) => R

But I need to to be:
    (inner: () => T, handler: (result: Effect | Pure<T>, Handler) => R) => R
  
The cool thing is I go through the handler pipeline each time
so I can do a little y combinator action.