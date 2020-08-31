

It would be nice to have runtime testcases.
you know.

Basics: 
- test cases for each "native" fn.
- test cases for scoping, shadowing
- recursive functions (like I've been making)
- handlers, nested handlers, fall-throughs, etc.




The issue with the effects:
If we have a stack

a | b | c | d | e | f | g
and we request at g, with handlers at d and b
at the request, we need to be saving the whole stack.
and essentially, processing the request means going up the stack, finding a handler (clearing it),
and continuing. If it gets re-thrown, we go back to thr drawing board (I think, need to verify.)

And if it gets "continued", we take the subset of frames that were the difference between the top and the handler, and put that on top of the current stack.

So essentually, the `Request` instance needs a pointer to the "currently handling handle" thing.
So that once the continuation is called, we can drop everything below it.
But if the fallthrough happens, then we need to jump back to that stack,
forgetting everything that happened up till then.

```
ability Both a where
	both : a ->{Both a} a

only5 = k -> handle !k with cases
    { a } -> a
    { Both.both n -> k } | n == 5 -> only5 '(k 55)

allthem = k -> handle !k with cases
    { a } -> a
    { Both.both n -> k } -> allthem '(k 100)

checkit = allthem '(only5 '(let
	x = Both.both 4
	y = Both.both 5
	z = Both.both 3
	a = Both.both 5
	[x, y, z, a]))

> checkit
[100,55,100,55] is what it should be.

```



Things I need
- Store.local
- #1qporgrhu3 
- #7q3ad898814q6gcetnartvvmko9hf76tjfn6bjd5h1ctk2hk8383527vhru2kisltc4dfdej1
- Store.local.test

# Steps

- [x] builtin fn app
- [x] let bindings (& vbl resolution)
- [x] if blocks
- [x] let inside if block
- [x] sequence construction
- [x] pattern matching
  - [x] with guards
- [x] load other terms
- [x] call internal functions
- [x] call external functions
- [x] Rc for less cloning
- [ ] 

## Function equality folks
how to do it

1. move the free variables declaration to the `] Fn(0)`? hm no that wont do it.
-
1. make a hash of each fn, and then override equality to check the hash?
   like, that might do it just fine.
1. Anything that involves actually deduplicating Fn declarations will run into variable name issues I think.
  2. but I could instead genericize free variables ... like have them just be indices, listed in order of access ... yeah that could be good. But maybe more sweeping of a change.
  but it's possible I'll have to do it?


So in javascript land, this means that we would never pass around "a function" as such, it would always just be a map of captured variables and an integer indicating where the function can be found? although I guess we could pass around the function as long as we dedup.

## Functional PartialOrd .. like .. umm
that seems quite difficult to replicate.
Unless unison could tell me the hashes of things or something, in what it outputs.


# WASM! What's it look like?

My basic idea has been:
- have a binary that you can call to "package" up a top-level unison term for running (this would create an IREnv probably? I'll need to trim down TranslationEnv so it doesn't depend on env::Env or TypeDecl probably)
  - this is probably a `TrimmedDownTranslationEnv` that has been bincoded.
- have a wasm "interpreter" that will take a package, `my-file.ubwasm`, parse it, and run it.

Once I have that done, I can think about implementing IO? Probably.

Also, I could implement my own custom IO-like dealios?
Yeah, for js ... externals ...

What js libraries do people want to use?
React, of course :P

I mean, it would be cool to get an elm-style-something running...

Ok, but more straightforward:
- document.createElement
- element.setAttribute
- element.set ... listener ...

# JASM! What do would it look like to implement the runtime that I've got in javascript?

I could encode the `TrimmedDownTranslationEnv` to JSON instead, and I think `ir_runtime` and `pattern.rs` are the only things I would need?

# Unison.js

This would be ~actually compiling to javascript. It would require transforming out the wazoo.

But, it might not be too complex? certainly would be interesting to explore.

Basics:
- Call a function! It might abort halfway through.


thing a b c =
  let x = a b c
      y = m x
      go (x + y)

->

```js
function thing(a, b, c, resume) {
  let x_0 = a(b)
  let x_1 = x_0(c)
  let y = m(x)
  let _1 = x + y
  return go(_1)
}

// hmm yeah this is looking somewhat like "machine code". hmmm.
// I feel like I could turn the IR into javascript without too many tears...
function thing(a, b, c, resume) {
  let x_0, x_1, y, _1;
  if (resume && resume[0].pos < 1) {
    x_0 = resume[0].locals.x_0
  } else {
    let res = a(b, resume ? resume.slice(1) : null)
    if (isResume(res)) {
      return res.concat({locals: {a, b, c}, pos: 1});
    }
    x_0 = res
  }
  if (resume && resume[0].pos < 2) {
    x_1 = resume[0].locals.x_1
  } else {
    let res = x_0(c, resume ? resume.slice(1) : null)
    if (isResume(res)) {
      return res.concat({locals: {a, b, c, x_0}, pos: 2});
    }
    x_1 = res
  }
  let y = m(x)
  let _1 = x + y
  return go(_1)
}
```




# Optimization

- detect what bindings aren't used, and change PopAndName to just Pop


- patterns get their own instruction.
  - they put things on the stack twice
- all lambdas get a unique ID, and the value
  that is put on the stack is `FnWithScope(fnid, bindings)`
- for Cycle()s, it's maybe `CycleWithScope(fnid, bindings, selfname)` -- do I need to include the names of the other ones? maybe
- Stacks have a return pointer, telling you where to go when its done

```rs











Ok, so general plan:

have a "to_ir()" recursive method.
It will push IR commands onto a Vec<>, that will represent an individual function.

"Return pointers" will point to A function (either top-level of on-heap) and an index within the function.



Sounds reasonable?
When we come across a function that we need to instantiate ...
... is there a way to no re-instantiate all the time? that would be great.



```

# With Rc, but some extra clones happening (59mb)
```
uruson on  rc ➜ time -l ./target/release/uruson ~/.unison/v1/terms/\#t9p6gt572cnfofjuo7862sjqramtp4pv8gk5v1dk892r155t3eibfs412o8me6enmlc0appl0l9e7t88ndbbqj171tfbm9eq9kvf8og/
Hello, world!
Running "/Users/jared/.unison/v1/terms" - t9p6gt572cnfofjuo7862sjqramtp4pv8gk5v1dk892r155t3eibfs412o8me6enmlc0appl0l9e7t88ndbbqj171tfbm9eq9kvf8og
Time: 855ms (855826727ns)
-> Sequence([PartialConstructor(#vmc06s4f23, 1, [Text(" : Passed 100 tests.")])])
        2.25 real         2.14 user         0.06 sys
  59596800  maximum resident set size
         0  average shared memory size
         0  average unshared data size
         0  average unshared stack size
     26773  page reclaims
         0  page faults
         0  swaps
         0  block input operations
         0  block output operations
         0  messages sent
         0  messages received
         0  signals received
       156  voluntary context switches
       605  involuntary context switches
```

# With my fake GC (273mb)

```
uruson on  master took 12s ➜ time -l ./target/release/uruson ~/.unison/v1/terms/\#t9p6gt572cnfofjuo7862sjqramtp4pv8gk5v1dk892r155t3eibfs412o8me6enmlc0appl0l9e7t88ndbbqj171tfbm9eq9kvf8og/
Hello, world!
Running "/Users/jared/.unison/v1/terms" - t9p6gt572cnfofjuo7862sjqramtp4pv8gk5v1dk892r155t3eibfs412o8me6enmlc0appl0l9e7t88ndbbqj171tfbm9eq9kvf8og
Time: 1118ms (1118042461ns)
-> 187027
        4.84 real         3.00 user         0.27 sys
 273383424  maximum resident set size
         0  average shared memory size
         0  average unshared data size
         0  average unshared stack size
     81545  page reclaims
         0  page faults
         0  swaps
         0  block input operations
         0  block output operations
         0  messages sent
         0  messages received
         0  signals received
        76  voluntary context switches
      2268  involuntary context switches
```