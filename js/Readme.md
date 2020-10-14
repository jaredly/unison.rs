
The javascript runtime!

It would be nice to:
- have evaluation logs on a per-value basis.
  - oooh yeah that would be so dope.
  - ok, so we're tracing execution, basically, right?
  - showing the decompiled values n stuff
  - and then showing what the values were when it was executed.
  - not totally sure how to keep track of it, but it's super cool.
  - would also be nice to have a format where the rust runtime could also do the jig.


AUDIT
- ir_exec -- ok, added a bunch of clones that might be extraneous


Once it's working, trying changing some `clone`s back to `slice`s

NEXT MOVE: import runtime_tests.json and run them! in nodejs folks. Maybe I'll want webpack doing the deal?