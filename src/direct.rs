use super::env::*;
use super::types::*;

// ok.. hm ...
// So the only things that we can do
// Ok, so we convert to "direct style"
// where we don't have any .. nesting?
// like, hm but we do have flow control
// Ok .. but back to the idea of each place
// in the thing having .. a .. an index associated with it
// but then ..
// yeah traversing .. down .. into .. expressions . makes it hard . because
// hm maybe I can avoid that too?
// Case:
// match m with
//   n | a b c + d e f > 10 -> 5
//   _ -> 6
/*

So it's like. we're evaluating `d e f` and we need to be able to
get back
to
the beginning

I guess transforming a match into a series of if/else .. makes the most sense?

hm

ok, I think I definitely want to be able to continue using the direct style,
where I'm using the rust stack. Right?

Yeah, otherwise I have to actually have an ABI n stuff
which I dont love.

But, on the other hand, I can't really do the "mutative" version
can I?
I could try it.

*/

fn direct(input: ABT<Term>) {}
