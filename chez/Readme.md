# Chez / Gerbil(Gambit)

Ok, what's the plan here?
We want a program that can produce scheme.
So far I've been taking the approach of "compile the world into a single binary".
Is that always the best policy?
Are there times I don't want to?
I mean, it's compiling in 10s of milliseconds right now, which is pretty solid.
Although if we start having a ton of code, it might be less practical.

The full mersenne generator is 87k of scheme, and the gerbil-compiled binary is
4.5mb.

## Next steps?

- get chez / gerbil passing tests folks.

- make some scheme macros so that the code generation can be even simpler, and more directly parallel unison syntax
