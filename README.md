# whatisthisgame

This is a game (titled *whatisthisgame*) written in Haskell! The graphics are
rendered using OpenGL, and the game logic is modeled using Elerea.

Links follow:

* [Haskell](http://haskell.org).
* [OpenGL](http://hackage.haskell.org/package/OpenGL) bindings for Haskell.
* [Elerea](http://hackage.haskell.org/package/elerea).

**WARNING**: The code in its current state is a little dirty. I'm working on a
'*do it first, then make it beautiful*' basis. I'll come back and polish
everything up after the project is reasonably complete.

### Installation

```bash
>$ git clone https://github.com/crockeo/whatisthisgame.git
>$ cd whatisthisgame/
>$ cabal sandbox init # Optional
>$ cabal install --only-dependencies

# To build you can do one of these two:
>$ cabal build
>$ make
>$ make fast # Uses make with -j4

# To run directly you can do:
>$ cabal run
```
