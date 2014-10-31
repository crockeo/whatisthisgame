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

For building this you have two options: static or dynamic linking (in the scope of the Haskell libraries). The default state of cabal file is to prefer dynamic linking (because once everything is set up, it's quicker to compile and recompile), but it may be easier for someone to compile it statically.

Before we do anything specific to each kind of build, we need to grab the source from this repo:

```bash
$ git clone https://github.com/crockeo/whatisthisgame.git
$ cd whatisthisgame/
$ cabal sandbox init # Optional
```

#### Dynamically

The main difference between static and dynamic compiling is making sure the packages you're using are compiled to shared libraries.

```bash
$ cabal install --only-dependencies --enable-shared
```

If you're building from already having some static libraries (such as OpenGL and OpenGLRaw) you may need to reinstall those as `--enabled-shared` as well.

For instance, when using Haskell Platform version `2014.2.0.0`, I needed to reinstall `OpenGL`, `OpenGLRaw`.

```bash
$ cabal install OpenGL-2.9.2.0 OpenGLRaw-1.5.0.0 --reinstall --enable-shared
```

#### Statically

To switch this project back to static compiling, you just need to edit the `whatisthisgame.cabal` file to remove the `-dynamic` flag from the `ghc-options` line near the bottom.

```cabal
-- Before edit
ghc-options: -O2 -Wall -fno-warn-unused-do-bind -dynamic

-- After edit
ghc-options: -O2 -Wall -fno-warn-unused-do-bind
```

#### Building

After you've set up the project for either kind of building, the actual process is generic.

```bash
# To build
$ cabal build

# To run
$ cabal run

# To install
$ cabal install
```
