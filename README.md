# cseg
Text chunking with collocation segmentation
# Installation

[Haskell Platform](https://www.haskell.org) is on your system.

> git clone git://github.com/vidaud/cseg.git
> cd cseg

## with cabal

> cabal install --only-dependencies
> cabal buid

## compiling with ghc

> cabal install cmdargs
> ghc --make -threaded -rtsopts -with-rtsopts="-N" -O2 cseg.hs


