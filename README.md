# cseg
Text chunking with collocation segmentation
# Installation

[Haskell Platform](https://www.haskell.org) is on your system.

> git clone https://github.com/vidaud/cseg.git

> cd cseg

> cabal configure

> cabal install --only-dependencies

> cabal build

## recompiling with ghc

> ghc --make -threaded -rtsopts -with-rtsopts="-N" -O2 cseg.hs



