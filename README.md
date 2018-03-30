# CSEG
Text chunking with collocation segmentation
# Installation

[Haskell Platform](https://www.haskell.org) is on your system.

```
git clone https://github.com/vidaud/cseg.git
cd cseg
cabal configure
cabal install --only-dependencies
cabal build
```

## recompiling with ghc

```
ghc --make -threaded -rtsopts -with-rtsopts="-N" -O2 cseg.hs
```

# Using

For help enter:
```
./cseg --help
```

## training

```
./cseg -d/path/to/corpus/en -nen -atrain -etxt
```
This will look for files with extention txt in /path/to/corpus/en directory and its subdirectories, and will save n-gram data in en.uni and en.bi files. Tokenization is not enabled and only whitespace is used to read tokens. 

You can enable tokenization with ''-t'' option.

## chunking

```
./cseg -d/path/to/corpus/en -m -cmajority -nen -aseg -etxt
```
This will look for files with extention txt in /path/to/corpus/en directory and its subdirectories, will use ngram data from en.uni and en.bi files, and will apply majority method for segmentation. Tokenization is not enabled and only white-space is used to read tokens. RAM memory use is disabled leading to slow processing. You need to use -u option to enable memory use to speed up processing (you should have at least 16 GB RAM to enable -u option).
Use **-a** option when you chunk texts not included in training data.

# Data
You can download data already preprocessed data for training and testing. The data source is [OPUS web site](http://opus.nlpl.eu/). We took subsets from [BOOKS](http://opus.nlpl.eu/Books.php), [Global Voices](http://opus.nlpl.eu/GlobalVoices.php) and [News Commentary](http://opus.nlpl.eu/download.php?f=News-Commentary11.tar.gz).
The data are merged from these three sources and then split into files with 1000 sentences each:

[AR](http://textmining.lt/corpora/ar.zip) 
[BN](http://textmining.lt/corpora/bn.zip) 
[CS](http://textmining.lt/corpora/cs.zip) 
[DE](http://textmining.lt/corpora/de.zip) 
[EN](http://textmining.lt/corpora/en.zip) 
[ES](http://textmining.lt/corpora/es.zip) 
[FR](http://textmining.lt/corpora/fr.zip) 
[HU](http://textmining.lt/corpora/hu.zip) 
[IT](http://textmining.lt/corpora/it.zip) 
[MG](http://textmining.lt/corpora/mg.zip) 
[NL](http://textmining.lt/corpora/nl.zip) 
[PT](http://textmining.lt/corpora/pt.zip) 
[RU](http://textmining.lt/corpora/ru.zip) 
[ZH](http://textmining.lt/corpora/zh.zip) 

