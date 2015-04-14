setting up haskell
tested on ghc 7.8.3 and cabal 1.18. Should work fine on newer versions, but newer versions break ghc-mod :(
note: cabal install cabal-install will install cabal in ~/.cabal/bin, update your path accordingly if you want to use that.


steps to build:

clone the repo, cd into it.

cabal sandbox init
cabal install --only-dependencies    #this will take a while... add -j4 to parallelize build
cabal configure && cabal build
cabal run






