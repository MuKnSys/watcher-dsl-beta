t clone https://github.com/MuKnSys/watcher-dsl-beta

cd watcher-dsl-beta/src/haskell

cabal build all

cd dist-newstyle/build/x86_64-linux/ghc-<ghc-version>/haskell-0.1.0.0/x/haskell/build/haskell

./haskell "compile" /watcher-dsl-beta/data/example <current-directory>/watchers/watcher-ts
