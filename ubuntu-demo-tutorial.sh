#!/usr/bin/env bash

function confirm {
  read -r -p "$1 [y/N] " response
  if [[ "$response" =~ ^([yY]|[yY])$ ]]; then
    true
  else
    false
  fi
}

if confirm "Do you want to clone the repository?"; then
  git clone https://github.com/MuKnSys/watcher-dsl-beta
fi

if confirm "Do you want to navigate to the node source directory and install npm dependecies?"; then
  cd src/node
  npm install
fi

if confirm "Do you want to navigate to the haskell source directory and build the project? "; then
  cd ../haskell
  cabal build all
fi


if confirm "Do you want to navigate to the Haskell binary directory?"; then
  cd dist-newstyle/build/x86_64-linux/ghc-<ghc-version>/haskell-0.1.0.0/x/haskell/build/haskell
fi

if confirm "Do you want to compile the DSL file?"; then
  ./haskell "compile" /watcher-dsl-beta/data/example <current-directory>/watchers/watcher-ts
fi
