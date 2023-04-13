# watcher-dsl-beta

Watcher DSL is a domain specific language (DSL) designed to provide a well-structured, machine-readable, non-Turing complete environment for composing watchers and improving the developer experience.

You can find [here](https://docs.google.com/document/d/1gDyuC78L_BdqCin9_N_iAUvyHVA-v0WEXEdTiFYO6q4) a more detailed specification of the DSL.

## Instructions
* As example use the watcher from `data/example` folder.
* In the `example/ERC20TOT.watcher` put the directory of the contract into the import declaration f.e `/home/user/Desktop/watchers/watcher-ts/node_modules/@openzeppelin/contracts/token/ERC20/ERC20.sol`. Contracts path currently had to be absolute. 
* To build all node project dependencies go into: `/watcher-dsl-beta/src/node` and run `npm install` command.
* To build all haskell project dependencies go into: `/watcher-dsl-beta/src/haskell` and run `cabal build all` command.
* After this you can run command from `watcher-dsl-beta/src/haskell/dist-newstyle/build/x86_64-linux/ghc-9.2.4/haskell-0.1.0.0/x/haskell/build/haskell`
* There are three paths that you have to provide as a parameters: 
    * Path to .watcher file. 
    * Path to watcher-ts library. 
    * Path to node directory in watcher-dsl-beta project.
* example `./haskell "compile" /home/user/Desktop/watcher-dsl-beta/data/example /home/user/Desktop/watchers/watcher-ts /home/user/Desktop/watcher-dsl-beta/src/node/`
* watcher will appear in packages folder of watcher-ts library 
## Hardware Requirements 
* OS: Ubuntu 22.10
* CPU: 64-bit, 2.5 GHz.
* RAM: Minimum 4 GB of RAM, but 8 GB or more recommended
* Storage: At least 20 GB of free disk space recommended.
* OS: Windows, macOS or Linux.

## Dependencies

* [Node JS / npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
* [Haskell](https://get-ghcup.haskell.org/)
    * GHC 9.2.4
    * [Cabal 3.6.2.0](https://www.haskell.org/cabal/)
    * Stack 2.9.3 
    * HLS 1.10.0.0 
* Builded [watcher-ts](https://github.com/cerc-io/watcher-ts) library


## Previous Examples

#### preview:
 27-03-23 
 [![Demo](https://i.imgur.com/pqrbpWp.png)](https://vimeo.com/810525901) 

#### TypeScript Generation Demo:
 08-03-23 
 [![Demo](https://i.imgur.com/MK6Xkk7.png)](https://vimeo.com/805929364) 

 


#### preview:
 15-02-23 
 
 https://user-images.githubusercontent.com/76069115/219162487-57f6b676-106e-4056-bdaf-9425c74b2be3.mp4

#### preview:
 22-02-23 
 

https://user-images.githubusercontent.com/76069115/220672066-92fb5bc3-f413-4769-82f2-71677e5fa80a.mp4
