#!/bin/bash

export PATH=/home/codeworld/.cabal/bin:$PATH

cd funblocks-server
cabal build
cd ..

cabal install ./funblocks-server

cwd=$(pwd)

cd funblocks-base/src

ghcjs Main.hs

cd $cwd

ln -s -r ./funblocks-base/src/Main.jsexe/lib.js ./web/js/blocks_lib.js
ln -s -r ./funblocks-base/src/Main.jsexe/out.js ./web/js/blocks_out.js
ln -s -r ./funblocks-base/src/Main.jsexe/rts.js ./web/js/blocks_rts.js
ln -s -r ./funblocks-base/src/Main.jsexe/runmain.js ./web/js/blocks_runmain.js
