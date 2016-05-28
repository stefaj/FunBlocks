#!/bin/bash

export PATH=/home/codeworld/.cabal/bin:$PATH

cd funblocks-server
cabal build
cd ..

cabal install ./funblocks-server

