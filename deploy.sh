#! /bin/bash

.cabal-sandbox/bin/site clean
git submodule init
git submodule update
cd _site && git checkout master && cd ..
.cabal-sandbox/bin/site build
cp README.md _site/README.md
cd _site && git add -A && git commit -m "build" && git push origin master
