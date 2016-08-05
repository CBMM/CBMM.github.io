#!/usr/bin/env sh

dist/build/site/site clean
git submodule init
git submodule update
cd _site && git checkout master && cd ..
dist/build/site/site build
cp README.md _site/README.md
cd _site && git add -A && git commit -m "build" && git push origin master
