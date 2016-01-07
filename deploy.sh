#! /bin/bash

.cabal-sandbox/bin/site clean
.cabal-sandbox/bin/site build
cp README.md _site/
cd _site && git add *.html *.png *.jpg *.gif *.js *.css *.md
cd _site && git commit -m "build" && git push origin master
