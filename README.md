# CBMM.github.io

A lightweight site & blog for CBMM's github presence.

## Contributing

Work on the `hakyll` branch. The [Hakyll](http://jaspervdj.be/hakyll) static site generator will build the site from the `.html`, `.markdown`, and `.org` files in the `posts` directory and in the root of the repository. If you have a working Haskell setup, you can see the results of your changes on a version of the site that runs on your computer (at http://localhost:8000)

```bash
git clone -b hakyll git@github.com:CBMM/CBMM.github.io
cabal sandbox init
cabal sandbox install --only-dep
cabal install
.cabal-sandbox/bin/site watch
% Edit or add a post in the posts/ directory. Go to localhost:8000 and reload
```

Or can get live updates of your work using `stack`, so you don't have to install Haskell yourself. Follow the `stack` installation [instructions](http://docs.haskellstack.org/en/stable/install_and_upgrade.html), then

```bash
git clone -b hakyll git@github.com:CBMM/CBMM.github.io
stack init --resolver lts-4.0
stack build
cp .stack-work/install/x86_64-osx/lts-4.0/7.10.3/bin/site ./site
./site watch
```

Watching the changes locally is nice because you don't have to re-upload the site to github to check that your formatting worked nicely. But if you don't want the hastle, you can just edit your post, push the changes back to GitHub, and let them get picked up the next time the site is built.
