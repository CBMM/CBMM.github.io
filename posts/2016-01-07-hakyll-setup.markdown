---
title: Hakyll setup
author: Greg Hale
tags: blog, web, hakyll, pandoc
---

Here is the first post on the CBMM @ Github website. As explained on the [About](/about.html) page, we wanted a space to organize the open-source activity going on at the [CBMM](http://cbmm.mit.edu), beyond just the wall of repositories on our github [org page](https://github.com/CBMM).

The design decisions reflect the broader universe of the CBMM software situation.
The developer staff is small, and there are far more projects to do than people to do them. 
At the same time we want the site to be reliable to maintain and simple to edit with the same workflow that we use for developing software.
We also want the development process to be totally *open*.
So, [GitHub pages](https://pages.github.com/) is an extremely good fit for hosting.
We don't have to do any maintenance or worry at all about backups.
And the site content is generated mainly from [markdown](http://commonmark.org/help/) via the fantastic document converter [pandoc](http://pandoc.org) and static site generator [Hakyll](http://jaspervdj.be/hakyll).

<!--more-->

We will be exercising Pandoc and Hakyll more in the future, especially looking at whether they are a good fit for publication-like posts with references and figures. But for now, getting basic blog functionality, quick uploads, and very small page sizes, I'm happy with the tech choices.

The rest of this post consists notes on design goals and things that needed tweaking for all the parts to fit together.
If they help inform you in your own site set-up, then I'm happy.

## Load time

Fast page loads are important[^1], and high-res monitors are becoming more popular, so the big banner at the top is an SVG image drawn in [inkscape](https://inkscape.org/en/) and weighing in at a delicate 24.1 kB (right-click, the choose `select` from the context menu, find the `Network` tab, and reload the page to get a listing of the assets and their sizes).
I went with Yahoo's [Pure CSS](http://purecss.io) for a change, instead of the more typical [Twitter Bootstrap](http://getbootstrap.com). Together Pure's files are less than 6 kB (compared to bootstap, which is closer to 30 kB).
These are all *very* small numbers.
But it was fun to get to try something different.
Getting the responsive display hamburger menu working required copying some HTML from the [side-menu](http://purecss.io/layouts/side-menu/) Pure layout example.

## Sticky Footer[^longnote]

The [recommended trick](https://philipwalton.github.io/solved-by-flexbox/demos/sticky-footer/) of using a flexbox to achieve a sticky footer[^2] didn't work for me, probably because the responsive nav element has a strange shape that confuses the flexbox layout (or possibly just the *person* trying to use the flexbox layout).

The workaround was to wrap all of the `body` content together in a `<div class="layout">` and make that layout div the `display: flex` container, rather that assigning that role directly to the `<body>`.

## Hamburger space

I prefer when the a sliding side menu covers the underlying content, rather than pushing the content over to make space.
The latter is the default in PureCSS's `side-menu` layout. Changing that was a matter of dropping the `left: 150px` in the `#layout.active` property of `side-menu.css`.

## Active link

Although PureCSS offers some javascript to highlight the menu item corresponding to the page you're looking at, I wanted to try to handle this statically in Hakyll, and I followed [Mesokurtosis's](http://mesokurtosis.com/posts/2015-02-22-pages.html) blog instructions to a T. Frankly I don't have any intuition about how Hakyll's templating system or compilers work (I should get on that!). Fortunately, one can clearly put a site together without understanding very much.

## Teasers, RSS, and Tags

Here again, I have some homework to do to understand the Hakyll compilation process.
I followed verbatim the [instructions](https://github.com/jaspervdj/hakyll/blob/master/web/tutorials/using-teasers-in-hakyll.markdown) from Hakyll's author, [Jasper Van der Jeugt](https://jaspervdj.be/), and another more hand-holding [guide](http://reichertbrothers.com/blog/posts/2014-04-08-hakyll-teasers.html). 

To set up the RSS feed I followed another of Jasper's [tutorial](https://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html)s. And for post tags, I looked at his own blog's [repository](https://github.com/jaspervdj/jaspervdj/blob/d0d76d0728f7827357b1ac8b9193f0630eda9019/src/Main.hs#L82-L97) and another [guide](http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html).
I'd be lying if I didn't admit some shame for taking so many shortcuts!
But please do have a look at the [repository](https://github.com/CBMM/CBMM.github.io) and feel free to clone the whole thing if this is a reasonably starting-point for your own Hakyll blog.

## Deployment

To manage the interaction with GitHub Pages, I keep all of the site's sources on the [hakyll branch](https://github.com/CBMM/CBMM.github.io/tree/hakyll). When Hakyll compiles a site, it puts the whole thing into a directory called `_site` in the root of the project.
By adding this repository's `master` branch as a [submodule](https://git-scm.com/docs/git-submodule) under the name `_site`, I'm able to deploy the site by simply `cd`ing into `_site`, committing all the files and pushing them to the master branch.
This method I learned from a [Stack Builders](http://www.stackbuilders.com) blog [post](http://www.stackbuilders.com/news/dr-hakyll-create-a-github-page-with-hakyll-and-circleci).
The script to do the pushing is [here](https://github.com/CBMM/CBMM.github.io/blob/hakyll/deploy.sh).

[^1]:[The Web Obesity Crisis](https://vimeo.com/147806338)
[^2]:[https://philipwalton.github.io/solved-by-flexbox/demos/sticky-footer/](https://philipwalton.github.io/solved-by-flexbox/demos/sticky-footer/)
[^longnote]: Sometimes it's the easiest-seeming things that are hardest in programming and web design. 'Sticky footer' refers to the ability for the page's footer to stick to the very bottom of the window, even if the window's other content is short and doesn't reach the bottom. It can be surprisingly hard to make this happen reliably. See the [flexbox solution](https://philipwalton.github.io/solved-by-flexbox/demos/sticky-footer/) for a clearer demo of what I mean here. Relatedly, [this](https://en.wikipedia.org/wiki/Holy_Grail_(web_design)) pattern is called the "Holy Grail" layout, because it's often what the designer has in mind while being surprisingly tricky to implement.
