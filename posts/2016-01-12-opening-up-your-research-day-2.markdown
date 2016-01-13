---
title: Opening your research up to the web - Day 2
author: Greg Hale
tags: open science, teaching, snap, flask
---

[IAP](http://web.mit.edu/iap) can giveth, and IAP can taketh away.
My bustling class of five has dropped by three and picked up one new student, then lost another.
But hope is not lost.
We may have a real project to work on.
Our student from [chemistry](http://chemistry.mit.edu) works on a very active Python codebase called the [Reaction Mechanism Generator](https://reactionmechanismgenerator.github.io).

Useful as it is, installing the dependencies has apparently been hard enough to limit adoption, so we want to expose some of its functionality through a web interface.
Just what I had in mind for our class!
We'll dedicate our three remaining sessions to this, hope that it benefits the Reaction Mechanism Generator project, and then see if the skills we learn spill over into other projects.

# Servers

On [Day 1](/posts/2016-01-11-opening-up-your-research-day-1.html) we talked about static assets like `.html` files, `.css` files, `.js` scripts, and images.
When the all we need to do is deliver these files to a user, we don't have to handle writing any code to serve the files; we can just find a static hosting services like [GitHub Pages](https://pages.github.com) or [Amazon S3](https://aws.amazon.com/s3).

But that was a whole day ago, and today we want more.
We need to be able to let the user specify a computation to run - like some set of reactants and environmental conditions - and we want to run the computation and return them a set of results.
The files we'd want to serve back don't exist yet - they are the output of the simulator, so our server needs to be a little more customized.

Fortunately for us, writing backend code is fun.
We get to choose any language we like as long as it has a library for serving HTTP traffic.
And we get to learn a little bit about [HTTP](http://www.tutorialspoint.com/http/http_requests) and [URLs](https://en.wikipedia.org/wiki/Query_string).

# Warm-up

We'll start with a miniaturized version of our final project goal.
For some small set of programs already available on our computer, let's write a server that calls those programs, passes them some data, and returns their result.
For example, `cowsay` and `doubleit`.

To get `cowsay`,

```
# on osx
brew install cowsay
# or ubuntu
sudo apt-get install cowsay
```

The program `cowsay` takes a string argument and returns an ASCII picture of a cow saying whatever you wrote in the string.
For example:

```
> cowsay This is a cowsay test

 _______________________
< This is a cowsay test >
 -----------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||

```

Very nice.

The program `doubleit` takes a single number and returns than number multiplied by two - it does not exist yet, so let's just write it in Python, in a file at `/usr/local/bin/doubleit`

```python
!#/usr/bin/env python

import sys

if __name__ == '__main__':
    i = sys.argv[1]
    print(2*float(i))
```

Any program printing twice its input will work, so I won't spend time explaining this.
But of course feel free to google around and pick apart any of the code examples in this series.

By this point, our machine is set up with a couple programs we want to expose - our standins for `runReactionSimulation`.
We can turn our attention to the HTTP server.

A server is basically just a function from HTTP Requests to HTTP Responses.
The request will be delivered to your server by the internet, usually coming from a web browser.
It is a block of ASCII text not unlike our cowsay picture, except that the text contains a URL, some metadata, a 'verb' (`GET`, `POST`, `PUT`, `DELETE`, or a hand full of others), and perhaps some auxiliary data.
The server we write will parse that request and deliver the pieces to you.
You will use them to determine what the user wanted, then build up a response - another block of text - to send back to the browser.

We wand our users to be able to ask for numbers to be doubled, or to have phrases cow-said, and we will ask them to use the URL bar for this. To get twice the number `20`, we ask them to visit

```
http://example.com/doubleit/20
```

For a cow saying `anything_at_all`:

```
http://example.com/cowsay/anything_at_all
```

We also want the user to see our static assets (`index.html` et. al.) when they visit `http://example.com/`.

The specification so far probably seems simple enough, but writing that software to handle the traffic may be more daunting.
Getting it up and running actually isn't as hard as you might imagine.
Maintaining it and keeping the bugs out is probably harder than you expect.

Let's just look at some code.
The following example is a complete working web server that implements our spec.

~~~~ {.haskell .numberLines}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8  (append, pack, unpack)
import Data.Maybe             (maybeToList)
import Snap                   (getParam, ifTop, route, writeBS)
import Snap.Http.Server       (commandLineConfig, quickHttpServe)
import Snap.Util.FileServe    (serveFile)
import System.Process         (readProcess)

------------------------------------------------------------------------------
-- | List of shell commands we'll make available to the server
cmds = ["doubleit", "cowsay"]

------------------------------------------------------------------------------
-- | A list of pairs: (Query striing to pattern-match from the URL, Handler)
routes = [ ("/:cmd/:arg", serveCommand cmds             ),
           ("",           ifTop (serveFile "index.html"))
         ]

------------------------------------------------------------------------------
-- | Main function chooses between serving static assets
--   and handling command requests
main = quickHttpServe (route routes)

------------------------------------------------------------------------------
-- | Handler to extract the command name and argument string
--   from the request, run it, and respond with the command's output
serveCommand okCmds = do
  cmd <- getParam "cmd"
  arg <- maybeToList <$> getParam "arg"
  case cmd of
    Nothing -> writeBS "No command"
    Just c
      | c `notElem` okCmds ->
        writeBS (append c " is not an ok command")
      | otherwise -> do
          retString <- liftIO (readProcess (unpack c) (map unpack arg) "")
          writeBS (pack retString)
~~~~

This is probably more intimidating than the other code examples we've seen, and some of the conventions are different from what we may be used to.
But we still haven't broken past 40 lines of code.

The first 10 lines are just listing the functions we're importing from the [Haskell](http://haskell.org) web framework [Snap](http://snapframework.com).
Lines 13 and 17 are values we're creating for later use - a list of strings corresponding to the commands on our computer we'll access, and then a list of `routes`.

Each `route` is a pair of a URL pattern and a handler function to execute if the pattern matches.
The `route` `"/:cmd/:arg"` will match any URL with the shape `http://example.com/something/another` - and we pair that pattern with `serveCommand cmds`, a function we write on lines 29-39.
The next route only matches `http://example.com/`.
That request will get routed to a function we imported from `Snap` for fetching files from the hard drive and sending them back to our client. `Snap` provides similar functions for allowing access to entire directories - you would do that if your site had more static assets than just a single `index.html` file.

The implementation of `serveCommand` pulls out the part of the URL matching our `:cmd` in the route pattern and the part matching `:arg`.
We test whether the `:cmd` part was empty (handling that case on line 33), and if not empty, we check to see that it is in the list of valid commands (line 35).
On line 38, we finally pass the command and argument parts to the function [readProcess](https://hackage.haskell.org/package/process-1.4.1.0/docs/System-Process.html#v:readProcess), which calls out to the shell environment to execute whatever command and argument were extracted from the URL[^longnote].

[^longnote]: The instances of [pack](http://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString.html#v:pack) and [unpack](http://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString.html#v:unpack) are transforming between different representations of text strings available in `Haskell`. [liftIO](https://hackage.haskell.org/package/transformers-0.4.2.0/docs/Control-Monad-IO-Class.html#v:liftIO) allows arbitrary IO actions to be embedded into the `Snap` monad. Making sense of these things requires studying [Haskell](http://haskell.org) the language and the [Snap Framework](http://snapframework.com), which each have great online documentation and communities.

We can achieve the same thing with a [Python](https://www.python.org) script using the [Flask](http://flask.pocoo.org/) framework.

~~~~ { .python .numberLines }
from flask import Flask
import os
import subprocess
app = Flask(__name__)

okCmds = frozenset(["cowsay","doubleit"])

@app.route('/<cmd>/<arg>')
def runCmd(cmd,arg):
    if cmd in okCmds:
        o = subprocess.check_output([cmd,arg])
        return ('<pre>' + o + '</pre>')
    else:
        return ( cmd + ' is not an ok command.' )

@app.route('/')
def index():
  return "Hi"

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8000)
~~~~

Flask provides a similar set of tools for taking a request, breaking it into parts, matching on the URL, and returning a response.
This code has the advantage that we can run it in Python without needing to run a compiler.
That can make it easier to get started and to experiment.
The disadvantage is that there is no compiler to check the code; and Python's increased flexibility means that it's easier to stretch into postures that are bugs.
Writing the above Python server was fun, but nearly every line contained a bug that I didn't catch until running the program.
When programs get bigger (as our server will when we graduate from `cowsay` to ReactionMechanismGenerator, we will see just how many places for bugs to shelter and enjoy the warmth emanating from our server machine).

We run the server code the same way we run any program.
In the compiled Haskell case[^1], we run our generated executable `shell-base-server`, optionally passing the aragument `-p 80`.
This argument specifies the port for the server to connect to. When debugging, we often default to port 8000.
Port 80 will expose your program to the public internet.
You will have to start the program as `sudo` to access port 80.
To run the Python server, pass the code to the Python interpreter: `python server.py`; the port selection in this case is made in the script.

Next time, we will talk about picking a server for the specific needs of a given project, and get to work building it out to those specs.
