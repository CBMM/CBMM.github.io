---
title: Opening your research up to the web - Day 3
author: Greg Hale
tags: open science, teaching, snap, flask
---

In our [last lecture](posts/2016-01-12-opening-up-your-research-day-2.html) we set up a server to call one of several shell commands depending the URL.
I wouldn't recommend just rereading those posts in order to start web programming - instead you should refer to the very good documentation released with a web framework like [Snap](http://snapframework.com) or [Flask](http://flask.pocoo.org).
When you get some familiarity with the languages used ([Haskell](http://haskell.org) and [Python](https://www.python.org) respectively), then the details in our code samples will begin to make more sense.

# Direct library calls vs. shell commands

The servers we built were language agnostic.
They can call any program you have installed on your system, passing arguments through the command line.
Sometimes we can trade away language agnosticism for convenience and code quality.
For example, if the scientific application is written in Python, and our server is written in Python, then instead of using a `process` library to call out to the shell, we can invoke our scientific function directly from the Flask handler.

<!--more-->

```python
from flask import *
from reactiongeneratormachine import runReaction

@app.route('/<reactants>')
def handleReactants(reactants):

    r = json.loads(reactants)
    if validate(r):
        products  = runReaction(r)
        return jsonify(products)

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=80)
```

Calling the function directly has a couple of advantages.
First, if your library needs to keep some kind of configuration or state around, you can manage these things in Flask (or let the user configure the application through more URL parameters, a la `https://reactions.io/runReaction?q=Methane+Hydrogen&temperature=285` and with `request.get.args('temperature')` in the handler)

More importantly, calling a function directly for your handler removes the need to turn the request data into a string for the parameter to a call to the shell.
In languages like Haskell where types provide huge amounts of structure to your data, this can be a strong advantage.
In Python too, converting your data between wire formats appropriate for HTTP and a shell call can be tedious and error-prone, and Python's type system will do little to help you keep all those representations in sync, and you will find that corrupted data and server crashes become more frequent and difficult to fix as both your server and your scientific code become more complex.

# Synchronous and asynchronous request handling

Whether we are calling a shell command or a library function to process some data, what happens when multiple people simultaneously request some work do be done?
Let's look at a handler we used above:

```python
@app.route('/<reactants>')
def handleReactants(reactants):

    reactants = json.loads(reactants)
    if validate(reactants):
        products  = runReaction(reactants)
        return jsonify(products)
```

Our function starts executing when given a `reactants` parameter by the underlying web framework.
The parameter is parsed into a JSON value, validated, and sent along to `runReaction()`, which takes (let's say) two seconds to run.
When `runReaction()` returns, we serialize the result and send it back to the browser.
The call to `runReaction()` is 'synchronous': it takes a bite from the same time budget as everything else in the handler, and we can't move forward until it is finished running.
Furthermore, if Flask itself dispatches requests to handlers synchronously, then a second user issuing a request will have to wait for processing of the first user's request to finish before getting a turn.
What's worse, synchronous request handling would block *all* subsequent request handling, so user two wouldn't even get to see a web page or a 'Please wait for User 1' message while they wait - it would look to them as though the server were down until User 1's request is done being processed.
This is of course not Ok for a webserver, and we can tell Flask to handle routes asynchronously in the call to `app.run()`

```python
if __name__ == "__main__":
    app.run(host="0.0.0.0", port=80, threaded=True)
```

In our `Snap` server, request handling is threaded by default.
But if we make a synchronous function call in a handler, that handler still will not return until the synchronous call finishes.

# Asynchronous calls

What could we do if we want to let a response come back to the user, without making their browser sit in what appears to be a inactive 'loading' state?
Well, we would need to return *something* to the user other than the results of `runReaction()`, because `runReaction()` can't possibly run in less than two seconds (the problem is of course much worse for really long-running computation).
It turns out that the easiest way to solve our problem is to let the server stay busy during the computation (don't return immediately), but decouple that that waiting from the loading of a page.
Well, that's vague! Let's see some code:

### index.html

```html
<html>
  <head>
    <script src="http://code.jquery.com/jquery-2.2.0.js">
    </script>
    <script src="run.js"></script>
  </head>
  <body>
    <h1>Example</h1>
    <input type="text" placeholder="Methane" 
           id="reactant-text"></input>
    <button onclick="run_job();">Run Job</button>
  </body>
</html>
```

### run.js

```javascript
function run_job(){
    var inParam = $('#reactant-text')[0];
    $.ajax('/runReaction/' + inParam.value,
           {'success': function (d) {
               $('body').append(d);
             },
            'failure': function (e) { 
              $('body').append('Error: ' + e);
             }
           });
};
```

There are several new things going on here.
To answer our earlier question "What do we return to the user if not the results of 'runReaction()'?" - we're going to just return this `html` page.
The page links to the [jQuery](http://jquery.com) library and to a script called `run.js` that we will write.
The user doesn't type any data-processing URL, instead we give the user a button that we link to a javascript function called `run_job()`.

When the `run_job()` function runs (still in the browser), we first get the text value of our `reactant-text` input field (from index.html).
Then we see a call to [$.axax()](http://api.jquery.com/jquery.ajax/). The `$` is a short name for the `jQuery` object, which provides the `ajax` function.
`ajax` stands for "Asynchronous javascript and XML".
The gist of an ajax call is that it visits some URL (the first argument to `$.ajax()`), and then calls either the `success` or `failure` callbacks that you pass, depending on how things go on the server.

Importantly, calls to `$.ajax()` are *asynchronous*.
The ajax job runs in one of the browser's background threads.
The UI in the browser remains responsive, and the server remains responsive too, as long as it handles separate requests in separate threads.
The only blocking bit is the one server thread processing our request, and this works out very well for us!
The server only returns an answer when it has one, at which point the `success` callback handles it and updates the page according to how you want to display the result (here we just print the text of the result).
You can see the [results](https://www.youtube.com/watch?v=42PEVGVJVgk) and [code](https://gist.github.com/imalsogreg/dfa352b454c68693f28b) for a more thorough worked example.

At this point, you may be looking at three unfamiliar programming languages and a handful of new concepts.
It would be very unusual for someone to be able to put these things into use after a single reads though.
Developing web sites takes a lot of practice.
Keep reading, [google](http://google.com) liberally, [open an issue](http://github.com/cbmm/cbmm.github.io/issues) if you find something confusing (or wrong!).
You can do it!
Next time we will discuss a little about making pages look nice.
