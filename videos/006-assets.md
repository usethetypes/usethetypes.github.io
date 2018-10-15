---
title: "usethetypes#6: Adding Static Assets"
layout: video
categories: videos
youtube_id: 3y1qtngOuWM
episode_id: 6
slug: assets
---
Our web application can serve static pages. Now let's serve files from a directory of static assets.

### Builds on

* [usethetypes#5: Simple Site with Static Content][005-simple-site]

### Getting stuck in

### Step 1: Our starter app

```bash
cd $HOME/src
mkdir snap-hello-world
cd snap-hello-world
wget -O - https://github.com/usethetypes/usethetypes-005-simple-site/archive/master.tar.gz | tar xvz --strip-components=1
git init
git add .
code .
```

### Step 2: Link to a JavaScript file

Let's modify our HTML to include jQuery and add an ID, `date-time`, to an element:

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Current time</title>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>
    <script src="/static/index.js"></script>
  </head>
  <body>
    <h1>Current time</h1>
    <div id="date-time"></div>
  </body>
</html>
```

Then we will create the JavaScript file `static/index.js` and manipulate the `date-time` element from there:

```javascript
$(() => {
  const e = $("#date-time");

  function updateDateTime() {
    e.text(new Date().toString());
  }

  updateDateTime();
  setInterval(updateDateTime, 500);
});
```

This will update the `date-time` element with the current date/time every 500 milliseconds.

We can then use `serveDirectory` to expose all files in the `static` directory to the server:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Snap.Core (ifTop, route)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe (serveDirectory, serveFile)

main :: IO ()
main = quickHttpServe $
    ifTop (serveFile "views/index.html")
    <|> route
        [ ("/static", serveDirectory "static")
        ]
```

Let's fire up the integrated terminal with ``Ctrl+` `` and start GHCi:

```bash
stack ghci
```

And run `main`:

```ghci
:main
```

We can then refresh the browser and check that the JavaScript is picked up.

### Step 3: Serve a stylesheet too

Let's finish up by adding a stylesheet to our simple web site:

```css
body {
  background-color: silver;
  font-family: 'Franklin Gothic Medium', 'Arial Narrow', Arial, sans-serif;
  margin: 2em;
}
```

and `views/index.html`:

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Current time</title>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>
    <script src="/static/index.js"></script>
    <link rel="stylesheet" href="/static/index.css">
  </head>
  <body>
    <h1>Current time</h1>
    <div id="date-time"></div>
  </body>
</html>
```

Refresh your browser to view the result and we're done!

[005-simple-site]: 005-simple-site
