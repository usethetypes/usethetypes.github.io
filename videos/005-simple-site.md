---
title: "usethetypes#5: Simple Site with Static Content"
layout: video
categories: videos
youtube_id: 2D544vHZF00
episode_id: 5
slug: simple-site
---
We have the beginnings of a useful development environment. We'll build on this by making our little Snap-based Haskell web application serve a static page.

### Builds on

* [usethetypes#3: Rapid Development with GHCi][003-rapid-dev-ghci]
* [usethetypes#4: Rapid Development with ghcid][004-rapid-dev-ghcid]

### Step 1: Our starter app

```bash
cd $HOME/src
mkdir snap-hello-world
cd snap-hello-world
wget -O - https://github.com/usethetypes/usethetypes-004-rapid-dev-ghcid/archive/master.tar.gz | tar xvz --strip-components=1
git init
git add .
code .
```

### Step 2: Tweak indentation settings

First of all, I wanted to say a few words on the subject of indentation. I like to use four spaces per tab for my Haskell code and this just happens to be Visual Studio Code's default. For HTML, JavaScript and CSS, however, I like something more compact, specifically two spaces per tab. Let's start VSCode and configure it to indent these formats as such while leaving the four-space default for everything else.

1. Go to _File_ | _Preferences_ | _Settings_ (or use the keyboard shortcut `Ctrl+,`)
1. Click on the ellipsis on the right-hand side
1. Click _Open settings.json_
1. Add the following to the top-level JSON object:
```json
"[css]": {
    "editor.tabSize": 2
},
"[html]": {
    "editor.tabSize": 2
},
"[javascript]": {
    "editor.tabSize": 2
}
```

You may need to close and reopen files of affected types after making this change.

### Step 3: Create a single static HTML page

I'll open the integrated terminal with ``Ctrl+` `` and create a `views` directory and an empty HTML file:

```bash
mkdir views
touch views/index.html
```

Now, we can create some HTML in this file:

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Hello world</title>
  </head>
  <body>
    <h1>Hello world</h1>
  </body>
</html>
```

### Step 4: Modify `Main.hs`

We can then update our `Main.hs` to serve this file:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Snap.Core (ifTop)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe (serveFile)

main :: IO ()
main = quickHttpServe $
    ifTop (serveFile "views/index.html")
```

In our integrated terminal, we can start GHCi

```bash
stack ghci
```

and run our `main` function:

```ghci
:main
```

We can then check this out in our browser at `http://localhost:8000/`.

[003-rapid-dev-ghci]: 003-rapid-dev-ghci
[004-rapid-dev-ghcid]: 004-rapid-dev-ghcid
