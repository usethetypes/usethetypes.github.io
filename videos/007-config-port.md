---
title: "usethetypes#7: Configuring the Web Server Port"
layout: video
categories: videos
youtube_id: JYxSAH78u_Q
episode_id: 7
slug: config-port
---
We're are gradually building up our app so that we can deploy it to a hosting provider such as [Heroku][heroku]. Providers like Heroku do not guarantee that the app will always be able to listen on the same port. In this short video, we'll modify our web app so that the web server port can be configured via an environment variable.

### Step 1: Grab our web app

Now we're going to get our little Haskell app's source code and create a new repo:

```bash
cd $HOME/src
mkdir snap-hello-world
cd snap-hello-world
wget -O - https://github.com/usethetypes/usethetypes-006-assets/archive/master.tar.gz | tar xvz --strip-components=1
git init
git add .
```

### Step 2: Check the current behaviour

Let's run our app to check its current behaviour:

```bash
stack build --exec snap-hello-world
```

We can see that it listens on port 8000 by default. Make a mental note of this.

### Step 2: Make the port configurable

Heroku passes the web server port to the executable using the `PORT` environment variable. We'll modify our program to read the port from this environment variable or default to port 8000 if this environment variable is not defined.

To do this we'll use `lookupEnv` from `System.Environment`. We can view a function's type signature from GHCi which we can start up as follows:

```bash
stack ghci
```

From GHCi, you can then view the type signature as follows:

```ghci
:type System.Environment.lookupEnv
:quit
```

This function evaluates to an I/O action that will return `Nothing` if the environment variable is not defined. We can use the following trickto view its type signature without running GHCi interactively:

```bash
stack ghci <<< ':t System.Environment.lookupEnv'
```

In future videos, we'll revisit this and other ways to look up type signatures.

We can then combine `lookupEnv` with the `maybe` function to provide a default value:

```bash
stack ghci <<< ':t maybe'
```

This takes a default value and a function to apply to the `Maybe` value if it is not `Nothing`. We can string these together to get a port with a default value. We can then use `httpServe` instead of `quickHttpServe` to allow us to pass in our configuration created using the `setPort` function. Let's start VSCode and open an integrated terminal and update our program:

stack exec ghcid

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Snap.Core (ifTop, route, writeText)
import Snap.Http.Server (httpServe, setPort)
import Snap.Util.FileServe (serveDirectory, serveFile)
import System.Environment (lookupEnv)

main :: IO ()
main = do
    mbPort <- lookupEnv "PORT"
    let port = maybe 8000 read mbPort
        config = setPort port mempty
    httpServe config $
        ifTop (serveFile "views/index.html")
        <|> route
            [ ("/static", serveDirectory "static")
            ]
```

### Step 3: Demonstrate the default behaviour

```bash
stack build --exec snap-hello-world
```

### Step 4: Demonstrate the `PORT` environment variable

```bash
PORT=8888 stack build --exec snap-hello-world
```

We can visit this in our browser to confirm that it's listening on the correct port: `http://localhost:8888`.

[006-assets]: 006-assets
[heroku]: https://www.heroku.com/
