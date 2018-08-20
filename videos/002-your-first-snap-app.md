---
title: "usethetypes#2: Your First Snap App"
layout: video
categories: videos
youtube_id: _C3QsKF4UQI
episode_id: 2
slug: your-first-snap-app
---
Now you have working Haskell Stack and VSCode installations, we can
create our first [Snap][snap-framework] application in Haskell. This
application will show you how to respond to the top-level route and how
to use alternation to handle named routes as well as a default route.

### Builds on

* [usethetypes#1: VSCode and Intero][001-vscode-intero]

### Notes

* This tutorial assumes that you have a working Haskell Stack installation as well as an editor, such as VSCode, set up for editing your code

### Step 1: Create starter project

```bash
stack new snap-hello-world simple --resolver=lts-12.0
cd snap-hello-world
git init
echo /.stack-work/ >> .gitignore
git add .
stack build --fast
stack exec snap-hello-world
git commit -m "Step 1: Create starter project"
```

### Step 2: Add Snap dependencies

Let's start a build loop:

```bash
stack build --fast --file-watch
```

Now we can see the results of our changes as they are saved.

Next, add `snap-core` and `snap-server` to the `build-depends` section
of your your `.cabal` file.

```bash
stack build --fast
git add .
git commit -m "Step 2: Add Snap dependencies"
```

### Step 3: Return "Hello World"

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Snap.Core (ifTop, writeText)
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = quickHttpServe $ ifTop (writeText "Hello world")
```

This makes use of three Snap functions:

* `ifTop`
* `writeText`
* `quickHttpServe`

It also uses a very commonly used language extension, namely
`OverloadedStrings`.

```bash
stack build --fast --exec snap-hello-world
git add .
git commit -m "Step 3: Return \"Hello World\""
```

Open up your browser and navigate to `http://localhost:8000`.

Voil&agrave;&mdash;we have pretty much the simplest possible web site in
about five lines of Haskell.

Also try out `http://localhost:8000/foo.html`. This errors out. This is
because the application explicitly handles only the top-level route
using `ifTop`.

### Step 4: Clean up

Let's go back and address the warning we see when we run the program:

```bash
stack exec snap-hello-world
```

What we're seeing is a function of the default configuration used by
`quickHttpServe`: it expects a `log` directory under the current
directory in order to log to. Let's create such a directory and ignore
it in Git:

```bash
mkdir log
touch log/.keep
git add log/.keep
echo /log/ >> .gitignore
stack exec snap-hello-world
git status
git diff
git add .
git commit -m "Step 4: Clean up"
```

Note that this is relative to the current working directory. Thus if you
run `snap-hello-world` in a subdirectory, the program will expect a
`log` directory there instead. Most likely as the program develops in
sophistication, you'll end up logging to a location determined by an
environment variable or command-line option. We'll explore this kind of
configuration in later videos.

### Step 5: Handle all routes indiscriminately

In `src/Main.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Snap.Core (writeText)
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = quickHttpServe $ writeText "Hello world"
```

Check out a few examples:

* `http://localhost:8000`
* `http://localhost:8000/foo.html`
* `http://localhost:8000/bar.html`

### Step 6: Alternation

Originally, we responded to only the top-level route using `ifTop` and
errored out for everything else. Then we removed this restrictions to
return "Hello World" for all paths. Let's add an alternative route:

Update `src/Main.hs` to look like the following:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Snap.Core (ifTop, writeText)
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = quickHttpServe $
    ifTop (writeText "Hello world")
    <|> writeText "Bad path"
```

Now we're using `ifTop` again plus `<|>` to provide alternatives. Snap
will evaluate the path conditions in the order they're defined. `ifTop`
matches `/` while the last `writeText "Bad path"` matches anything not
already matched.

### Step 7: Matching paths

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Snap.Core (ifTop, route, writeText)
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = quickHttpServe $
    ifTop (writeText "Hello world")
    <|> route [ ("/ping", writeText "Ping") ]
    <|> writeText "Bad path"
```

`route` allows us to provide a list of zero or more "named" paths.

### Conclusions

You've created your first Snap app. You can build, run it and test it in
your browser.

### Next steps

Watch this space!

[001-vscode-intero]: 001-vscode-intero
[snap-framework]: http://snapframework.com/
