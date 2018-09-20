---
title: "usethetypes#3: Rapid development with GHCi"
layout: video
categories: videos
youtube_id: nDFSKTLUwvE
episode_id: 3
slug: rapid-dev-ghci
---
We've written one or two real programs. Our approach so far has been to edit in VSCode and then to build and run from the command line using Stack. To try out changes, we then go back to VSCode and repeat the cycle. This requires frequent context switches between the editor and the terminal and back again in order to develop our programs iteratively. In the next couple of videos, we'll look at some straightforward ways we can streamline the [edit-build-run-test][edit-compile-link-run] cycle.

### Builds on

* [usethetypes#1: VSCode and Intero][001-vscode-intero]
* [usethetypes#2: Your First Snap App][002-your-first-snap-app]

### Background

Haskell is a compiled language much like Java. If we follow typical Java practice by relying exclusively on editing, building and re-running our program, our iteration time will gradually become longer and longer as our program becomes longer and longer. Java partly solves this problem by compiling more quickly than Haskell (I think). With Haskell we can combat long build times by splitting our program up into modules. However, I don't know if this really achieves much and nothing I've seen about Stack and GHC has demonstrated that they do a good job (or any job at all) of performing incremental compilation.

This behaviour stands in contrast with the developer experience we see with web with _scripting_ languages like Ruby (with Rails), Python (with Django) and PHP and so on, as typically encountered in web development. With these languages, developers have the expectation that they can write their code, save their files and more or less immediately see the effect of the changes by refreshing their app in the browser. These languages have the (unacceptable, some might say) downside of weak static type systems that defer too many types of error to runtime.

In this video, I make the claim that Haskell can give you an acceptable compromise between the two workflows:

1. A strong static type system
1. Efficient native code when fully compiled for production use
1. An interpreter (GHCi) for rapid iteration

In [usethetypes#1][001-vscode-intero], we saw how to set up VSCode with the Haskero plugin. This gave us symbol navigation and squiggly red lines. Along with the GHC optimizing native code compiler, we accomplish goals (1) and (2) above.

I will show you that Haskell can provide an _interpreted_ experience that supports rapid iteration through the use of [GHCi][ghci]. This will start to address item (3) above.

### Step 1: Our starter app

```bash
# Clone our Snap app
cd $HOME/src
mkdir rapid-dev
cd rapid-dev
wget -O - https://github.com/usethetypes/usethetypes-002-your-first-snap-app/archive/master.tar.gz | tar xvz --strip-components=1

# Create a Git repo so we can track changes
git init
git add .

# Build and run program in terminal
stack build --fast
stack exec snap-hello-world

# Open up our editor
code .
```

Check the output in your browser at `http://localhost:8000`.

First, kill the program with `Ctrl+C`. We can then return to VSCode and open an integrated terminal using ``Ctrl+` `` and fire up GHCi (Glasgow Haskell Compiler Interactive):

```shell
stack ghci
:main
```

Check the output in your browser at `http://localhost:8000`.

### Step 2: Make an edit

In `src/Main.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Snap.Core (ifTop, route, writeText)
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = quickHttpServe $
    ifTop (writeText "index")
    <|> route [ ("/ping", writeText "Ping") ]
    <|> writeText "Bad path"
```

Save this change, go to your GHCi session and press `Ctrl+C` to interrupt the program. Then run the following commands:

```shell
:reload
:main
```

Refresh your browser and you'll see `index` instead of `Hello World`.

While this does involve interrupting the interpreter and running a couple of annoying commands repeatedly, you should already see that reloading and run a Haskell program in _interpreted mode_ is substantially faster than performing a full compile and link using `stack build`, for example.

### Step 3: Add some keyboard shortcuts

With this workflow, the ``Ctrl+` `` keyboard shortcut is worth getting used to. There are likely ways to send the `:reload` and `:main` commands to the integrated terminal built into VSCode. I haven't figured out an elegant, built-in way to do this yet, though I do have a VSCode [plugin][ghci-helper]. This may be the topic of a future video.

### Next steps

Interpreted mode is faster that building, linking and running. In the next video, we'll talk about other tools that can help too:

* [usethetypes#4: Rapid development with ghcid][004-rapid-dev-ghcid]

[001-vscode-intero]: 001-vscode-intero
[002-your-first-snap-app]: 002-your-first-snap-app
[004-rapid-dev-ghcid]: 004-rapid-dev-ghcid
[edit-compile-link-run]: http://wiki.c2.com/?EditCompileLinkRun
[ghci]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html
[ghci-helper]: https://marketplace.visualstudio.com/items?itemName=rcook.ghci-helper
[index]: index.md
