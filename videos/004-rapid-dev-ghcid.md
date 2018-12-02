---
title: "usethetypes#4: Rapid Development with ghcid"
layout: video
categories: videos
youtube_id: c_BfMElBqcQ
episode_id: 4
slug: rapid-dev-ghcid
---
In the usethetypes#3, we used GHCi to run our Haskell program in interpreted mode. In this episode, we'll take a quick look at "ghcid", Neil Mitchell's awesome GHCi daemon, which makes type-checking and error-checking our programs even faster.

### Builds on

* [usethetypes#3: Rapid development with GHCi][003-rapid-dev-ghci]

### Introduction

We've seen GHCi. Now let's play with ghcid.

### Step 1: Starter app

```bash
# Clone our Snap app
cd $HOME/src
mkdir rapid-dev
cd rapid-dev
wget -O - https://github.com/usethetypes/usethetypes-003-rapid-dev-ghci/archive/master.tar.gz | tar xvz --strip-components=1

# Create a Git repo so we can track changes
git init
git add .

# Open up our editor
code .
```

We'll open up the integrated terminal with ``Ctrl+` `` and install ghcid using Stack:

```bash
stack build --copy-compiler-tool ghcid
```

We've seen `stack build` before but this is the first time we've seen `--copy-compiler-tool`. This switch tells Stack to install the `ghcid` executable in a directory specific to the version of GHC used by the current project. This prevents one project's use of ghcid from colliding with other projects. This step can be a bit time-consuming. We should, however, only have to wait through this once per version of GHC.

You can see the path it installs to in the output. This is a path that will be accessible using Stack's `exec` command. You can see a list of Stack's paths for the current project with `stack path`:

```bash
stack path
```

You'll see this path labelled as `compiler-tools-bin`. Let's run `ghcid`:

```bash
stack exec ghcid
```

Nice! ghcid takes over whichever terminal it's running in and provides live type checking. Try making some typos of symbol names in the editor and saving.

While Haskero/Intero provides type checking and squigglies etc., I find live program checking &agrave; la ghcid to be very nice.

We can also use the `-T` option to specify a GHCi command to run on successful compilation:

```bash
stack exec ghcid -- -T":main"
```

This can be problematic for a web server, where `Ctrl+C` is required to interrupt execution. However, it can be nice for functions that terminate:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Snap.Core (ifTop, route, writeText)
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = putStrLn "Hello world!"
```

I haven't yet figured out how to kill a running web server without killing GHCi. I will let you know when I work something out. I'm on a mission, so I will figure it out one day.

Our future development efforts will involve a combination of Haskero, Intero, GHCi and ghcid. I will mix and match. You should do the same to find out what works for you.

### Next steps

Next up:

* [usethetypes#5: Simple Site with Static Content][005-simple-site]

[003-rapid-dev-ghci]: 003-rapid-dev-ghci
[005-simple-site]: 005-simple-site
[ghcid]: https://github.com/ndmitchell/ghcid
[index]: index.md
