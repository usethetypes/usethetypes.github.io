---
title: "usethetypes#0: Haskell Stack"
layout: video
categories: videos
youtube_id: dN1M8ql1vPQ
episode_id: 0
slug: haskell-stack
---
In this episode of usethetypes, I'll show you how to install
[Haskell Stack][haskell-stack] from scratch on a clean Linux development
machine. Stack is the Haskell build tool I will use for many of my other
videos and so this setup step will be an important prerequisite for most
future tutorials. After following this video, you'll be able to build
and run a simple "Hello World" program written in Haskell. You'll also
be set up to get stuck into my future videos.

### Notes

* Video shows [Ubuntu 18.04 LTS Desktop][ubuntu-desktop]
* I haven't done much to the operating system except install updates,
clean up the desktop and install my [favourite][chrome] web browser

### Step 1: Install Haskell Stack

```bash
wget -qO- https://get.haskellstack.org/ | sh
echo 'export PATH=$HOME/.local/bin:$PATH' >> ~/.bashrc
mkdir $HOME/.local/bin
```

### Step 2: Check Haskell Stack version

```bash
stack --version
```

### Step 3: Create a "Hello World" project

```bash
mkdir src
cd src
stack new hello-world simple --resolver=lts-12.0
cd hello-world
git init
echo /.stack-work/ >> .gitignore
stack build --fast
stack exec hello-world
cat src/Main.hs
```

[chrome]: https://chrome.google.com/
[haskell-stack]: https://docs.haskellstack.org/en/stable/README/
[ubuntu-desktop]: https://www.ubuntu.com/download/desktop
