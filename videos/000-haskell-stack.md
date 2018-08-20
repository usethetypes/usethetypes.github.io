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

Full installation instructions for Haskell Stack are available on its
[web site][haskell-stack]. To view these, open up your web browser and visit `https://haskellstack.org/`. For Unix-like operating systems, there are two main commands described which are largely equivalent: one using `curl` and the second using `wget`. We'll use the `wget` variant since this command is most likely to be present on a clean installation of Ubuntu 18.04:

```bash
wget -qO- https://get.haskellstack.org/ | sh
```

The `wget` command will need to run some commands as `root` and so will prompt you for your password via `sudo`.

Once that has finished, you'll notice that the output references a `.local/bin` directory. This is a user-wide location where Stack will copy executables when using the `stack install` command. This is a useful location to have available at all times, so we should add this path to the `PATH` environment variable in your bash configuration file:

```bash
echo 'export PATH=$HOME/.local/bin:$PATH' >> ~/.bashrc
mkdir $HOME/.local/bin
```

Adjust this to taste.

### Step 2: Check Haskell Stack version

You should check the version of Stack you have installed:

```bash
stack --version
```

This video assumes that you have at least version 1.7.1.

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

### Next steps

* [usethetypes#1: VSCode and Intero][001-vscode-intero]

[001-vscode-intero]: 001-vscode-intero
[chrome]: https://chrome.google.com/
[haskell-stack]: https://docs.haskellstack.org/en/stable/README/
[ubuntu-desktop]: https://www.ubuntu.com/download/desktop
