---
title: "usethetypes#1: VSCode and Intero"
layout: video
categories: videos
youtube_id: TLMjeCN32eM
episode_id: 1
slug: vscode-intero
---
In this video, we'll assume that you've already installed
[Haskell Stack][haskell-stack] and that now you'd like an editor and
some nice Haskell IDE integration. Currently, my preferred editor is
[Visual Studio Code][vscode]. In this video, I'll take you through
installing VSCode as well as [Haskero][haskero], a VSCode extension
which provides Haskell IDE features based on the [Intero][intero]
package.

### Builds on

* [usethetypes#0: Haskell Stack][000-haskell-stack]

### Step 1: Install Intero using Haskell Stack

```bash
cd ~/src/hello-world
stack exec which intero
stack build --copy-compiler-tool intero
```

This may fail on a clean Ubuntu installation due to missing
external/native packages. Stack is a great tool for managing projects,
dependencies and sandboxes but does not currently take care of
installing system-wide native libraries for you.

In my case, we see a linker error referring to `-ltinfo`. This is
mentioned in [this][intero-issue] issue in Intero's GitHub issue
tracker. We need to install `libtinfo-dev`. Note that the equivalent packages on other Linux distributions as well
as macOS and Windows will have slightly different names:

```bash
sudo apt-get install libtinfo-dev
stack build --copy-compiler-tool intero
stack exec which intero
```

### Step 2: Install Haskero extension in VSCode

From your project directory, restart VSCode:

```bash
cd ~/src/hello-world
code .
```

From the Extensions panel, search for "Haskero", click on "Haskero: A full featured Haskell IDE" and click "Install". You may need to restart VSCode at some point.

### Step 3: Use Haskero features

With Haskero, you can now right-click on Haskell symbols. Open up `src/Main.hs` and right-click on `putStrLn`, for example.

In addition to the standard context menu commands, Haskero adds the following:

* "Go to Definition"
* "Peek Definition"
* "Find All References"
* "Rename Symbol"

It also provides red squigglies or underlines and tooltips with compiler errors as well as program issues in the "Problems" tab.

### Next steps

* [usethetypes#2: Your First Snap App][002-your-first-snap-app]

[000-haskell-stack]: 000-haskell-stack
[002-your-first-snap-app]: 002-your-first-snap-app
[haskell-stack]: https://docs.haskellstack.org/en/stable/README/
[haskero]: https://marketplace.visualstudio.com/items?itemName=Vans.haskero
[intero]: https://github.com/commercialhaskell/intero
[intero-issue]: https://github.com/commercialhaskell/intero/issues/243
[vscode]: https://code.visualstudio.com/
