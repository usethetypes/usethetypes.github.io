---
title: "usethetypes#1: VSCode and Intero"
layout: synopsis
youtube_id: TLMjeCN32eM
episode_id: "001"
slug: vscode-intero
---
In this video, we'll assume that you've already installed
[Haskell Stack][haskell-stack] and that now you'd like an editor and
some nice Haskell IDE integration. Currently, my preferred editor is
[Visual Studio Code][vscode]. In this video, I'll take you through
installing VSCode as well as [Haskero][haskero], a VSCode extension
which provides Haskell IDE features based on the [Intero][intero]
package.

### Install Intero using Haskell Stack

```bash
cd ~/src/hello-world
stack exec which intero
stack build intero
```

This may fail on a clean Ubuntu installation due to missing
external/native packages. Stack is a great tool for managing projects,
dependencies and sandboxes but does not currently take care of
installing system-wide native libraries for you.

In my case, we see a linker error referring to `-ltinfo`. This is
mentioned in [this][intero-issue] issue in Intero's GitHub issue
tracker. We need to install `libtinfo-dev`:

```bash
sudo apt-get install libtinfo-dev
stack build intero
stack exec which intero
```

Note that the equivalent packages on other Linux distributions as well
as macOS and Windows will have slightly different names.

[haskell-stack]: https://docs.haskellstack.org/en/stable/README/
[haskero]: https://marketplace.visualstudio.com/items?itemName=Vans.haskero
[intero]: https://github.com/commercialhaskell/intero
[intero-issue]: https://github.com/commercialhaskell/intero/issues/243
[vscode]: https://code.visualstudio.com/
