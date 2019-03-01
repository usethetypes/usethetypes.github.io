---
title: "usethetypes#11: More Ginger"
layout: video
categories: videos
youtube_id: VMxj3-UIBas
episode_id: 11
slug: more-ginger
---
We looked at Ginger for basic string interpolation. Now we'll look at more advanced stuff including operating on lists.

### Builds on

* [usethetypes#10: HTML Templating with Ginger][010-ginger-templates]

### Overview

In the [previous episode][010-ginger-templates], I showed you how to get the basic plumbing for Ginger up and running. In order to render a template, Ginger requires a _context_ which it uses to look up the values of names embedded in the template. That video demonstrated one of the simplest possible contexts: a hash map of string key-value pairs. In general, our contexts will be more complicated than this and we will need some mechanism of providing values other than strings to our templates.

Accordingly, Ginger provides a variant type, [`GVal`][gval], for just this purpose. We will modify our project to use this type which will allow us to pass both scalar strings and lists of strings to `easyRender`. For more detail about how I figured out how to do this, please consult my [blog post][blog-post].

We will throw away our `HashMap` definition for `Context` and replace it with a definition in terms of `GVal`:

```haskell
type Context = GVal (Run SourcePos (Writer Text) Text)
```

This is a concrete type created by applying `GVal`'s type constructor to various other bits and pieces. `SourcePos` is a type used to define how `GVal` handles source positions, `Writer Text` is the type used to accumulate output, while `Text` is the underlying string type. Please consult/decipher the documentation for `GVal` for more information.

The interesting thing about this change is that we will not need to touch the definition of `renderTemplate` once we have fixed up all the dependencies and imports as appropriate.

### Step 0: Grab project source code

```bash
cd $HOME/src
mkdir snap-hello-world
cd snap-hello-world
wget -O - https://github.com/usethetypes/usethetypes-010-ginger-templates/archive/master.tar.gz | tar xvz --strip-components=1
git init
git add .
script/dev
```

Once that's done, then fire up VSCode.

### Step 1: Add `transformers` dependency

Before we make that change, we'll add `transformers` to `snap-hello-world.cabal`. Since `Main.hs` and `App/Template.hs` intimately depend on one another, it will be easier to use GHCi instead of ghcid to incrementally build one or the other:

```bash
stack ghci
```

### Step 2: Add new imports

Add an import for `Control.Monad.Trans.Writer` in `App/Template.hs`:

```haskell
import Control.Monad.Trans.Writer
```

### Step 3: Modify definition of `Context`

Modify the definition of `Context` in `App/Template.hs`:

```haskell
type Context = GVal (Run SourcePos (Writer Text) Text)
```

You can reload this definition in GHCi:

```ghci
:reload App.Template
```

Notice how `App/Template.hs` compiles without error. `renderTemplate` does not need any changes at all to work with the new `GVal`-based `Context` type.

### Step 4: Remove unused imports

We definitely have at least one redundant import. We can have GHCi enumerate any such imports for use by inserting the following pragma at the top of our source file:

```haskell
{-# OPTIONS_GHC -Wunused-imports #-}
```

Then reload in GHCi:

```ghci
:reload App.Template
```

### Step 5: Fix up `Main.hs` using `dict`

If we reload the whole project, we'll see what else needs to be fixed up:

```ghci
:reload
```

At this point, running in ghcid will be helpful:

```bash
script/ghcid
```

This tells us that the existing `HashMap` being passed in `showInfo` is incompatible with the new `GVal`-based `Context`.

We can update `showInfo` to construct a `GVal` dictionary using the [`dict`][dict] helper function from `Text.Ginger`:

```haskell
import Text.Ginger
```

Change `HashMap.fromList` to `dict`:

```haskell
showInfo :: MonadSnap m => m ()
showInfo =
    renderTemplate "views/_info.html" $ dict
        [ ("title", "Info")
        ]
```

And it still works: see `http://localhost:8000/info`.

### Step 6: Use the [`~>`][make-pair] operator

We can use the "tilde arrow" or what I call the ["make pair"][make-pair] operator) from `Text.Ginger` to build other types of value:

```haskell
import Data.Text (Text)
```

and

```haskell
showInfo :: MonadSnap m => m ()
showInfo =
    renderTemplate "views/_info.html" $ dict
        [ ("title" ~> "Info"))
        , ("items" ~> ["line0" :: Text, "line1", "line2"])
        ]
```

### Step 7: Update the view template to use the `items` list

Add the following to our template:

{% raw %}
```haskell
    {% for item in items %}
      <p>{{ item }}</p>
    {% endfor %}
```
{% endraw %}

### Step 8: Check it out in your browser

Here you go: `http://localhost:8000/info`

### Step 9: Clean-up

You can then remove any references from `Data.HashMap` from your `.hs` files and also remove `unordered-containers` from `snap-hello-world.cabal`.

[010-ginger-templates]: 010-ginger-templates
[blog-post]: https://blog.rcook.org/blog/2019/ginger-examples/
[dict]: http://hackage.haskell.org/package/ginger-0.8.4.0/docs/Text-Ginger-GVal.html#v:dict
[gval]: http://hackage.haskell.org/package/ginger-0.8.4.0/docs/Text-Ginger-GVal.html#t:GVal
[make-pair]: http://hackage.haskell.org/package/ginger-0.8.4.0/docs/Text-Ginger-GVal.html#v:-126--62-
