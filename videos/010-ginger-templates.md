---
title: "usethetypes#10: HTML Templating with Ginger"
layout: video
categories: videos
youtube_id: _JIfgf0y5_w
episode_id: 10
slug: ginger-templates
---
In this video, we'll learn how to generate dynamic HTML views in a structured way using the [Ginger][ginger] package.

### Builds on

* [usethetypes#7: Configuring the Web Server Port][007-config-port]
* [usethetypes#8: Hosting Your App on Heroku][008-heroku]
* [usethetypes#9: Clean-up of our Project][009-cleanup]

### Why Ginger?

There are many high-quality templating libraries for Haskell. Some you may encounter include:

* [Heist][heist]
* [blaze-html][blaze-html]
* [Shakespeare][shakespeare]

Some of them rely on lenses and other frameworks for their operation; others make extensive use of Template Haskell and various Haskell language extensions to allow the developer to build sophisticated type-safe templates. We may look into some of these in the future, but for the time being I'm going to focus on Ginger so I don't have to present a tutorial on lenses just to get some HTML generated. I'm a big fan of the [Jinja2][jinja2] framework for generating HTML in Python and so this package is a natural choice. It has a clean syntax and does not depend on any advanced Haskell language extensions or other sophisticated packages.

### Step 0: Grab project source code

```bash
cd $HOME/src
mkdir snap-hello-world
cd snap-hello-world
wget -O - https://github.com/usethetypes/usethetypes-009-cleanup/archive/master.tar.gz | tar xvz --strip-components=1
git init
git add .
script/dev
```

### Step 1: Create a Ginger template in the `views` directory

```bash
cat << EOF > views/_info.html
<!DOCTYPE html>
<html>
  <head>
    <title>{{ title }}</title>
  </head>
  <body>
    <h1>{{ title }}</h1>
    <p>Paragraph</p>
  </body>
</html>
EOF
```

This is regular HTML with Ginger tags, e.g. `{{ title }}`, that will be interpolated in the output.

### Step 2: Add a new `info` route

First start VSCode in the project directory

```bash
code .
```

And run ghicd in an integrated terminal:

```bash
script/ghcid
```

First change the import of `Snap.Core` to import everything from this module:

```haskell
import Snap.Core
```

We'll use `method` and `GET` (one of `Method`'s data constructors) imported from `Snap.Core` to specify an HTTP GET route and also import `sendFile` for use in the handler:

```haskell
  <|> route
      [ ("/info", method GET showInfo)
      , ("/static", serveDirectory "static")
      ]
```

And create `showInfo` handler:

```haskell
showInfo :: MonadSnap m => m ()
showInfo = sendFile "views/_info.html"
```

For the time being, this will send the raw template contents in response to `GET /info`, see `http://localhost:8000/info` in your browser.

### Step 3: Add Ginger and other dependencies

Now we're going to get down to the business of rendering the template by performing textual interpolation. What we're going to do is have the `showInfo` handler programmatically set the value of the `{{ title }}` placeholder in the template.

First, we need to add some dependencies including `ginger` to our `.cabal` file:

```cabal
executable snap-hello-world
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:
      base >= 4.7 && < 5
    , ginger
    , snap-core
    , snap-server
    , text
    , unordered-containers
```

We've added `ginger`, `text` and `unordered-containers`. `text` provides APIs for manipulating instances of the `Text` type while `unordered-containers` contains collections, such as `HashMap` etc., which we'll use with Ginger.

Let's try building with these new dependencies:

```bash
stack build
```

You'll see error spew telling us that Ginger is not available in Stackage. To fix this, we need to tell Stack which version of the package to pull from Hackage using an `extra-deps` section in `stack.yaml`:

```yaml
resolver: lts-12.0

packages:
- .

extra-deps:
- ginger-0.8.4.0
```

We should then check that everything builds now:

```bash
stack build
```

### Step 4: Create a new module

We're going write a couple of functions to handle rendering templates from a `HashMap` of key-value pairs. In order to avoid cluttering our `Main` module, we'll place this code in a new module which will name `App.Template`. To do this, we'll create an `App` subdirectory under `src` and an empty source file:

```bash
mkdir -p src/App
touch src/App/Template.hs
```

We'll also add this module to our `.cabal` file:

```cabal
  other-modules:
      App.Template
```

In `Template.hs` we'll create the skeleton for our template-rendering function:

```haskell
module App.Template
    ( renderTemplate
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Snap.Core (MonadSnap, sendFile)

type Context = HashMap Text Text

renderTemplate :: MonadSnap m => FilePath -> Context -> m ()
renderTemplate path _ = sendFile path
```

Points of note:

* We export the function `renderTemplate`
* We define our key-value pair type alias `Context` as `HashMap Text Text`
* We provide an implementation of `renderTemplate` based on our previous `showInfo` handler
* It currently ignores the context and simply send the file to the client as `showInfo` did previously

As you save the file, ghcid should repeatedly type-check it and re-run the `main` action.

We should then head back to `showInfo` and modify it to use our `renderTemplate` function:

```haskell
showInfo :: MonadSnap m => m ()
showInfo = renderTemplate "views/_info.html" $ HashMap.fromList
            [ ("title", "Info")
            ]
```

We'll also import `App.Template` and make sure everything is still building and running. At this point, it still yields the unexpanded template to the client.

### Step 5: Implement `renderTemplate`

Now, we'll provide a more meaningful implementation for `renderTemplate` using various Ginger functions including [`parseGingerFile`][parse-ginger-file] and [`easyRender`][easy-render] and the type alias [`IncludeResolver`][include-resolver]:

We can fire up GHCi to interrogate some of these symbols:

```bash
stack ghci
```

And:

```ghci
import Text.Ginger
:t parseGingerFile
:i IncludeResolver
:t easyRender
```

This leads to one possible implementation:

```haskell
renderTemplate :: MonadSnap m => FilePath -> Context -> m ()
renderTemplate path ctx = do
    result <- liftIO $ parseGingerFile resolve path
    case result of
        Left e -> do
            liftIO $ print e
            modifyResponse $ setResponseCode 500
        Right template -> do
            modifyResponse $ setHeader "Content-Type" "text/html"
            writeText $ easyRender ctx template
```

We'll need to add a pragma for the `OverloadedStrings` language extension and import the following modules:

* `Control.Monad.IO.Class`
* `System.IO.Error`
* `Text.Ginger`

So, `resolve` is our resolver which takes the source path and evaluates to an action that will retrieve `Just` the contents of the file, if it exists, or `Nothing` if it doesn't. `parseGingerFile` is an action that uses `Either` to returned a template parsed from the file or error. Finally, `easyRender` takes a context and the template and renders it as `Text`. In this implementation, we send this text to the client with the appropriate `Content-Type` header. In the error case, we yield a generic server error response.

Here's our `resolve` function:

```haskell
resolve :: FilePath -> IO (Maybe String)
resolve path = do
    mbContents <- tryIOError $ readFile path
    case mbContents of
        Right contents -> pure $ Just contents
        Left e -> print e >> pure Nothing
```

We can save all of those changes and test it out at `http://localhost:8000`. Voil&agrave! HTML!

Next time we'll do some more advanced things with Ginger.

### Next steps

* [usethetypes#11: More Ginger][011-more-ginger]

[007-config-port]: 007-config-port
[008-heroku]: 008-heroku
[009-cleanup]: 009-cleanup
[011-more-ginger]: 011-more-ginger
[blaze-html]: http://hackage.haskell.org/package/blaze-html
[easy-render]: http://hackage.haskell.org/package/ginger-0.8.4.0/docs/Text-Ginger-Run.html#v:easyRender
[ginger]: https://ginger.tobiasdammers.nl/
[heist]: http://hackage.haskell.org/package/heist
[include-resolver]: http://hackage.haskell.org/package/ginger-0.8.4.0/docs/Text-Ginger-Parse.html#t:IncludeResolver
[jinja2]: http://jinja.pocoo.org/
[parse-ginger-file]: http://hackage.haskell.org/package/ginger-0.8.4.0/docs/Text-Ginger-Parse.html#v:parseGingerFile
[shakespeare]: http://hackage.haskell.org/package/shakespeare
