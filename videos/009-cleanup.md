---
title: "usethetypes#9: Clean-up of our Project"
layout: video
categories: videos
youtube_id: OCKqypAMlW4
episode_id: 9
slug: cleanup
---
Before we go any further, we should take five minutes (or close enough) to tidy up our project. If you don't care about the indentation of your configuration files and inconsequential things like that, feel free to skip this video!

### Builds on

* [usethetypes#8: Hosting Your App on Heroku][008-heroku]

### Why?

As we develop our app, we will be adding more and more dependencies to it. This will be a little easier if we strip out some unused parts of our project and will stand us in good stead for the future.

### Step 0: Grab project source code

```bash
cd $HOME/src
mkdir snap-hello-world
cd snap-hello-world
wget -O - https://github.com/usethetypes/usethetypes-008-heroku/archive/master.tar.gz | tar xvz --strip-components=1
git init
git add .
```

### Step 1: Strip out noise from `stack.yaml`

We'll be making some changes to our Stack configuration in order to reference the certain additional packages in the future. This will be easier if we remove the noise from this file. If you open this file in VSCode you'll see that most of it is documentation in the form of comments such as:

* How to change the resolver
* How to reference packages in various different ways
* How to specify extra dependencies and special compiler flags
* Configuration settings for referencing local native libraries etc.

All of this information can be found at the Stack [documentation][stack-yaml-docs] site in the future if you need it. Let's remove all this for now leaving just the `resolver` and `packages` sections:

```yaml
resolver: lts-12.0

packages:
- .
```

### Step 2: Tidy up layout of `.cabal` file

I am what you might call _particular_ and I like to see things line up in my `.cabal` file. Therefore, I like to reflow the list of build dependencies. This will make it easier to add more dependencies in the future and looks more aesthetically pleasing (in my opinion!). Open up `snap-hello-world.cabal` file to make it easier when we start adding more dependencies:

```cabal
executable snap-hello-world
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:
      base >= 4.7 && < 5
    , snap-core
    , snap-server
```

I also like to line up all of my columns consistently which I'll do too.

### Step 3: Create some helper scripts

These scripts do not necessarily need to live directly in the project's repo but I find them handy to have lying around so I'm going to include them in the project until somebody convinces me that this is a silly idea:

Here's a little script that I typically run on newly checking out a project in order to initialize the IDE tools I've discussed in some of my earlier videos:

```bash
mkdir -p script
cat << EOF > script/dev
#!/bin/bash
stack build intero
stack build --copy-compiler-tool ghcid
EOF
chmod +x script/dev
```

And here's a script I use to run `ghcid` using Stack:

```bash
mkdir -p script
cat << EOF > script/ghcid
#!/bin/bash
stack exec ghcid -- -T':main'
EOF
chmod +x script/ghcid
```

And we're done!

[008-heroku]: 008-heroku
[stack-yaml-docs]: https://docs.haskellstack.org/en/stable/yaml_configuration/
