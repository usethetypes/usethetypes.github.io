---
title: "usethetypes#8: Hosting Your App on Heroku"
layout: video
categories: videos
youtube_id: S1a_nhVjeCw
episode_id: 8
slug: heroku
---
In this video, we'll go through the steps required to get your Haskell web application up and running on the Internet using [Heroku][heroku].

### Builds on

* [usethetypes#7: Configuring the Web Server Port][007-config-port]

### Step 1: Sign up for an Heroku account

Before you go any further, you'll need to sign up for a free account with Heroku via the [sign-up][heroku-sign-up] page. This should be fairly straightforward. It will involve an e-mail address verification step. You'll also need to create a password which you should remember for later. Once you've done this and are able to view your Heroku dashboard, you can proceed to the next step.

### Step 2: Install the Heroku CLI

I am a command-line sort of a person and so I will do most things using the [Heroku CLI][heroku-cli]. Before we can go any further, we'll need to install it. There are several ways to install the CLI depending on which platform you're running on:

* macOS: Via the [installer][heroku-macos-installer] or using [Homebrew][homebrew]
* Windows: Via the [installer][heroku-windows-installer]
* Linux: Via [Snap][snap], standalone [installer][heroku-linux-installer], [tarball][heroku-tarball] or package manager

I'm running a fairly recent version of Ubuntu which includes Snap by default, so I'm going to use this since it's probably the easiest way to get up and running (and I'm _opinionated_):

```bash
sudo snap install --classic heroku
```

Whichever method you choose, once that's done, you should be able to open a terminal and check the version you're running:

```bash
heroku --version
```

This reports `heroku/7.19.4 linux-x64 node-v11.3.0` for me. You should get something similar.

### Step 3: Log in to Heroku from the command line

```bash
heroku login
```

This will start a browser-based authentication session. Make sure that the browser logs you in as the correct user.

### Step 4: Runs some CLI commands

You can list available commands with the `help` command:

```bash
heroku help
```

You'll probably have no apps available at this point:

```bash
heroku apps
```

### Step 5: Grab our web app

Now we're going to get our little Haskell app's source code and create a new repo:

```bash
cd $HOME/src
mkdir snap-hello-world
cd snap-hello-world
wget -O - https://github.com/usethetypes/usethetypes-007-config-port/archive/master.tar.gz | tar xvz --strip-components=1
git init
git add .
```

Let's commit these changes:

```bash
git commit -m "Initial commit"
```

### Step 6: Create an Heroku app

To do this we'll use the Heroku `create` command:

```bash
heroku create --help
```

Running `create` with no additional arguments will create a new app using the default Heroku stack which is, I believe, Ruby on Rails. We'll make use of the `--buildpack` option to indicate a specific type of application. We'll use the `heroku-buildpack-stack` build pack which is available on [GitHub][heroku-buildpack-stack] if you're interested in the details of how this works. Let's create a new app with this build pack and default values for everything else:


```bash
heroku create --buildpack https://github.com/mfine/heroku-buildpack-stack
```

This generates a brand-new Heroku app with a guaranteed-unique name and a corresponding Git repo. Make a note of the output of this command for later use. This will include the following:

* A URL at which you can reach the app, e.g. `https://stark-bastion-84125.herokuapp.com/`
* A Git URL which is the source code repo to which we will deploy our source code, e.g. `https://git.heroku.com/stark-bastion-84125.git`

You can list your apps again now:

```bash
heroku apps
```

Or display information specific to this app:

```bash
heroku info stark-bastion-84125
```

A side effect of the `create` command is that the CLI should've set up your Git remotes to point to the new deployment repo:

```bash
git remote -v
```

This should show _fetch_ and _push_ remotes corresponding to your new app.

### Step 7: Create a `Procfile`

Before you can deploy, you need to tell Heroku about your app's executable:

```bash
echo web: snap-hello-world > Procfile
```

Confirm that your `Procfile` was created correctly:

```bash
cat Procfile
```

This should yield something like the following:

```yaml
web: snap-hello-world
```

Add the `Procfile` and commit it to your repo and we're almost ready to go:

```bash
git add .
git commit -m "Heroku Procfile"
```

### Step 8: Build and deploy

To push your source code and built it, use the following:

```bash
git push heroku master
```

This will do several things:

* It will push the source code to the `master` branch on the Heroku remote
* It will ensure the correct version of GHC is available
* It will build your package and its dependencies

This might be quite time-consuming the first time you run it. Subsequent pushes should be much quicker since Heroku will cache build artifacts between invocations.

### Step 9: Visit your new web site!

Visit your app's URL in your browser: `https://stark-bastion-84125.herokuapp.com/`. Assuming that all works, you should then share it with your friends!

[007-config-port]: 007-config-port
[heroku]: https://www.heroku.com/
[heroku-buildpack-stack]: https://github.com/mfine/heroku-buildpack-stack
[heroku-cli]: https://devcenter.heroku.com/articles/heroku-cli
[heroku-linux-installer]: https://cli-assets.heroku.com/install.sh
[heroku-macos-installer]: https://cli-assets.heroku.com/heroku.pkg
[heroku-sign-up]: https://signup.heroku.com/
[heroku-tarball]: https://cli-assets.heroku.com/heroku-linux-x64.tar.gz
[heroku-windows-installer]: https://cli-assets.heroku.com/heroku-x64.exe
[homebrew]: https://brew.sh/
[snap]: https://snapcraft.io/
