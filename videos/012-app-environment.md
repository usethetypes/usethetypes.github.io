---
title: "usethetypes#12: Displaying our Application's Environment"
layout: video
categories: videos
youtube_id: 0b76WYcF7xk
episode_id: 12
slug: app-environment
---
We can now serve static content and generate dynamic content in our application. We're now building up to adding some real functionality to our app. Typically web apps will store some state somewhere. This is done to persist information and state across page loads and so on. We're going to follow the very common approach of storing state in a relational database. But first, we need to investigate how Heroku communicates its configuration settings to our app.

### Builds on

* [usethetypes#11: More Ginger][011-more-ginger]

### Overview

Doing anything nontrivial will require us to pass around some configuration. Heroku passes app configuration, including relational database settings, via environment variables. Eventually, we'll enable a PostgreSQL database Dyno in Heroku. Before then, we need to look at how to access environment variables in our app.

### Step 0: Grab project source code and start VSCode

```bash
cd $HOME/src
mkdir snap-hello-world
cd snap-hello-world
wget -O - https://github.com/usethetypes/usethetypes-011-more-ginger/archive/master.tar.gz | tar xvz --strip-components=1
git init
git add .
script/dev
code .
```

### Step 1: Modify `showInfo` handler

After opening the integrated terminal and running `script/ghcid` we can get started.

First we'll add some imports:

* Add `MonadIO(..)` from `Control.Monad.IO.Class` to our import list
* Add `getEnvironment` to your `System.Environment` import list

This will give us access to the `liftIO` function which will allow us to invoke `IO` actions from within our Snap handlers:

* Add lifted call to `getEnvironment` to read all environment variables
* Pass the environment variables to `renderTemplate`

Snap handlers use the `MonadSnap` type class which is a specialized context that includes `MonadIO` and others. `liftIO` allows us to call `IO` functions in this context.

### Step 2: Generate a table of environment variables in `_info.html`

We will do the following:

* Correct a few `{{ title }}` tags to correctly escape the HTML with the `escape` filter
* Add a table
* Use a `for` statement to loop over `envVars`
* Add some CSS

### Step 3: View the results

Once that's saved, we can then visit the local web site at `http://localhost:8000/info` to see our environment variables.

### Step 4: Publish to Heroku

First check in the changes:

```bash
git add .
git commit -m "Show environment variables"
```

This is a newly-created Git repo, so does not currently have an Heroku app associated with it. The `config` command demonstrates this:

```bash
heroku config
```

This will show the name of your Heroku app, if configured, along with any custom environment variables defined for it. If your repo is not associated with an Heroku app, you can list your Heroku apps as follows:

```bash
heroku apps
```

If you have not already authenticated with Heroku, you'll need to open up a web browser as instructed by the CLI tool and enter your credentials.

If you haven't already created an Heroku app you can do so as follows:

```bash
heroku apps:create --buildpack https://github.com/mfine/heroku-buildpack-stack
```

This will generate a random name and automatically set up your Git remote. You can then associate the app with your repo as follows:

```bash
heroku git:remote -a secure-woodland-62401
git remote -v
```

Once you've associated the Git repo with your app, you can publish as follows:

```bash
git push heroku master
```

That might take a little time (especially on the first publish). Once that's done, check out the app at your subdomain under `herokuapp.com`. If you forget the domain name, you can view this as follows:

```bash
heroku domains
```

Check out those beautiful environment variables!

[011-more-ginger]: 011-more-ginger
