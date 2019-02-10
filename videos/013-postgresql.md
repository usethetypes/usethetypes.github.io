---
title: "usethetypes#13: PostgreSQL Access"
layout: video
categories: videos
youtube_id: TBD
episode_id: 13
slug: postgresql
---
TBD



=================

#### Run app locally

Well, we already know how to do this:

```bash
stack exec snap-hello-world
```

Or we can let ghcid run it for us. In fact, this is exactly how we'll run the app most of the time. There is also the Heroku `local` command which can do to run the app "more like Heroku would". I'm not quite sure what the implications of this are right now, but I'd like to try it anyway:

```bash
heroku local
```

However, it doesn't work by default, even if you've built the project. This is because it cannot find the `snap-hello-world` executable. In fact, Heroku typically will expect an absolute path in the `Procfile` or will resolve it relative to the system search path. My current workaround of choice is to create another helper script which sets up the `PATH` beforehand:

```bash
cat << EOF > script/heroku-local
#!/bin/bash
PATH=\$(stack path --local-install-root)/bin:\$PATH \\
  heroku local
EOF
chmod +x script/heroku-local
```

This is a simulation of how Heroku will run the app. It will be available on port 5000. You can check out your app at `http://localhost:5000/info`.
=================


### Builds on

* [usethetypes#12: Displaying our Application's Environment][012-app-environment]

### Overview

TBD

### Step 0: Grab project source code

```bash
cd $HOME/src
mkdir snap-hello-world
cd snap-hello-world
wget -O - https://github.com/usethetypes/usethetypes-012-app-environment/archive/master.tar.gz | tar xvz --strip-components=1
git init
git add .
script/dev
```

### Step 1: Go to your Heroku dashboard

https://dashboard.heroku.com/apps/fast-lake-66895/

### Step 2: Configure PostgreSQL add-on

1. Click on _Configure Add-ons_.
2. Enter _postgres_ in the search box
3. Select _Heroku Postgres_
4. Choose _Hobby Dev&mdash;Free_
5. Click _Provision_

You'll see another environment variable `DATABASE_URL` if you refresh the `info` view in your browser. This is how Heroku communicates database credentials to your app. This is how we'll interact with our database.

### Step 3: Interact with database

We'll create a simple Haskell script to manipulate this database. We'll pass in the same PostgreSQL database URL via the `DATABASE_URL` environment variable. Let's create this script first:

```bash
touch script/DBTest.hs
```

Here's the content:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module DBTest (main) where

import qualified Data.ByteString.Char8 as Char8 (pack)
import Database.PostgreSQL.Simple
    ( Only(..)
    , close
    , connectPostgreSQL
    , query_
    )
import System.Environment (getEnv)

main :: IO ()
main = do
    dbUrl <- getEnv "DATABASE_URL"
    conn <- connectPostgreSQL (Char8.pack dbUrl)
    [Only result] <- query_ conn "SELECT 'Hello' || ' ' || 'world'"
    print (result :: String)
    close conn
```

Notes:

* `connectPostgreSQL` opens a database connection described by the passed-in URL or connection string
* `query_` runs a SQL query which does not take any variable substitutions in the query string
* `close` closes the database connection

Next, we'll create an `.env` file containing the the database URL.

```bash
echo export DATABASE_URL=$(heroku config:get DATABASE_URL) > .env
echo /.env >> .gitignore
```

We'll also create a shell script to run our scripts with environment variables sourced from this file, `run-hs`:

```bash
#!/bin/bash
set -euo pipefail

this_dir=$(cd $(dirname $0); pwd -P)

source $this_dir/../.env
stack runghc -- -i$this_dir/../src $*
```

We can then run our Haskell script as follows:

 should then be able run the script:

```bash
script/run-hs script/DBTest.hs
```

Well, that's nice! Note that the `.env` file will allow us to use a local PostgreSQL database in the future just by changing the `DATABASE_URL` environment variable to point at the appropriate PostgreSQL instance.

Note that the `DBSetup.hs` script might not work if you do not have libpq installed. This might result in errors such as the following:

```
Configuring postgresql-libpq-0.9.4.1...
    setup: The program 'pg_config' is required but it could not be found.
```

To install the required packages you'll need to the following or equivalent for your platform:

```bash
sudo apt update
sudo apt install libpq-dev
```

### Step 4: A note about Haskell scripting

* Why did we create the shell script?
* Why did we not use Haskell Stack scripting?

I'm a big fan of [scripting][stack-scripting] with Haskell Stack. Something I have not figured out how to do is to reference scripts outside the current directory. Using `stack runghc` we can pass the `-i` option to reference source files in the `src` directory. This will be useful in the future when we'd like to share Haskell sources between our app and our app's scripts.

### Next steps

* [usethetypes#14: More PostgreSQL][014-more-postgresql]

[012-app-environment]: 012-app-environment
[014-more-postgresql]: 014-more-postgresql
[stack-scripting]: https://haskell-lang.org/tutorial/stack-script
