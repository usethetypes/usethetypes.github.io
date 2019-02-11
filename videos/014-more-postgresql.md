---
title: "usethetypes#14: More PostgreSQL"
layout: video
categories: videos
youtube_id: TBD
episode_id: 14
slug: more-postgresql
---
TBD

### Builds on

* [usethetypes#13: PostgreSQL Access][013-postgresql]

### Overview

TBD

### Step 0: Grab project source code

```bash
cd $HOME/src
mkdir snap-hello-world
cd snap-hello-world
wget -O - https://github.com/usethetypes/usethetypes-013-postgresql/archive/master.tar.gz | tar xvz --strip-components=1
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

[013-postgresql]: 013-postgresql
