https://clojars.org/dv/tick-util

[![Clojars Project](https://img.shields.io/clojars/v/dv/tick-util.svg)](https://clojars.org/dv/tick-util)

Extracted from an app, contains some helpers for tick.

Build a deployable jar of this library:

    $ clojure -A:jar

Install it locally:

    $ clojure -A:install

Deploy it to Clojars -- needs `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` environment variables:

    $ clojure -A:deploy


# Deploy notes for clojars

1. Update the version of the maven package in pom.xml.
2. Build the jar
   make
3. CLOJARS_USERNAME='' CLOJARS_PASSWORD='deploy_token' clojure -A:deploy 
