Smucket
=======

Introduction
-------

I don't like browser bookmarks, and delicious has flaws that I can't address because I can't touch the source. This is my attempt to scratch my cross-platform bookmarking itch.

I drag .webloc files from browsers on Mac OS X onto my desktop. Smucket parses these new files and attempts to extract and save their content in a mongodb database.

Installation
-------

1. Download source
2. Install [leiningen](https://github.com/technomancy/leiningen) or cake
3. Run `lein deps` in the project root
4. Run `lein uberjar`

Usage
-------

The following command will start smucket. You may wish to run it in a screen.
    
    `java -jar <name of the STANDALONE jar generated>`

Todo
-------

* Sync with delicious
* Alternate entry methods
* More filetypes
* Better analysis (perhaps user-defined "important words" to group related pages.)
* Improve my implementation of the readability algorithm


License
-------

Copyright 2010, Andrew Moreland.

Distributed under the GPLv3 License
