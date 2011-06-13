
Swizzle
=======

Find all the words that can be made from the given set of letters (including the original word which was shuffled).

Running
-------

    $ bundle
    $ bundle exec rackup

Hax
---

The puzzles are generated from the dictionary in `word-generation/words`. That part is in haskell, and can be built and run like so:

    $ cd word-generation
    $ cabal configure && cabal build
    $ ./dist/build/equivocating/equivocating

The sinatra app is `swizzle.rb`, and doesn't really do much but serve up puzzles and assets. The game logic is all in `public/game.js`.
