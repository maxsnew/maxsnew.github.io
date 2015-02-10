---
title: Libraries
---

[Racket](http://racket-lang.org)
========

- - - 

[Redex](http://redex.racket-lang.org/)
-------

For my master\'s research I work with [Robby
Findler](http://www.eecs.northwestern.edu/~robby/) on automated
test-case generation for the Redex formal semantics-engineering
dsl. My generators are isomorphisms from natural numbers to terms in a
redex grammar. The generators are defined automatically when you
define a language in Redex and have a more predictable distribution of
terms than random generators. A major update is rolling out with the
next release of Racket that includes support for all decidable Redex
grammars. Next we\'re looking at improving performance and generating
well-typed terms.

[Elm](http://elm-lang.org)
=====

- - -

Elm is a fairly young language, and I\'m working on testing its limits
to help guide future development.

[Lazy](http://github.com/maxsnew/Lazy)
------

I develop and maintain a library for Lazy evaluation and lazy data
structures in the Elm programming language. Elm is a pure strict
language and doesn\'t have built-in support for call-by-need semantics
so there are performance limitations. If Elm gets some sort of
native-extension mechanism then state can be used to reduce
recomputation.

[Generic](http://github.com/maxsnew/Generic)
---------

This is a library for experimenting with a value-level implementation
of ad-hoc polymorphism (aka overloading) akin to Haskell\'s type
classes. Elm\'s row types are powerful enough to express this in a
first order way with something resembling subtyping (e.g., every Monoid
is a Semigroup), but not powerful enough to express things that
require higher-kinded polymorphism (such as Monads). This library is
meant to push the limit of what\'s possible in Elm and help guide the
development of new language features.
