---
title: Monad Tutorial
date: Some day...
---

# A Formal Monad Tutorial

Why?

What's a Monad?

## Monads in Certain 2-categories

in CAT: a traditional monad
monad in Rel: a reflexive, transitive relation
comonad in Rel: a subset
monad in Span : a small category
comonad in Span? 
monad in a 1-object Bicategory: a monoid!
comonad in a 1-object Bicategory: a comonoid?
monad/comonad in a locally chaotic Bicategory: a morphism

comonad in a programming language...

Programming language as a 2-category

f => g if \/K. if K[f] = _|_ then K[g] = _|_

usually denoted g \sub f. The idea is that f and g are the "same"
except g has more errors.
So they only disagree on g's errors.

Then what's a monad in a programming language?

eta : *1 => m* (wherever m is not an error it's the identity)
mu  : *mm => m* (running m twice has less errors than m)

note that since our category is locally a poset, we actually get *mm =
m*.

So *m : A -> A* is idempotent (running it twice is the same as once)
and at most adds errors to the identity.

Such a program could be called a *contract* 

