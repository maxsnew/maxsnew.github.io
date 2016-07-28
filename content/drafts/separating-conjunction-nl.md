---
title: Separating Conjunction in Natural Language
date: soon
---

# Separating Conjunction in Natural Language

When I tell non-computer scientists I work on semantics of programming
languages, they often ask if its similar to studying natural language.

Usually my answer is that they are related but there are some key
differences.
For example, ambiguity is ubiquitous in natural language, but in
programming languages we almost always want our programs to be
unambiguous so we know what the computer will do when we run them!

However I think there are interesting connections between the topics,
but few working researchers bridge this gap.

One notable exception from our side is Chung-Chieh Shan, who has
written extensively about the presence of what we would call "control
effects" or "continuation effects" in natural language.
The very overly simplistified idea is that a phrase in a sentence can
depend on its context in complicated ways.
Shan has written a book with a linguist Chris Barker called
*Continuations and Natural Language* which I haven't read but is
certainly more coherent than what I have to say.

Inspired by Shan, I keep an eye out for natural language concepts that
correspond to programming language concepts and I remember one time in
particular that stood out to me where a natural language concept
corresponded to a topic seen as a bit difficult to understand by some:
Separation Logic.

Recently I was in Philadelphia's Reading Terminal Market (excellent
market, make sure to get the Amish donuts).
It's a big indoor market with lots of butchers, farmers and great
food.
I came to a vendor that was selling vegetables and artisanal cheese
and saw an interesting sign:

	Raw and Pasteurized Cheese

At first I laughed at the sign because it seemed like a contradiction,
how could you have a cheese that was *both* raw and pasteurized?
But then I realized that I was overly Cartesian in my analysis and
that the phrase made perfect sense, if we have a more refined
interpretation of *and*.

## Separation Logic

Separation Logic is a program logic for heap-manipulating programs,
its sort of like Hoare Logic meets Linear Logic.

Like in Hoare Logic you have a language of imperative programs

	c,d,e : W -> W

that manipulate the "world", that is, the heap.

Then you prove correctness of your program by constructing a
derivation of a triple:

	{P} c {Q}

where P and Q are propositions of separation logic, which are
*predicates* on the world *W*.
We'll be concerned with this language of propositions.
We'll give a presentation where we give meaning to a proposition *P*
by defining the subset of heaps that satisfy it:

	h \in P

means that the heap *h* satisfies the separation logic proposition
*P*.
Then we can define some traditional connectives such as and, or, true
and false and they correspond to the obvious set theoretic operations:

	h \in true   := true
	h \in P /\ Q := h \in P and h \in Q
	h \in P \/ Q := h \in P or h \in Q
	h \in false  := false

And so on, and we'll also has quantifiers like \exists and probably
some primitive type of pointer.

But the insight of separation logic is that there are other
connectives that don't fit into "traditional" logic that are very
useful for reasoning about imperative programs.

The reason is that 
