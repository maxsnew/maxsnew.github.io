
Top --> Set as a type refinement system.

One thing that trips students up in an introductory point-set topology
class is the product topology.

Topology books usually present 2 constructions the "box" topology and
the true product topology.
When you're young and naive the box topology looks nicer but the
product topology is the one you want because it's an actual categorical
product.

The only reason, I think, for this preference is because of how
uncategorical your point-set topology book is, with a focus on
manipulating bases and sub-bases and on and on.

However if we instead view topology with a focus on the fibration Top
--> Set, then the construction of the product topology is part of a
more general pattern.

## Top --> Set fibration

One elementary fact you may learn about Point-Set Topologies is that
if you fix the underlying set, you get an ordering of all possible
topologies on the set.

At the top we have the least informative or coarsets topology, the
indiscrete topology, and at the bottom we have the most informative or
finest topology, the discrete topology.
This is the "fineness" ordering.

The most important aspect of this ordering is that it has all "meets"
or "greatest lower bounds".

For finite meets this means we have the top topology, coarser than
everything and for two topologies X, Y on a set A, we can construct
the "coarsest topology finer than X and Y".

...

then the product space of A and B is just

	pi_1^*(A) /\ pi_2^*(B)

which fits the characterization you eventually hear that it's the
"coarsest topology making the projections continuous".
