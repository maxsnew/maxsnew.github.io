---
title: Paper Requirement
---

# Secure Compilation via Mediated Universal Embedding

[Fully Abstract Compilation via Universal Embedding, ICFP 2016](fabcc-paper.pdf)

Max S. New, William J. Bowman, and Amal Ahmed

Acceptance rate: 31%

Advisor’s statement:

This is a paper about secure compilation, which ensures that when compiled components are linked with target code (attackers), the latter can make no more observations about the compiled component than a source-level attacker can about the original component.  Building secure compilers is hard because target languages have features that source languages don't, which allows target attackers to make observations that source attackers can’t.  *Proving* that a compiler is secure is even harder because it requires showing that any target code you can link with will behave like some source code — i.e., that it’s “back-translatable” into the source.

This paper studies the closure-conversion pass of a compiler, using static interfaces at the target-level to ensure that compiled code is only linked with well-behaved target attackers.  The more significant contribution is a new proof technique, called “back-translation by universal embedding", that works for non-terminating languages, target features that are untypeable in the source, and control effects even when the source has no control effects.

The universal embedding was entirely Max’s idea and is a major step forward in building verified secure compilers for *realistic* languages.

Max formalized the back-translation and the multi-language, which allows the compiler’s source and target language to interoperate, and he did almost all of the proofs.  He also did the bulk of the writing.  Specifically, sections 4 and 5, which contain the heart of the contribution, as well as the excellent discussion in section 7 are entirely his, as are large chunks of the Intro and Related Work sections. (William helped with proofs in an earlier version of the paper but the back-translation technique that we were using then did not pan out.  William also wrote a first draft of the intro for the earlier version, but Max has since made significant revisions to the intro.)
