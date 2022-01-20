---
title: Category Theory
---

# EECS 598: Category Theory for Computer Scientists
- Lecture: Tuesday & Thursday, 9:30-11:30, 2153 G.G. Brown Laboratories
- Instructor: Max S. New
- Office: Beyster 4640
- uniqname: maxsnew
- [Office Hours][q]: Monday & Wednesday 4-5:30pm, remote
- [Canvas][canvas]
- [Piazza][piazza]

This graduate-level course is an introduction to category theory for
computer scientists. Category theory is a branch of algebra that
studies structures abstracting function composition. This field of
abstract mathematics has found many uses in the theory of programming
languages, especially typed functional programming and design and
implementation of proof assistants. Our goal in this course is to get
a basic understanding of category theory sufficient to understand
applications in the theory of programming languages.

## Course Materials
 
The primary text is Steve Awodey’s Category Theory, 2nd Edition.
 
We will also use as supplementary material, the freely available texts
[Category Theory in Context by Emily Riehl][ctc] and [Categorical
Logic from a Categorical Point of View by Michael Shulman][clcpov].

## Office Hours

Office hours will be held twice weekly, on Mondays and Wednesdays from
4-5:30pm. For the time being, these will be remote office hours. You
should use the [queuing system][q] to access office hours.

## Evaluation

Your grade in the course will primarily be based on the homework
assignments, given out every 1-2 weeks. Homeworks will unless
otherwise stated be problem sets to be completed either handwritten or
in pdf format, preferably using LaTeX. You will be graded on
correctness and clarity of your work. Homeworks can be done in groups
of two. You should not discuss your homework with anyone besides your
partner and the instructor.

*Late homeworks will not be accepted*. If you need an extension talk
to the professor. However the lowest two homework grades will be
dropped.

The next major portion of your grade will be an in-class presentation
of homework solutions at the end of certain lectures. These will start
once the first homework is due. At the end of each class one or more
students will present a solution from the previous assignment to the
class.

Lectures will be recorded using lecture capture, available to be
accessed on Canvas. I encourage you to attend class in person if
possible. You will get much more out of the course if you attend and
ask questions in class.

The precise breakdown of grading is as follows

| Activity               | Percentage of final grade |
|:----------------------:|:-------------------------:|
| Problem Sets           | 80                        |
| In-class presentation  | 20                        |

## Schedule of Topics

Below is a tentative schedule of topics and associated readings for
the semester. Besides the first lecture, you are expected to read the
assigned readings *prior to* lecture.

| Meeting Date | Topic                                                 | Assigned Readings                                 | HW                                   |
|:------------:|:-----------------------------------------------------:|:-------------------------------------------------:|--------------------------------------|
| Thu, Jan 6   | Course Overview, what is Category theory?             | Awodey Ch 1.1-1.3, Riehl Examples 1.1.3 and 1.1.4 |                                      |
| Tue, Jan 11  | More Examples of Categories                           | Awodey Ch 1.4, Riehl Examples 1.3.2               |                                      |
| Thu, Jan 13  | Isomorphisms and Constructing Categories              | Awodey Ch 1.5-1.6                                 | [PS 1 out][ps1], [PS1 LaTeX][latex1] |
| Tue, Jan 18  | Free Categories as Syntax                             | Awodey Ch 1.7, Shulman Ch. 0, Ch. 1.1-1.2         |                                      |
| Thu, Jan 20  | Epimorphisms/monomorphisms, Initial/Terminal          | Awodey Ch 2.1-2.3                                 | [PS 2 out][ps2], [PS2 LaTeX][latex2] |
| Tue, Jan 25  | Initial/Terminal Objects, Products                    | Awodey Ch 2.4-2.7,                                |                                      |
| Thu, Jan 27  | Duality                                               | Awodey Ch 3                                       | PS 3 out                             |
| Tue Feb 1    | Syntax for Free Categories with X                     | Shulman Ch 1.4-1.6                                |                                      |
| Thu, Feb 3   | Pullbacks                                             | Awodey Ch 5.1-3                                   | PS 4 out                             |
| Tue Feb 8    | Limits and Colimits                                   | Awodey Ch 5.4-5.6                                 |                                      |
| Thu Feb 10   | Exponentials                                          | Awodey Ch 6.1-6.2                                 | PS 5 out                             |
| Thu Feb 10   | Cartesian Closed Categories in CS                     | Awodey Ch 6.3-6.6                                 |                                      |
| Tue Feb 15   | Cartesian Multicategories                             | Shulman Ch 2                                      | PS 6 out                             |
| Thu Feb 17   | Category of Categories                                | Awodey Ch 7.1-7.2                                 |                                      |
| Tue Feb 22   | Natural Transformations                               | Awodey Ch 7.4-7.5                                 |                                      |
| Thu Feb 24   | Functor Categories, Monoidal Categories               | Awodey Ch 7.7-7.8                                 |                                      |
| Tue Mar 8    | Equivalence of Categories                             | Awodey Ch 7.9-7.10                                | PS 7 out                             |
| Thu Mar 10   | Presheaves                                            | Awodey 8.1-8.4                                    |                                      |
| Tue Mar 15   | Yoneda Lemma                                          | Awodey 8.3-8.4                                    |                                      |
| Thu, Mar 17  | Higher-order Logic                                    | Awodey 8.8                                        | PS 8 out                             |
| Tue, Mar 22  | Adjunctions                                           | Awodey 9.1-9.2                                    |                                      |
| Thu Mar 24   | Examples of Adjunctions                               | Awodey 9.3-9.5                                    |                                      |
| Tue Mar 29   | Right Adjoints preserve limits                        | Awodey 9.6                                        | PS 9 out                             |
| Thu Mar 31   | Monads                                                | Awodey 10.1-10.2                                  |                                      |
| Tue, Apr 5   | Algebras of Monads                                    | Awodey 10.3-10.4                                  |                                      |
| Thu, Apr 7   | Monads in Programming                                 | TBD                                               | PS 10 out                            |
| Thu Apr 12   | Inductive/Coinductive Types as Initial/Final Algebras | TBD                                               |                                      |
| Thu Apr 14   | Call-by-push-value                                    | TBD                                               |                                      |
| Tue Apr 19   | Linear Logic                                          | TBD                                               |                                      |


[q]: https://oh.eecs.umich.edu/courses/eecs598
[ctc]: https://math.jhu.edu/~eriehl/context.pdf
[clcpov]: https://mikeshulman.github.io/catlog/catlog.pdf
[canvas]: https://umich.instructure.com/courses/493039
[piazza]: https://piazza.com/umich/winter2022/eecs598006 
[ps1]: /teaching/eecs-598-w22/docs/problem-set-1.pdf
[latex1]: /teaching/eecs-598-w22/docs/problem-set-1.tex
[ps2]: /teaching/eecs-598-w22/docs/problem-set-2.pdf
[latex2]: /teaching/eecs-598-w22/docs/problem-set-2.tex
