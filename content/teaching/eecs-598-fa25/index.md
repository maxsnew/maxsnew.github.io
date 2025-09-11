---
title: Category Theory
---

# EECS 598 005: Category Theory for Computer Scientists
- Lecture: Monday & Wednesday, 2:30-4:30pm, Beyster 1620
- Instructor: [Max S. New][maxsnew]
- Office: Beyster 4628
- uniqname: maxsnew
- Office Hours: Tuesdays and Thursdays 3:30-5pm or by appointment
- [Canvas][canvas]
- [Lecture capture][leccap]

Category theory is an area of abstract algebra that studies structures
along with their transformations. Categorical structures and universal
mapping properties are ubiquitious in mathematics and category theory
provides a common language and toolkit that greatly helps to organize
mathematical theories. 

This course provides an introduction to category theory, focusing on
applications relevant to computer science. Specifically we will be
focusing on the subfield of _categorical logic_ which applies category
theory to the design and analysis of formal logics and programming
languages.

There are no formal prerequisites for the course, but it would be
helpful to have familiarity with basic notions of programming language
theory such as abstract syntax, type systems and formal semantics
(operational, axiomatic or denotational). We will cover the relevant
notions in an accelerated fashion in the course. Students with
mathematical maturity but minimal programming language background are
welcome to enroll but should understand the focus of the class will be
on these computer science applications.

## Learning Objectives

After completing this course, students should be able to

- give axiomatic and denotational semantics to different kinds of
  logics and programming languages
- carry out proofs in order theory and category theory
- learn and explore further applications of order theory and category
  theory on their own, especially in programming language research
  papers

## Degree Requirements

To see what degree requirements this course satisfies, see the
spreadsheet on [this page](https://cse.engin.umich.edu/academics/course-information/special-topics-courses/).

## Evaluation

There are four components of your grade:

1. Problem Sets (60%)
2. Final Project (20%)
2. In-class presentation of solutions (10%): we will set aside class
   time to review the solutions to problem sets
3. Scribing (5%): students will contribute notes written in LaTeX of
   the in-class lectures to share with the entire class.
4. Participation (5%): attendance and participation in lecture.

### Problem Sets

Problem sets provide challenge problems to ensure mastery of the
topics of lecture and the readings. Problem Sets will be posted below
in the schedule with their LaTeX source in the [github
repo][signups]. You can complete them in groups of 2 or 3. You will
submit them on Canvas as a pdf. You may include hand-drawn diagrams,
but text and equations should be typeset in LaTeX.

Problem sets will be always be due at 11:59pm on the listed due
date. Late work will not be accepted. It is always better to submit
incomplete or incorrect work than nothing at all.

Students will present solutions to problem sets at the end of certain
lecture periods. Signup for a presentation slot on the [github][signups].

### Final Projects

To complement the material from lecture, students will undertake an
independent research project related to category theory. Examples
include a survey of an area of category theory not covered in class,
implementation of category theory in a proof assistant, or a novel
categorical analysis of some area of your own choosing. Students are
encouraged to explore an area related to their own research interests. 

### Scribing

Each lecture will have an assigned scribe who will take notes on the
lecture and post a LaTeX copy in the [git repo][signups].

### Attendance and Participation

In-person attendance in this course is required. If you have a strong
interest in taking the course but cannot attend in person, you may
discuss your special circumstances with the professor. Lectures will
be recorded for reference but this is not a substitute for
attendance. Each class will have a sign-in sheet.

## Course Schedule and Readings

The following schedule of topics and homeworks is *tentative* and will
be updated throughout the semester. 

Readings are chosen to complement the lecture. All readings will be
from freely available online sources. I recommend reading before
attending class. There is no textbook we will directly follow, but the
two we will reference the most are [Roy Crole's _Categories for
Types_][Crole]
and [Emily Riehl's _Category Theory in
Context_][Riehl].

| Meeting Date | Topic                                                            | Readings                                                                                                                        | HW                           | Scribe                          |
|:------------:|:----------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------:|------------------------------|---------------------------------|
| Mon, Aug 25  | Course overview, Intuitionistic Propositional Logic              | [Frank Pfenning notes on natural deduction][pfenning-prop-log]  [Shulman Chapter 0 primer on categorical logic][shulman-catlog] |                              | [Max S. New][lec0825]           |
| Wed, Aug 27  | Concrete and Abstract Models of IPL                              | [Crole Ch 1.1-1.4][Crole]                                                                                                       |                              | [Ivan Wei][lec0827]             |
| Thu, Aug 28  |                                                                  |                                                                                                                                 | [PS1 Released][ps1]          |                                 |
| Mon, Sep 01  | NO CLASS - Labor Day                                             |                                                                                                                                 |                              |                                 |
| Wed, Sep 03  | Soundness, Completeness, Initiality of Heyting Algebra Semantics |                                                                                                                                 |                              | [Ayan Chowdhury][lec0903]       |
| Mon, Sep 08  | Simple Type Theory: Syntax and Equational Theory                 | [Crole Ch 4.1-4.3][Crole]                                                                                                       |                              | [Alexander Bandukwala][lec0908] |
| Wed, Sep 10  | Set-theoretic Semantics, Categories                              | [Crole Ch 3][Crole]                                                                                                             |                              |                                 |
| Thu, Sep 11  |                                                                  |                                                                                                                                 | PS1 Due, [PS2 Released][ps2] |                                 |
| Mon, Sep 15  | Categories, Functors                                             | [Crole Ch 2.1-2.3][Crole], [Riehl Ch 1.1-1.3][Riehl]                                                                            |                              |                                 |
| Wed, Sep 17  | Natural Transformations, Universal Properties                    | [Crole Ch 2.4,2.6,2.8][Crole], [Riehl Ch 1.4-1.6][Riehl]                                                                        |                              |                                 |
| Mon, Sep 22  | Presheaves, Representability and Yoneda's Lemma                  | [Riehl Ch 2][Riehl]                                                                                                             |                              |                                 |
| Wed, Sep 24  | Simple Categories with Families                                  |                                                                                                                                 |                              |                                 |
| Thu, Sep 25  |                                                                  |                                                                                                                                 | PS2 Due, PS3 Released        |                                 |
| Mon, Sep 29  | Initiality of STLC                                               | <!-- [Riehl Ch. 2.1][Riehl] -->                                                                                                 |                              |                                 |
| Wed, Oct 01  | Logical Relations, Canonicity                                    | <!-- [Riehl Ch. 2.2-2.3][Riehl] -->                                                                                             |                              |                                 |
| Mon, Oct 06  | <!-- Simple Categories with Families I -->                       | <!-- [Riehl Ch. 2.3-2.4][Riehl] -->                                                                                             |                              |                                 |
| Wed, Oct 08  | <!-- SCwF II -->                                                 | <!-- [Crole Ch. 4.5-4.9][Crole] -->                                                                                             |                              |                                 |
| Thu, Oct 09  |                                                                  |                                                                                                                                 | PS3 Due, PS4 Released        |                                 |
| Mon, Oct 13  | NO CLASS - Fall Study Break                                      |                                                                                                                                 |                              |                                 |
| Wed, Oct 15  | <!-- SCwF III, Soundness and Completeness -->                    | <!-- [Crole Ch. 4.5-4.9][Crole] -->                                                                                             |                              |                                 |
| Thu, Oct 16  |                                                                  |                                                                                                                                 | Final Project Proposals Due  |                                 |
| Mon, Oct 20  | <!-- Logical Relations -->                                       | <!-- [Crole Ch. 4.10][Crole] -->                                                                                                |                              |                                 |
| Wed, Oct 22  | <!-- More LR/Natural Numbers Objects -->                         | <!-- [Hutton JFP '99][Hutton99] -->                                                                                             |                              |                                 |
| Thu, Oct 23  |                                                                  |                                                                                                                                 | PS4 Due, PS5 Released        |                                 |
| Mon, Oct 27  | <!-- Inductive Definitions, Structural Recursion -->             | <!-- [McBride POPL '08][McBride08] -->                                                                                          |                              |                                 |
| Wed, Oct 29  | <!-- Equivalence of Categories -->                               | <!-- [Riehl Ch 1.5][Riehl] -->                                                                                                  |                              |                                 |
| Mon, Nov 03  | <!-- Adjoint Functors               -->                          | <!-- [Riehl Ch 4.1-4.3][Riehl] -->                                                                                              |                              |                                 |
| Wed, Nov 05  | <!-- More Adjoint Functors -->                                   |                                                                                                                                 |                              |                                 |
| Thu, Nov 06  |                                                                  |                                                                                                                                 | PS5 Due, PS6 Released        |                                 |
| Mon, Nov 10  | Evaluation Order and Computational Effects                       |                                                                                                                                 |                              |                                 |
| Wed, Nov 12  | Monads, Kleisli Categories                                       |                                                                                                                                 |                              |                                 |
| Mon, Nov 17  | Call-by-value                                                    |                                                                                                                                 |                              |                                 |
| Wed, Nov 19  | Step-indexed models of unbounded Recursion                       |                                                                                                                                 |                              |                                 |
| Wed, Nov 20  |                                                                  |                                                                                                                                 | PS6 Due                      |                                 |
| Mon, Nov 24  | first-order and higher-order logic, toposes                      |                                                                                                                                 |                              |                                 |
| Mon, Dec 01  | dependent type theory                                            |                                                                                                                                 |                              |                                 |
| Wed, Dec 03  | Final Project Presentations                                      |                                                                                                                                 |                              |                                 |
| Mon, Dec 08  | Wrap up, Final Project Presentations                             |                                                                                                                                 |                              |                                 |
| Thu, Dec 11  |                                                                  |                                                                                                                                 | Final Project Writeups Due   |                                 |

[maxsnew]: http://maxsnew.com
[canvas]: https://umich.instructure.com/courses/784407
[signups]: https://github.com/um-catlab/eecs-598-fa2025
[leccap]: https://leccap.engin.umich.edu/leccap/site/fx6em90i7nhkh6ajtl5

[pfenning-prop-log]: http://www.cs.cmu.edu/~fp/courses/15317-f17/lectures/02-natded.pdf
[Crole]: https://doi-org.proxy.lib.umich.edu/10.1017/CBO9781139172707
[Riehl]: https://emilyriehl.github.io/files/context.pdf
[Hutton99]: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf
[McBride08]: https://dl.acm.org/doi/pdf/10.1145/1328897.1328474
[Levy]: https://link-springer-com.proxy.lib.umich.edu/book/10.1007/978-94-007-0954-6
[shulman-catlog]: https://mikeshulman.github.io/catlog/catlog.pdf

[lec0825]: /teaching/eecs-598-fa25/docs/08-25-notes.pdf
[lec0827]: /teaching/eecs-598-fa25/docs/08-27-notes.pdf
[lec0903]: /teaching/eecs-598-fa25/docs/09-03-notes.pdf
[lec0908]: /teaching/eecs-598-fa25/docs/09-08-notes.pdf
[lec0910]: /teaching/eecs-598-fa25/docs/09-10-notes.pdf
[lec0915]: /teaching/eecs-598-fa25/docs/09-15-notes.pdf
[lec0917]: /teaching/eecs-598-fa25/docs/09-17-notes.pdf
[lec0922]: /teaching/eecs-598-fa25/docs/09-22-notes.pdf
[lec0924]: /teaching/eecs-598-fa25/docs/09-24-notes.pdf
[lec0929]: /teaching/eecs-598-fa25/docs/09-29-notes.pdf

[lec1001]: /teaching/eecs-598-fa25/docs/10-01-notes.pdf
[lec1006]: /teaching/eecs-598-fa25/docs/10-06-notes.pdf
[lec1008]: /teaching/eecs-598-fa25/docs/10-08-notes.pdf
[lec1015]: /teaching/eecs-598-fa25/docs/10-15-notes.pdf
[lec1020]: /teaching/eecs-598-fa25/docs/10-20-notes.pdf
[lec1022]: /teaching/eecs-598-fa25/docs/10-22-notes.pdf
[lec1027]: /teaching/eecs-598-fa25/docs/10-27-notes.pdf
[lec1029]: /teaching/eecs-598-fa25/docs/10-29-notes.pdf

[lec1103]: /teaching/eecs-598-fa25/docs/11-03-notes.pdf
[lec1105]: /teaching/eecs-598-fa25/docs/11-05-notes.pdf
[lec1110]: /teaching/eecs-598-fa25/docs/11-10-notes.pdf
[lec1112]: /teaching/eecs-598-fa25/docs/11-12-notes.pdf
[lec1117]: /teaching/eecs-598-fa25/docs/11-17-notes.pdf
[lec1119]: /teaching/eecs-598-fa25/docs/11-19-notes.pdf
[lec1124]: /teaching/eecs-598-fa25/docs/11-24-notes.pdf

[lec1201]: /teaching/eecs-598-fa25/docs/12-01-notes.pdf
[lec1203]: /teaching/eecs-598-fa25/docs/12-03-notes.pdf
[lec1208]: /teaching/eecs-598-fa25/docs/12-08-notes.pdf

[ps1]: /teaching/eecs-598-fa25/docs/ps1.pdf
[ps2]: /teaching/eecs-598-fa25/docs/ps2.pdf
[ps3]: /teaching/eecs-598-fa25/docs/ps3.pdf
[ps4]: /teaching/eecs-598-fa25/docs/ps4.pdf
[ps5]: /teaching/eecs-598-fa25/docs/ps5.pdf
[ps6]: /teaching/eecs-598-fa25/docs/ps6.pdf
