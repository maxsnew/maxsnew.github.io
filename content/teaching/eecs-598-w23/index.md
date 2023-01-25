---
title: Category Theory
---

# EECS 598 005: Category Theory for Computer Scientists
- Lecture: Monday & Wednesday, 2:30-4:30pm, EECS 3427
- Instructor: [Max S. New][maxsnew]
- Office: Beyster 4640
- uniqname: maxsnew
- Office Hours: Tuesdays 2-5pm or by appointment
- [Canvas][canvas]
- [Piazza][piazza]
- [Lecture Capture][leccap]

Category theory is a mathematical theory of structures and
transformations. While originally developed for applications to pure
mathematics, category theory has become a useful tool for studying
logic and programming languages.

The breadth of applications and high level of abstraction can make
category theory difficult to approach for those without an extensive
math background. This course aims to introduce and motivate category
theory by focusing on applications in computer science, in particular
to logic and programming languages.

Students are expected to have familiarity with basic notions of
programming language theory such as abstract syntax, type systems and
some form of formal semantics (operational, axiomatic or
denotational). Students with a strong math background but minimal
programming language background are also welcome to enroll but the
focus of the class will be on these computer science applications.

## Learning Objectives

After completing this course, students should be able to

- give axiomatic and denotational semantics to different kinds of
  logics and programming languages
- carry out basic proofs in order theory and category theory
- learn and explore further applications of order theory and category
  theory on their own, especially in programming language research
  papers

## Degree Requirements

To see what degree requirements this course satisfies, see the
spreadsheet on [this page](https://cse.engin.umich.edu/academics/course-information/special-topics-courses/).

## Evaluation

There are four components of your grade:

1. Problem Sets (70%)
2. In-class presentation of solutions (10%): we will set aside class
   time to review the solutions to problem sets
3. Scribing (10%): students will contribute notes written in LaTeX of
   the in-class lectures to share with the entire class.
4. Participation (10%): attendance and participation in lecture.

## Problem Sets

Problem Sets will be posted below in the schedule with their LaTeX
source in the [gitlab repo][signups]. You can complete them in groups
of 2 or 3. You will submit them on Canvas as a pdf. You may include
hand-drawn diagrams, but text and equations should be typeset in
LaTeX.

Problem sets will be always be due at 11:59pm on the listed due
date. Late work will not be accepted.

Students will present solutions to problem sets at the end of certain
lecture periods. Signup for this on the [gitlab repo][signups].

## Scribing

This semester will use a new curriculum without following a single
textbook. To help everyone in the class, each lecture will have an
assigned scribe who will take notes on the lecture and post a LaTeX
copy in the [gitlab repo][signups].

## Attendance

Participating in this course means attending lecture in
person. Lectures will be recorded but this is not a substitute for
attendance. Each class will have a sign-in sheet.

## Course Schedule

The following schedule of topics and homeworks is *tentative* and will
be updated throughout the semester. 

We will have two weeks of no lecture during the semester: the week of
MLK day and spring break. There will not be a homework due after
spring break but there will be after MLK week (since only one day is
an academic holiday).

Readings are chosen to complement the lecture. All readings will be
from freely available online sources.

| Meeting Date | Topic                                                            | Readings                                  | HW                           | Scribe                   |
|:------------:|:----------------------------------------------------------------:|:-----------------------------------------:|------------------------------|--------------------------|
| Wed, Jan 04  | Course overview, Propositional Logic                             | [Frank Pfenning notes][pfenning-prop-log] |                              | [Max S. New][notes0104]  |
| Mon, Jan 09  | Models of Propositional Logic                                    | [Crole Ch 1.1-1.4][Crole]                 | [PS1 Released][ps1]          | [Alan Yang][notes0109]   |
| Wed, Jan 11  | Soundness, Completeness, Initiality of Heyting Algebra Semantics |                                           |                              | [Zhemin Qu][notes0111]   |
| Mon, Jan 16  | NO CLASS - MLK Day                                               |                                           |                              |                          |
| Wed, Jan 18  | NO CLASS - POPL                                                  |                                           |                              |                          |
| Mon, Jan 23  | Simple Type Theory: Syntax and Axiomatic Semantics               | [Crole Ch 4.1-4.3][Crole]                 | PS1 Due, [PS2 Released][ps2] | [Yanjun Chen][notes0123] |
| Wed, Jan 25  | Signatures for STT, Set-theoretic Semantics                      | [Crole Ch 3][Crole]                       |                              |                          |
| Mon, Jan 30  | Categories, Functors                                             |                                           |                              |                          |
| Wed, Feb 01  | Universal Mapping Properties and Yoneda's Lemma                  |                                           | PS2 Due, PS3 Released        |                          |
| Mon, Feb 06  | Initiality of STT in Cartesian Closed Categories                 |                                           |                              |                          |
| Wed, Feb 08  | Disjunction Property for STT                                     |                                           |                              |                          |
| Mon, Feb 13  | Inductive Datatypes, Axiomatics                                  |                                           | PS3 Due, PS4 Released        |                          |
| Wed, Feb 15  | Inductive Datatypes, Models                                      |                                           |                              |                          |
| Mon, Feb 20  | Evaluation Order and Computational Effects                       |                                           |                              |                          |
| Wed, Feb 22  | Concrete Models of Effects, Adjoint Functors                     |                                           | PS4 Due                      |                          |
| Mon, Feb 27  | NO CLASS - SPRING BREAK                                          |                                           |                              |                          |
| Wed, Mar 01  | NO CLASS - SPRING BREAK                                          |                                           |                              |                          |
| Mon, Mar 06  | Monads and Algebras                                              |                                           | PS5 Released                 |                          |
| Wed, Mar 08  | Call-by-push-value                                               |                                           |                              |                          |
| Mon, Mar 13  | Recursive programs and datatypes, fixed points                   |                                           |                              |                          |
| Wed, Mar 15  | Basic Domain Theory                                              |                                           | PS5 Due, PS6 Released        |                          |
| Mon, Mar 20  | Solving Domain Equations                                         |                                           |                              |                          |
| Wed, Mar 22  | Guarded Domain Theory                                            |                                           |                              |                          |
| Mon, Mar 27  | Intuitionistic Linear Logic, Monoidal Categories                 |                                           | PS6 Due, PS7 Released        |                          |
| Wed, Mar 29  | Models of ILL                                                    |                                           |                              |                          |
| Mon, Apr 03  | Bunched Implications/Separation Logic                            |                                           |                              |                          |
| Wed, Apr 05  | Dependent Type Theory                                            |                                           | Ps7 Due, PS8 Released        |                          |
| Mon, Apr 10  | Fibrations, Categories with Families                             |                                           |                              |                          |
| Wed, Apr 12  | Presheaf Models                                                  |                                           | PS8 Due                      |                          |
| Mon, Apr 17  | Subobject Classifiers, Toposes                                   |                                           |                              |                          |


[maxsnew]: http://maxsnew.com
[canvas]: https://umich.instructure.com/courses/574129
[piazza]: https://piazza.com/class/lcgj8zh7crs1ba/
[signups]: https://gitlab.eecs.umich.edu/598-wi23/scribed-notes
[leccap]: https://leccap.engin.umich.edu/leccap/site/z02eb2esrpaddy7cnwz

[pfenning-prop-log]: http://www.cs.cmu.edu/~fp/courses/15317-f17/lectures/02-natded.pdf
[Crole]: https://doi-org.proxy.lib.umich.edu/10.1017/CBO9781139172707

[notes0104]: /teaching/eecs-598-w23/docs/01-04-notes.pdf
[notes0109]: /teaching/eecs-598-w23/docs/01-09-notes.pdf
[notes0111]: /teaching/eecs-598-w23/docs/01-11-notes.pdf
[notes0123]: /teaching/eecs-598-w23/docs/01-23-notes.pdf

[ps1]: /teaching/eecs-598-w23/docs/ps1.pdf
[ps2]: /teaching/eecs-598-w23/docs/ps2.pdf

