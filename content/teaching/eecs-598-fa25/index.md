---
title: Category Theory
---

# EECS 598 005: Category Theory for Computer Scientists
- Lecture: Monday & Wednesday, 2:30-4:30pm, EECS 3427
- Instructor: [Max S. New][maxsnew]
- Office: Beyster 4628
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
mathematical background. This course aims to introduce and motivate
category theory by focusing on applications in computer science, in
particular to logic and programming languages.

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

1. Problem Sets (60%)
2. Final Project (20%)
2. In-class presentation of solutions (10%): we will set aside class
   time to review the solutions to problem sets
3. Scribing (5%): students will contribute notes written in LaTeX of
   the in-class lectures to share with the entire class.
4. Participation (5%): attendance and participation in lecture.

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

Readings are chosen to complement the lecture. All readings will be
from freely available online sources. I recommend reading before
attending class.

| Meeting Date | Topic                                                            | Readings                                             | HW                           | Scribe                  |
|:------------:|:----------------------------------------------------------------:|:----------------------------------------------------:|------------------------------|-------------------------|
| Mon, Aug 25  | Course overview, Propositional Logic                             | [Frank Pfenning notes][pfenning-prop-log]            |                              | [Max S. New][notes0104] |
| Wed, Aug 27  | Concrete and Abstract Models of Propositional Logic              | [Crole Ch 1.1-1.4][Crole]                            | [PS1 Released][ps1]          |                         |
| Mon, Sep 01  | NO CLASS - Labor Day                                             |                                                      |                              |                         |
| Wed, Sep 03  | Soundness, Completeness, Initiality of Heyting Algebra Semantics |                                                      |                              |                         |
| Mon, Sep 08  | Simple Type Theory: Syntax and Axiomatic Semantics               | [Crole Ch 4.1-4.3][Crole], [STT Full Rules][STT]     | PS1 Due, [PS2 Released][ps2] |                         |
| Wed, Sep 10  | Signatures for STT, Set-theoretic Semantics                      | [Crole Ch 3][Crole]                                  |                              |                         |
| Mon, Sep 15  | Set-theoretic Semantics, Categories                              | [Crole Ch 2.1-2.2][Crole]                            |                              |                         |
| Wed, Sep 17  | Categories                                                       | [Riehl Ch 1.1-1.2][Riehl]                            |                              |                         |
| Fri, Feb 03  |                                                                  |                                                      | PS2 Due, [PS3 Released][ps3] |                         |
| Mon, Sep 22  | Functors                                                         | [Crole Ch 2.3][Crole], [Riehl Ch 1.3][Riehl]         |                              |                         |
| Wed, Sep 24  | Natural Transformations, Universal Properties I (Products)       | [Crole Ch 2.4,2.6,2.8][Crole], [Riehl Ch 1.4][Riehl] |                              |                         |
| Mon, Sep 29  | Universal Properties II, Exponentials and Predicators            | [Riehl Ch. 2.1][Riehl]                               | PS3 Due, [PS4 Released][ps4] |                         |
| Wed, Oct 01  | Universal Properties III, Representability and Yoneda's Lemma    | [Riehl Ch. 2.2-2.3][Riehl]                           |                              |                         |
| Mon, Oct 06  | C-T Structures I                                                 | [Riehl Ch. 2.3-2.4][Riehl]                           |                              |                         |
| Wed, Oct 08  | C-T Structures II                                                | [Crole Ch. 4.5-4.9][Crole]                           |                              |                         |
| Fri, Feb 05  |                                                                  |                                                      | PS4 Due                      |                         |
| Mon, Oct 13  | NO CLASS - Fall Study Break                                      |                                                      |                              |                         |
| Wed, Oct 15  | C-T Structures III, Soundness and Completeness                   | [Crole Ch. 4.5-4.9][Crole]                           | [PS5 Released][ps5]          |                         |
| Mon, Oct 20  | Logical Relations                                                | [Crole Ch. 4.10][Crole]                              |                              |                         |
| Wed, Oct 22  | More LR/Natural Numbers Objects                                  | [Hutton JFP '99][Hutton99]                           |                              |                         |
| Mon, Oct 27  | Inductive Datatypes                                              | [McBride POPL '08][McBride08]                        |                              |                         |
| Wed, Oct 29  | Evaluation Order and Computational Effects                       | [Levy Chapter 1][Levy]                               | PS5 Due, [PS6 Released][ps6] |                         |
| Mon, Nov 03  | Equivalence of Categories                                        | [Riehl Ch 1.5][Riehl]                                |                              |                         |
| Wed, Nov 05  | Adjoint Functors                                                 | [Riehl Ch 4.1-4.3][Riehl]                            |                              |                         |
| Mon, Nov 10  | More Adjoint Functors                                            | [Levy Chapter 2][Levy]                               |                              |                         |
| Fri, Mar 31  |                                                                  |                                                      | PS6 Due, [PS7 Released][ps7] |                         |
| Wed, Nov 12  | Call-by-push-value                                               |                                                      |                              |                         |
| Mon, Nov 17  | Call-by-push-value II                                            |                                                      |                              |                         |
| Wed, Nov 19  | Effects in CBPV                                                  |                                                      | PS7 Due, [PS8 Released][ps8] |                         |
| Mon, Nov 24  | Models of CBPV                                                   |                                                      |                              |                         |
| Mon, Dec 01  | Final Project Presentations                                      |                                                      |                              |                         |
| Wed, Dec 03  | Final Project Presentations                                      |                                                      |                              |                         |
| Mon, Dec 08  | Final Project Presentations                                      |                                                      |                              |                         |
| Fri, Apr 21  |                                                                  |                                                      | PS8 Due                      |                         |

[maxsnew]: http://maxsnew.com
[canvas]: https://umich.instructure.com/courses/574129
[piazza]: https://piazza.com/class/lcgj8zh7crs1ba/
[signups]: https://gitlab.eecs.umich.edu/598-wi23/scribed-notes
[leccap]: https://leccap.engin.umich.edu/leccap/site/z02eb2esrpaddy7cnwz

[pfenning-prop-log]: http://www.cs.cmu.edu/~fp/courses/15317-f17/lectures/02-natded.pdf
[Crole]: https://doi-org.proxy.lib.umich.edu/10.1017/CBO9781139172707
[Riehl]: https://math.jhu.edu/~eriehl/context.pdf
[Hutton99]: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf
[McBride08]: https://dl.acm.org/doi/pdf/10.1145/1328897.1328474
[Levy]: https://link-springer-com.proxy.lib.umich.edu/book/10.1007/978-94-007-0954-6
