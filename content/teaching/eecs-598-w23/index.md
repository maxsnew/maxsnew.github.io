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

| Meeting Date | Topic                                                            | Readings                                             | HW                           | Scribe                         |
|:------------:|:----------------------------------------------------------------:|:----------------------------------------------------:|------------------------------|--------------------------------|
| Wed, Jan 04  | Course overview, Propositional Logic                             | [Frank Pfenning notes][pfenning-prop-log]            |                              | [Max S. New][notes0104]        |
| Mon, Jan 09  | Models of Propositional Logic                                    | [Crole Ch 1.1-1.4][Crole]                            | [PS1 Released][ps1]          | [Alan Yang][notes0109]         |
| Wed, Jan 11  | Soundness, Completeness, Initiality of Heyting Algebra Semantics |                                                      |                              | [Zhemin Qu][notes0111]         |
| Mon, Jan 16  | NO CLASS - MLK Day                                               |                                                      |                              |                                |
| Wed, Jan 18  | NO CLASS - POPL                                                  |                                                      |                              |                                |
| Mon, Jan 23  | Simple Type Theory: Syntax and Axiomatic Semantics               | [Crole Ch 4.1-4.3][Crole], [STT Full Rules][STT]     | PS1 Due, [PS2 Released][ps2] | [Yanjun Chen][notes0123]       |
| Wed, Jan 25  | Signatures for STT, Set-theoretic Semantics                      | [Crole Ch 3][Crole]                                  |                              | [Chris Jiang][notes0125]       |
| Mon, Jan 30  | Set-theoretic Semantics, Categories                              | [Crole Ch 2.1-2.2][Crole]                            |                              | [Jonah Nan][notes0130]         |
| Wed, Feb 01  | Categories                                                       | [Riehl Ch 1.1-1.2][Riehl]                            |                              | [Han Jiang][notes0201]         |
| Fri, Feb 03  |                                                                  |                                                      | PS2 Due, [PS3 Released][ps3] |                                |
| Mon, Feb 06  | Functors                                                         | [Crole Ch 2.3][Crole], [Riehl Ch 1.3][Riehl]         |                              | [Wen Plotnick][notes0206]      |
| Wed, Feb 08  | Natural Transformations, Universal Properties I (Products)       | [Crole Ch 2.4,2.6,2.8][Crole], [Riehl Ch 1.4][Riehl] |                              | [Steven Schaefer][notes0208]   |
| Mon, Feb 13  | Universal Properties II, Exponentials and Predicators            | [Riehl Ch. 2.1][Riehl]                               | PS3 Due, [PS4 Released][ps4] | [Runze Xue][notes0213]         |
| Wed, Feb 15  | Universal Properties III, Representability and Yoneda's Lemma    | [Riehl Ch. 2.2-2.3][Riehl]                           |                              | [Shubh Agrawal][notes0215]     |
| Mon, Feb 20  | C-T Structures I                                                 | [Riehl Ch. 2.3-2.4][Riehl]                           |                              | [Jigang Li][notes0220]         |
| Wed, Feb 22  | C-T Structures II                                                | [Crole Ch. 4.5-4.9][Crole]                           |                              | [Yuchen Jiang][notes0222]      |
| Fri, Feb 05  |                                                                  |                                                      | PS4 Due                      |                                |
| Mon, Feb 27  | NO CLASS - SPRING BREAK                                          |                                                      |                              |                                |
| Wed, Mar 01  | NO CLASS - SPRING BREAK                                          |                                                      |                              |                                |
| Mon, Mar 06  | C-T Structures III, Soundness and Completeness                   | [Crole Ch. 4.5-4.9][Crole]                           | [PS5 Released][ps5]          | [Jin Pan][notes0306]           |
| Wed, Mar 08  | Logical Relations                                                | [Crole Ch. 4.10][Crole]                              |                              | [Benjamin Kelly][notes0308]    |
| Mon, Mar 13  | More LR/Natural Numbers Objects                                  | [Hutton JFP '99][Hutton99]                           |                              | [Pranav Srinivasan][notes0313] |
| Wed, Mar 15  | Inductive Datatypes                                              | [McBride POPL '08][McBride08]                        |                              | [Kevin Wang][notes0315]        |
| Mon, Mar 20  | Evaluation Order and Computational Effects                       | [Levy Chapter 1][Levy]                               | PS5 Due, [PS6 Released][ps6] |                                |
| Wed, Mar 22  | Adjoint Functors and Equivalence of Categories                   |                                                      |                              |                                |
| Mon, Mar 27  | Adjunctions and Monads                                           |                                                      |                              |                                |
| Wed, Mar 29  | Call-by-push-value: A calculus for adjunctions                   |                                                      |                              |                                |
| Fri, Mar 31  |                                                                  |                                                      | PS6 Due, PS7 Released        |                                |
| Mon, Apr 03  | Recursive programs and datatypes, Untyped Lambda Calculus        |                                                      |                              |                                |
| Wed, Apr 05  | Domain Theory                                                    |                                                      |                              |                                |
| Mon, Apr 10  | Guarded Domain Theory                                            |                                                      | PS7 Due, PS8 Released        |                                |
| Wed, Apr 12  | Logical Relations for Effectful Programs                         |                                                      |                              |                                |
| Mon, Apr 17  | Something Fun                                                    |                                                      |                              |                                |
| Fri, Apr 21  |                                                                  |                                                      | PS8 Due                      |                                |

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

[notes0104]: /teaching/eecs-598-w23/docs/01-04-notes.pdf
[notes0109]: /teaching/eecs-598-w23/docs/01-09-notes.pdf
[notes0111]: /teaching/eecs-598-w23/docs/01-11-notes.pdf
[notes0123]: /teaching/eecs-598-w23/docs/01-23-notes.pdf
[notes0125]: /teaching/eecs-598-w23/docs/01-25-notes.pdf
[notes0130]: /teaching/eecs-598-w23/docs/01-30-notes.pdf
[notes0201]: /teaching/eecs-598-w23/docs/02-01-notes.pdf
[notes0206]: /teaching/eecs-598-w23/docs/02-06-notes.pdf
[notes0208]: /teaching/eecs-598-w23/docs/02-08-notes.pdf
[notes0213]: /teaching/eecs-598-w23/docs/02-13-notes.pdf
[notes0215]: /teaching/eecs-598-w23/docs/02-15-notes.pdf
[notes0220]: /teaching/eecs-598-w23/docs/02-20-notes.pdf
[notes0222]: /teaching/eecs-598-w23/docs/02-22-notes.pdf
[notes0301]: /teaching/eecs-598-w23/docs/03-01-notes.pdf
[notes0306]: /teaching/eecs-598-w23/docs/03-06-notes.pdf
[notes0308]: /teaching/eecs-598-w23/docs/03-08-notes.pdf
[notes0313]: /teaching/eecs-598-w23/docs/03-13-notes.pdf
[notes0315]: /teaching/eecs-598-w23/docs/03-15-notes.pdf
[notes0320]: /teaching/eecs-598-w23/docs/03-20-notes.pdf
[notes0322]: /teaching/eecs-598-w23/docs/03-22-notes.pdf
[notes0327]: /teaching/eecs-598-w23/docs/03-27-notes.pdf
[notes0329]: /teaching/eecs-598-w23/docs/03-29-notes.pdf
[notes0403]: /teaching/eecs-598-w23/docs/04-03-notes.pdf
[notes0405]: /teaching/eecs-598-w23/docs/04-05-notes.pdf
[notes0410]: /teaching/eecs-598-w23/docs/04-10-notes.pdf
[notes0412]: /teaching/eecs-598-w23/docs/04-12-notes.pdf
[notes0417]: /teaching/eecs-598-w23/docs/04-17-notes.pdf


[STT]: /teaching/eecs-598-w23/docs/stt-full-rules.pdf

[ps1]: /teaching/eecs-598-w23/docs/ps1.pdf
[ps2]: /teaching/eecs-598-w23/docs/ps2.pdf
[ps3]: /teaching/eecs-598-w23/docs/ps3.pdf
[ps4]: /teaching/eecs-598-w23/docs/ps4.pdf
[ps5]: /teaching/eecs-598-w23/docs/ps5.pdf
[ps6]: /teaching/eecs-598-w23/docs/ps6.pdf
[ps7]: /teaching/eecs-598-w23/docs/ps7.pdf
[ps8]: /teaching/eecs-598-w23/docs/ps8.pdf

