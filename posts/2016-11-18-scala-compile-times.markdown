---
title: Scala compile times
author: PhD
tags: scala, compilers, efficiency, frustration
---

Yesterday saw me merging in some changes from master into an out-of-date feature branch in the project that I work on as part of my PhD. This involved the usual frustration of merge conflicts, and interface changes between the master branch and my code (~4-5 months out of date!) None of those frustrations, however, were quite as bad as how slow scala was to iterate with after I had finished merging.

The project I work with is an embedded domain specific langague for writing highly parallel GPU codes. As an EDSL, it is essentially a library (in scala), which means that instead of expressing a main "end product" as a fixed executable [^1] our main "runnable" endproduct is an extensive test suite which explores the various parts of the language that we can express. 

After a merge, the first thing to do is run `sbt test`, to first compile the project, and then run the test suite to find regressions. This is where I got frustrated, as the initial compilation step is *interminably* slow. I can perfectly understand a slow first compilation but, even with (I assume??) incremental compilation, compiling the project (only around 80kloc), took around a minute to a minute and a half each time. This meant that every time I found a bug, or type missmatch, or the tests threw an execption, it took almost double the time to compile the project as it did to fix the damn bug. 

[^1]: We do have small executables for, e.g. benchmarks, however they are not the main target of the project, and often only use a subset of the full functionality. 