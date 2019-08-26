---
title: Avoid mutability
tags: programming, culture, information, learning
date: 2019-08-26
unfinished: true
abstract: Avoid mutability when you can, not just in code, but in how you think about tools, process and systems.
---

Avoid mutability when you can. It's a common refrain amongst certain sets of programmers, for example those of a functional bent, and suggests than when writing code immutable values should be preferred to mutable variables. Avoiding mutability brings many benefits: it reduces dependencies between code, making it easier to optimise, and reduces the likelyhood of concurrency or ordering bugs cropping up. Avoiding mutability is generally a good thing.

I'd like to propose that, as programmers, we should be avoiding mutability wherever possible - not just in our programs, but in the way we think about software, and the way we build tools, ecosystems, and communities around software.

In particular, I think we need to try and avoid *mutable information*. By mutable information, I mean facts, diagnostics, guides, or documentation that are subject to change. Unlike variables, information exists in a continuum of mutability. At one end, there are (arguably objective) facts, such as the statement "The number five is prime". From the perspective of human civilisation, such statements are not going to change in any meaningful way. At the other end, are fluid, temporary datums, such as "I'm feeling cold right now". Such statements are mutable, and the truth or relevance of them is constantly in flux.

It seems clear to me that when we wish to build something of value, or an artefact that is designed to last, our starting point should be principles that are as close to *immutable* as possible.

But what does immutability mean in the context of a software ecosystem?
