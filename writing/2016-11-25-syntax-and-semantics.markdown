---
title: Syntax and semantics
tags: scala, industry, programming languages, unfinished, in progress
unfinished: true
---

## Note

This post is unfinished. Take everything I say here with a pinch of salt. 

## Why the continuum is bad.

One of my friends has recently been trying to convince me that Scala is the language of choice for functional programming in industry. He has some persuasive arguments: Scala walks a hybrid path with traditional procedural OOP languages, runs on the JVM, and has lots of industry support already. As a strong supporter of purer functional languages (Haskell, Idris, etc), and cleaner approaches to imperative code (Rust etc), I have thus far found those arguments uncompelling. This has been compounded by my experience in using Scala for a largish software project[^1], and the messiness and complexity that it has unlocked in the codebase. 

Personally, this frustration has been borne out of the incremental, additive nature of the design of Scala. Instead of starting fresh with a new runtime system, standard libraries and semantics (like Haskell more or less did), Scala aims to provide a clear upgrade path for developers writing Java code. It does this by offering a similar syntax (extended with functional features), semantics, and by running on the JVM. I find this frustrating, as it offers no clear "breaking point" from older, less safe, technologies. 

For example, although Scala provides its own IO libraries, and recommended methods for printing to the terminal, there is nothing to prevent you, or even discourage you from calling the Java function `System.out.println`. By contrast, other language maintain a clear separation: Haskell forces you to use `unsafePerformIO` to break out from the IO monad, Rust requires that you mark code as `unsafe` to skip many of the compiler's safety checks, and even C provides lower level abilities (assembly) through a specific `asm` keyword[^2], rather than allowing it to be mixed into normal code.

I believe this is a large flaw in Scala - at least for the purposes of writing safe, performant, *maintainable* software. In my opinion, it is essential to make clear distinctions between safe, checked code, and code that may use less safe abstractions, and contain fewer checks[^3]. This makes it easier to track down bugs (hint: they're probably in the less safe code), and separate out your abstractions in a safe logical manner.

## Why the continuum is good.

However, is an unclear separation between old (unsafe) and new (safe) code always a good thing? 

One aspect of computer science I have often idly pondered is that of education, and in particular the learning of programming languages. There's an old belief that 

> If you know one programming language, learning another programming language is easier than learning the first!

I think there is a lot of truth behind this statement - I certainly found that once I got a handle on my first programming language, learning more became simpler and simpler. 

There is an unspoken implicit assumption, however, that I think tempers the strength of this cultural truism. 




    - Wrong attitude to take - possibly?
    - How do programming languages work, and how do we conceptualise them
    - Two aspects syntax, and semantics
    	- Syntax: how a program looks, and how it's structured (visually)
    	- Semantics: how it works, under the hood
    - Widely understood that moving between languages with similar syntax (but different semantics) is simple
    - Fewer examples of languages (approachable ones at last) with vastly different syntax but same semantics, so can't really draw any conclusions
    - I claim: Scala has a similar _enough_ syntax to Java, and similar enough semantics that it makes a simple logical shift from many other paradigms of languge
    - Functional features are just that - features, which allows for a smooth "functionalisaion" procedure, unlike (say) Haskell
    - This is why Scala will win - but does not make it a good language

[^1]: The [lift-lang](http://www.lift-lang.org/) functional embedded domain specific programming language for writing safe, efficient parallel code
[^2]: This may be compiler dependent, I haven't checked in a while.
[^3]: This is also a criticism I have of the language C++. Although C++ offers many features aimed at safety (smart pointers, RAII etc), which are often comparable to (say) Rust in their power, they are completely neutered by the ability of the programmer to drop back to unsafe C equivalents any time they wish, undermining any safety guarantees. Apart from coding standards and linters, there is nothing to stop a developer from dropping in an unsafe `scanf` where a much safer `cin >> ...` would do.