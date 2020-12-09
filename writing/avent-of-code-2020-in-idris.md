---
title: Advent of Code 2020, in Idris
tags: advent-of-code, Idris
date: 2020-12-09
unfinished: True
abstract: Thoughts and feedback on using Idris for solving the Advent of Code 2020 problems
---

# Preamble

For the first time in a while I'm taking a solid crack at solving some of the "Advent Of Code" [problems](https://adventofcode.com/). Advent of code is a nice set of daily programming challenges, released in the same way as an advent calendar.

Usually I'm not a huge fan of programming challenges, as they often embody much of the elitist masochistic culture that pervades computer science, but this year I was encouraged to partake by a friend of mine and so decided to have a proper go. My decision was helped by the style of problems that Advent of Code poses: it provides very nice straightforward puzzles and solving them is much more like filling in a crossword puzzle, rather than entering into a testosterone fuelled no-holds-barred hackathon.

When approaching puzzles or "pointless" problems my usual approach is to treat them as a vehicle for learning something new. For example, will they teach me about some new bit of maths? Can I use a new tool to solve them? What will I learn from this? Advent of code has been no different, and I've decided to use this as a chance to get my teeth into writing some Idris code.

## Idris

For those of you who have not come across it, Idris is a dependently typed strict functional language originally written by Edwin Brady. I chose it as my *leçon du jour* as I am keen to try and develop my understanding of dependent types, and get some practice with using them "in anger". Although there are other languages (such as Agda) that offer an opportunity to play around with dependent types, I chose to use Idris as I have previously dipped my toes into it, and it positions itself more as a programming language than a proof assistant. 

## Some criticism 

**Warning:** This post will include some criticism of Idris (the language) and its associated ecosystem. I want to be very clear that this criticism is *purely constructive*, and offered mostly as documentation (for myself) of aspects that could be improved. 

I have the utmost respect for Edwin, and the various other contributors to the Idris language and ecosystem, and no criticism here is a criticism of their work or them as people. They have worked incredibly hard to create a wonderful tool, but they are only mortals, and they can only do so much. Almost every single problem I have come across has been caused by a surfeit of manpower or investment in the language.

Please contact me if you wish to discuss anything I bring up in this post.

# Solving Advent of Code in Idris

The rest of this post will more or less be my blow-by-blow account of the joy and stress I have encountered while tackling the Advent Of Code using Idris. I started writing this document shortly after solving Day 3, so comments on Day 4 and onwards will be in-the-moment, while earlier ones may be reflective. 

You can find my solutions [here](https://github.com/AdamHarries/advent-of-code-2020).

# Day 1

Solution: [Day 1](https://github.com/AdamHarries/advent-of-code-2020/blob/main/Day1.idr)

<!-- Solving Day 1 was overall straightforward. I did, however, encounter some pain points while using the language:  -->
