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

When approaching puzzles or "pointless" problems my usual approach is to treat them as a vehicle for learning something new. For example, will they teach me about some new bit of maths? Can I learn a new tool to solve them? What will I learn from this? Advent of code has been no different, and I've decided to use this as a chance to get my teeth into writing some Idris code.

## Idris

For those of you who have not come across it, Idris is a dependently typed strict functional language originally written by Edwin Brady. I chose it as my *leçon du jour* as I am keen to try and develop my understanding of dependent types, and get some practice with using them "in anger". Although there are other languages (such as Agda) that offer an opportunity to play around with dependent types, I chose to use Idris as I have previously dipped my toes into it, and it positions itself more as a programming language than a proof assistant. 

I used Idris 2 for solving the Advent of Code problems, so wherever I say "Idris" in this post, please mentally substitute in "Idris 2".

## Some criticism 

**Warning:** This post will include some criticism of Idris (the language) and its associated ecosystem. I want to be very clear that this criticism is *purely constructive*[^constructive], and offered mostly as documentation (for myself) of aspects that could be improved. 

I have the utmost respect for Edwin, and the various other contributors to the Idris language and ecosystem, and no criticism here is a criticism of their work or them as people. They have worked incredibly hard to create a wonderful tool, but they are only mortals, and they can only do so much. Almost every single problem I have come across has been caused by a surfeit of manpower or investment in the language.

I am keenly aware that Idris is *not* meant to be a production-ready tool for immediate industrial use, and in none of my criticisms do I intend to measure it as such. I am sure, however, that my opinions *are* subconsciously informed by using other (very polished) industrial tools and I am sure that this has impacted and will impact my experience with using Idris, and cause me to compare it to such tools. Any criticism I have that stems from this comparison is not me saying "Idris has failed to do *X*", but instead something more like "Wow, wouldn't Idris be *even better* if it did *X* **too**".

Please contact me if you wish to discuss anything I bring up in this post.

# Solving Advent of Code in Idris

The rest of this post will more or less be my blow-by-blow account of the joy and stress I have encountered while tackling the Advent Of Code using Idris. I started writing this document shortly after solving Day 3, so comments on Day 4 and onwards will be in-the-moment, while earlier ones may be reflective. 

You can find my solutions [here](https://github.com/AdamHarries/advent-of-code-2020).

# Day 1

<!-- Solution: [Day 1](https://github.com/AdamHarries/advent-of-code-2020/blob/main/Day1.idr) -->

Solving Day 1 was overall straightforward. I did, however, immediately encounter some pain points in the tooling and documentation "around" the language: 

- Firstly, I found that I couldn't compile or install the actual idris2 executable from source. I'm not entirely sure why this was, however deleting a previous install and re-cloning the repository seemed to work. 
- Getting set up with an "ide-like" experience was quite frustrating. I don't use Emacs or Atom, and wasn't interested in learning either in order to use a specific programming language. Luckily, VSCode (my "productive" editor of choice) has an extension available that works with the interactive editing protocol that Idris provides. The extension (or the tools it uses under the hood) are slightly buggy however, and the extension seems designed to be used with Idris 1 (though it seems to work with Idris 2). One example is that commands (such as "case split") sometimes fail to do anything, and only work on a second attempt.
- There doesn't seem to be much documentation for how to compile Idris code, or how a project should be set up. I ended up going with a [Makefile](https://github.com/AdamHarries/advent-of-code-2020/blob/main/Makefile) which seemed to work quite well, but presented some other problems that (as I understand it) Idris' native `.ipkg` format is designed to solve. Unfortunately, I couldn't find much documentation for the latter, and it was only after looking into the source for the actual Idris compiler that I got an idea of how I could use one to compile the various source files that I had for the probelms. 

Once I got everything set up and working, I was able to quite quickly get started solving. The main logic of the problem was easy to implement, but I had difficulties in implementing the actual IO code to read in the file and transform it into a list of integers. To start with, here was the solution I came up with (taken from [`Util/Parsing.idr`](https://github.com/AdamHarries/advent-of-code-2020/blob/main/Util/Parsing.idr)): 

```Idris
readAsIntegers : String -> IO (Maybe (List Int))
readAsIntegers fname = do
    contents <- System.File.readFile fname >>= (pure . getRight)
    pure $ (contents >>= (\s => sequence $ map parseInteger (lines s)))
```

There were a few things that I found difficult while writing this: 

- As mentioned before, the interactive editing is still rather buggy, which meant that checking the type of intermediate values was difficult. For example, while trying to simplify the code, I wasn't able to to check the type of `contents` without manually starting the Idris repl and copying the code in verbatim.
- The Repl is annoying in a number of ways: It doesn't support "arrow keys"[^arrowkeys] for editing/reusing previous lines, and a number of Idris statements, such as `import Data.Vect` require repl-specific commands such as `:module Data.Vect`. As the repl is not in the IO monad, printing things is quite annoying: The default behaviour is to dump a syntax tree of the actual Idris expression, rather than evaluating it. 
- Finding the types or definitions of standard library functions (or even whether or not they actually exist!) is a huge pain that requires searching through the source code of the standard library. As I understand it, it should be possible to generate documentation [but it is not currently happening](https://www.reddit.com/r/Idris/comments/k25r06/where_can_i_find_standard_library_docs/gds9339). I think this would be a huge win for productivity in Idris, as it was a pain point that I felt repeatedly while using the language. Even having the ability to generate documentation locally would be a huge help, even if it is not hosted elsewhere. 

Overall, however, I really enjoyed solving the first day's problem in Idris. Although there were a lot of rough edges tooling-wise, the language felt fresh and well thought out, and it was surprisingly fast! I've been itching to try and do some performance optimisation on Idris for a while (Rust backend anyone?), but from actually using it I'm not sure it's really that necessary. I found the strange inability to refer to definitions later in a file quite annoying (I like to put my `main` function at the top, and build a dependency tree down the file), but judicious (if nasty) use of `mutual` solved that. As I understand it, the requirement that definitions are in a specific order is related to the way Idris' typechecker works, and required for type-checking a dependently typed language, so I can easily forgive it! The only other (minor) gripe I had was a lack of automatic formatter for Idris code. As a newbie, I feel like I'm almost certainly writing very non-idiomatic code, and formatting it really weirdly (when I even bother to!). Having an auto-formatter would make everything a lot more readable and consistent, and definitely speed up my development. 

# Day 2 

With Day 2, things started to kick up a notch. The problem here involved taking a list of "passwords" with "constraints" and validating that the passwords were valid within the constraints. That meant taking a file that looked like: 

```
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
```

and counting the number of valid passwords. 

The first task here was to write a parser for a constraint/password pair. As I'm apparently a huge masochist I decided to try and use Idris' `contrib` (third-party libraries bundled with the language) [`Text.Lexer`](https://github.com/idris-lang/Idris2/blob/master/libs/contrib/Text/Lexer.idr) and [`Text.Parser`](https://github.com/idris-lang/Idris2/blob/master/libs/contrib/Text/Parser.idr) libraries. Although I think this was the right tool, it was also a *huge* pain, and almost caused me to give up on Day 2 entirely. 

To start with, I chose to use these specific libraries as I was confident of their quality and that they would play nice with Idris 2. My basis for this was that **they are what the Idris 2 compiler uses**, so they should *definitely* work with the newer version of the language[^selfhosting], unlike the other existing third-party parser combinator libraries available which are mostly written in Idris 1 and thus not guaranteed to work with Idris 2.

Using the libraries, however, was incredibly painful. Like most other Idris 2 libraries they are under documented (and documentation is only viewable in the source code, or repl as I later discovered), and I found working out what parts I needed, and in what order, to be a difficult endeavour. I did, however, quickly find [this brilliant tutorial](http://docs.idris-lang.org/en/latest/parserLibrary/) on using the library which was enough to get me to a working lexer. For the parser[^directparsing], however, the tutorial quickly diverged from what I was looking to implement, and I ended up looking through the source code of the Idris compiler to work out how to use the parsing library. My lexer/parser combination ended up being far more verbose than I was hoping for, but once I managed to conquer it I was much happier using the libraries for Day 3 as well. 

Outside of the direct in-language use of the libraries, I also found it quite difficult to tell the Idris compiler that I actually *wanted* to use the libraries. I eventually discerned that there are two ways to do this: 

- If you have control of the actual command that invokes the compiler (such as in a `Makefile`), you need to pass the `--package contrib` flag to specify that you wish to also use the `contrib` package.
- If you *do not* have control of the command (for example, in a situation where your editor invokes the compiler for interactive editing), then the only solution is to write an Idris package which specifies the dependencies, such as: 

```
package advent_of_code

depends = contrib

sourcedir = "."
```

The latter case is especially non-obvious, and I am yet to find any good documentation for the Idris package format. I managed to cobble the above together by referring to package definition files from the Idris compiler itself, and I'm quite surprised that it works. I was

I was especially surprised that I needed to specify that I wanted to use the `contrib` package, as it appears in a similarly privileged position in the Idris 2 repository to the `network`, `base`, and `prelude` packages - the latter two of which do not need specifying as dependencies[^network]. Moreover, it is installed along with the latter two libraries to the main Idris directory, and appears to be used as much as them - for example by the compiler. This was overall quite confusing for me, and I didn't see any obvious documentation (though I may have just missed it!) pointing out the necessity of listing it as a dependency. 

Overall, as with day 2, the actual implementation of the problem logic was fairly straightforward, but I was slowed by a lack of documentation with the tools and libraries that I wanted to use. Once I got everything working, however, I was very very proud of the result and I definitely managed to learn a huge amount in doing so - which is the main goal anyway!

# Day 3 

Day 3's problem presented me with my first opportunity to *properly* use dependent types, as my solution involved indexing into vectors. Idris provides the `Vect` data structure which holds it's length as part of the type, allowing the user to prove properties about it and about uses of it. For example, the `Vect` structure allows the following function for indexing: 

```idris
index : Fin len -> Vect len elem -> elem
index FZ     (x::_)  = x
index (FS k) (_::xs) = index k xs
```

Note the type of this function: It takes a `Fin len`, and a `Vect len elem` and returns an `elem`. This means that it takes a "Finite" number bounded by `len` (i.e. we know that it will be less than `len`), and a vector of length `len` with element type `elem`, and returns an element. The beauty of this type is that we know that *every* permissable value of type `Fin len` will be a valid index for the vector as they are guaranteed (by the type system) to be less than the length.

Idris goes a long way towards making dependent types ergonomic and usable for real-world programs[^adventcalendar] but it does occasionally slip up and fail to help when the compiler needs help itself. One place where I found this was while implementing a function to tranform a `Nat` (Natural number) `n` into a possible index `Fin len` by computing the modulo `n % len`[^modull] which is guaranteed to be a valid element of `Fin len`. My first attempt went something like this: 

```idris
-- Set a concrete bound, minus one
lenMo : Nat 
lenMo = 30

-- Set a concrete bound in terms of len - 1
len : Nat 
len = S lenMo

-- Calculate an index bounded by `len` from `n`
ix : Nat -> (Fin len)
ix n = restrict (lenMo) (cast n)
```

We use the idris-provided function `restrict : (n : Nat) -> Integer -> Fin (S n)`, which does almost what we want: it converts an `Integer` to a `Fin` using modulo, but with a slightly different bound to what we want. That's no problem however: we define the bound that we want in terms of the *successor* of some other value (in this case `lenMo` - len "Minus One"), and use them to fill out the values, and thus hopefully the types.

Unfortunately, our optimism is misplaced: 

```
Errors (1)
/home/adam/projects/advent-of-code-2020/ActionReplay.idr:22:8
While processing right hand side of ix. When unifying Fin (S lenMo) and Fin len.
Mismatch between: S lenMo and len.

/home/adam/projects/advent-of-code-2020/ActionReplay.idr:22:8--22:33
    |
 22 | ix n = restrict (lenMo) (cast n)
    |        ^^^^^^^^^^^^^^^^^^^^^^^^^
```

Hmm, so Idris cannot unify `Fin (S lenMo)` and `Fin len` - that makes sense, *we* know that `(S lenMo) = len`, but does Idris? And does it have any way to push that information inside the `Fin`? Let's try and write a proof of the equality, and then try and rewrite it: 

```idris
prf : (S lenMo) = len 
prf = Refl
```

```
Errors (2)
/home/adam/projects/advent-of-code-2020/ActionReplay.idr:22:7
While processing right hand side of prf. When unifying len = len and S lenMo = len.
Mismatch between: len and S lenMo.

/home/adam/projects/advent-of-code-2020/ActionReplay.idr:22:7--22:11
    |
 22 | prf = Refl
    |       ^^^^
```

Err. What? But we know that they're the same, don't we? Well, dear reader, we do - and so does Idris! However, we can't directly write this proof down (I'm not sure why, maybe it's too simple?), but we can ask Idris to *search* for it. To cut a long story short (and ignore some other diversions that were of my own doing), this is what I ended up with: 

```idris
-- Set a concrete bound
len : Nat 
len = 31

-- Calculate an index bounded by `len` from `n`
ix : {lenMo: _} -> Nat -> {auto prf: len = S lenMo} -> (Fin len)
ix n = rewrite prf in restrict lenMo (cast n)
```

Essentially, we give the `ix` function an implicit argument `lenMo` in the type, and also ask it (through the use of `auto`) to *search* for a proof that `len = S lenMo`. Given that proof `prf`, we can *rewrite* the type of our restrict using the proof to demonstrate equality, which gives us the right type. 

Phew. 

Overall, I found this really tough to implement. Firstly, I'm still getting to grips with dependent types and proof assistants which meant that there were quite a few *"WHY WON'T YOU BLOODY WORK, IT'S OBVIOUS THEY'RE EQUAL!!"* moments. Secondly, features such as automatic proof construction seemed obscure and I only found in an (now forgotten) stack overflow answer. This is something where I think more ergonomic compiler error messages would be very useful. For example, adding (in the original error) a suggestion to add a proof search if Idris can unify the two expressions automatically would go a long way towards pointing out features that a user might miss. *(Edit: I have since found [this documentation](http://docs.idris-lang.org/en/latest/tutorial/miscellany.html?highlight=auto%20#auto-implicit-arguments) which explains exactly what I should have been using. It's definitely my fault for not finding this documentation, but my comments about error messages still stands.)*

As with the parsing and lexing libraries, this exploration into the proving side of dependent types has definitely taught me a lot, and I'm sure I'll be able to put it to good use later. Unlike with the libraries, however, I'm not sure how I could have learned it any faster, or got out of the problems any faster. More up to date documentation (and by more, I mean *quantity*, not *recentness*) on writing proofs in Idris 2 would definitely be useful, and more verbose compiler errors with suggestions a-la the Rust compiler would also be very helpful, but both are very heavyweight solutions that I'm not sure would really pay off.

*Edit: Since writing this, I've found a number of other pieces of documentation that I hadn't come across, such as [this page](http://docs.idris-lang.org/en/latest/tutorial/miscellany.html?highlight=auto%20#auto-implicit-arguments), which explores auto implicit arguments. In many ways, this is exactly the up-to-date documentation that I was asking for above, however it wish it was closer to the documentation on theorem proving, as it's quite a central aspect to that it seems.*

# Day 4

Day 4 was my first try at using Edwin's concept of "Type-driven Development", and with Idris it is a really pleasant way to develop! I started off by defining a very simple token type for the various possible input fields, parameterised by strings, that looked something like this: 

```idris 
data PassportEntry = BirthYear String 
    | IssueYear String
    | ExpriationYear String
    | Height String
    | HairColour String
    | EyeColour String 
    | PassportID String 
    | CountryID String 
```

However, I quickly found that having explicit constructors for every kind of entry that I needed got quite cumbersome. Enter dependent types!

```idris
keys : List String
keys = ["byr","iyr","eyr","hgt","hcl","ecl","pid","cid"]

record PassportEntry (key: String) where 
    constructor MkPassportEntry
    value : String 

export 
implementation {k: String} -> Show (PassportEntry k) where 
    show e = k ++ ":" ++ (value e)
```

This gives us (essentially) exactly the same implementation, but with a much nicer path for implementing functions where we want to be able to print the key for the entry. Instead of having to pattern match over every constructor, we can instead directly use the string that we use to parameterise the type `PassportEntry`, and use that to fill out functions like `show`.

Not five minutes later, I realised that this was a terrible idea, as it forced us to make every list of `PassportEntry` have a hetrogenous type (e.g. birth year), which is counterproductive. Instead, I'm just going to store the key as a string in the record, i.e.: 

```idris
keys : List String
keys = ["byr","iyr","eyr","hgt","hcl","ecl","pid","cid"]

record PassportEntry where 
    constructor MkPassportEntry
    key : String
    value : String 

export 
implementation Show PassportEntry where 
    show e = (key e) ++ ":" ++ (value e)
```

This has basically the same utility5, but without the typing issue. 

```C
// TODO: Not implemented yet
```

[^constructive]: Constructive criticism is closely related to intuitionistic criticism which is essentially classical criticism without the law of the excluded middle. 
[^arrowkeys]: I'm not sure what else to call it - when you push up in a terminal or repl, and your previous line appears. 
[^selfhosting]: Idris 2 is a self-hosting language, meaning that the compiler is written in the language that it compiles. There is also a "bootstrap" compiler, written in Scheme for those without an existing Idris 2 compiler (presumably a vanishing minority of software engineers by now).
[^directparsing]: I didn't need to separate the parsing and lexing stages - other solutions (such as [this one](https://github.com/JoeyEremondi/aoc-2020-idris/blob/main/Day2a.org#parsing-the-input)) parse directly, but as I was new to the libraries I decided to stick with the lex-then-parse approach that they and the tutorial took.  
[^network]: I have not yet used the `network` package, so it may require the same as `contrib`.
[^adventcalendar]: Which toy advent calendar problems 100% definitely are.
[^modull]: I feel like this `Nat -> Fin k`, implemented via modulo should be something that is in the `Fin` package in the first place - it seems so obvious to me!