---
title: Projects.
---

## [Ellington](https://github.com/AdamHarries/ellington)

> An experimental testbed and tool for tempo detection of swing music, named after the legendary band leader Duke Ellington.  

As might be evident from my writings across this site, I'm quite a big fan of swing dancing, and the swing music that underpins it. One of the ways that I get involved in swing dancing is as a *DJ*, where my responsibility is to queue up tracks for attendees at a dance. Unfortunately, dancers are a picky bunch, not unlike goldilocks, so finding the perfect music for them can sometimes be a challenge. 

One of the key components for picking a song is the Beats Per Minute (BPM), or the tempo of the song. Too many songs at too high a tempo, and dancers will quickly become exhausted, hot, and sweaty, and stop dancing. Too many songs at too low a tempo and dancers will become bored, and the sluggish music will drain the energy from the room. 

Swing DJs generally tend to calculate the tempo of their tracks by hand, either by tapping the beat using a tool (such as the one at [all8](https://www.all8.com/tools/bpm.htm)), or by manually counting the number of beats in a minute. This is a fairly laborious task. Generally, each track will take upwards of a minute to "bpm", which for a large library can quickly become a huge task. 

Automated solutions for tempo calculation *are* available, but they (unfortunately) do not work well for swing music. This is generally due to the irregularity with which swing music accents its beats (see [the essence of swing](writing/the-essence-of-swing.html) for more information). Most tools that calculate tempo assume that every beat receives the same accent, and importance, and using the time between each detected beat, calculates the tempo. Swing music, however, generally accents the second and fourth beat of a bar, which often leads to confusion, as tempo calculation algorithms assume that they are the *only* beats, leading to a tempo that is twice as slow as would be correct. 

*Ellington* is a tool, and test-bed, for tempo detection algorithms that do not suffer from this limitation. Specifically, it aims to provide a complete tool for calculating tempo information for a track, or whole library, as well as a modular system to allow developers to experiment with new variants of tempo detection algorithms. 

The tool is written in the [Rust programming language](https://www.rust-lang.org/en-US/), partly for reasons of performance (tempered by safety!), and partly because I finally had a project that would give me an excuse to try it out. 

[*Ellington can be found on Github.*](https://github.com/AdamHarries/ellington)

***

## [Bellson](https://github.com/AdamHarries/bellson)

> Bellson is an attempt at using deep convolutional neural networks to infer the tempo structure of swing music. Named after Louis Bellson, one of Duke Ellington's drummers. 

Bellson is designed to be a component of Ellington. 

- Written in python

- Want to port to rust/C++

[*Bellson can be found on Github.*](https://github.com/AdamHarries/bellson)

***

## [Collegiate Shag Pins](https://wwwi.nstagram.com/p/Bb-EVGyAvwC/)

![](/images/projects/shagpins/pin.jpg "A collegiate shag pin")

Collegiate Shag is a somewhat niche swing dance, with a small community even by the standards of the small swing community. In order to make it easier to find Collegiate Shag dancers at general swing events, I commissioned a run of pins that dancers could wear to indicate that they dance Shag.

[*Originally posted on instagram.*](https://www.instagram.com/p/Bb-EVGyAvwC/)

***

## Other projects

### Lift

> Lift is a high performance functional programming language, designed as a solution to the "performance portability" problem, and targeting hetrogenous parallel programming systems. 

### Sycl parallel stl

> Sycl implementation of the C++ Parallel STL

### SYCL-BLAS

> Sycl implementation of the netlib BLAS routines
