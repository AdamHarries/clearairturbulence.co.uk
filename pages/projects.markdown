---
title: Projects.
---

## [Ellington](https://github.com/AdamHarries/ellington)

<div style="text-align: center;">
<script src="https://asciinema.org/a/uqpZZU0DXZSdmRk1ddRXXOmLg.js" id="asciicast-uqpZZU0DXZSdmRk1ddRXXOmLg" data-cols="80" data-rows="14" data-autoplay="1" data-preload="1" data-speed="2" data-theme="monokai" async></script>
</div>

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

![](https://github.com/AdamHarries/clearairturbulence.co.uk/raw/master/images/projects/shagpins/pin.jpg "A collegiate shag pin")

Collegiate Shag is a somewhat niche swing dance, with a small community even by the standards of the small swing community. In order to make it easier to find Collegiate Shag dancers at general swing events, I commissioned a run of pins that dancers could wear to indicate that they dance Shag.

[*Originally posted on instagram.*](https://www.instagram.com/p/Bb-EVGyAvwC/)

***

## [Custom Leather Phone Case](https://clearairturbulence.co.uk/writing/basic-leatherwork.html)

![](https://github.com/AdamHarries/clearairturbulence.co.uk/raw/master/images/projects/leatherwork/phone_case_finished_smaller.jpg "phone_case_finished")

My old phone case fell apart, so I decided to make a new one! I'd wanted to do more physical projects for a while, and leatherworking sounded interesting, so it sounded like a good project to start with.

[*I've written up my process here, along with snapshots of another small leather project that I made.*](https://clearairturbulence.co.uk/writing/basic-leatherwork.html)

***

## Professional/work projects

When I'm not procrastinating, I do things for people in exchange for financial reimbursement. At one point that was studying for a PhD, which I have now left, and I now exchange my typing and thinking skills for money at Codeplay, a smallish software engineering firm in Edinburgh. 

Here are some of the projects that I worked on. 

### Lift

> Lift is a high performance functional programming language, designed as a solution to the "performance portability" problem, and targeting hetrogenous parallel programming systems. 

I worked on Lift as part of my MSc and (withdrawn) PhD at the University of Edinburgh. Lift is designed to solve the problem of performance portability across accelerator platforms. Performance portability refers to the problem of writing code that is both cross platform (i.e. it will run correctly across different hardware platforms), and also performant across platforms. 

Simple "portability" is a solved problem - we have numerous high level languages for day to day computing, such as C, C++, Haskell, Agda etc, as well as portable languages for accelerator programming such as OpenCL and SYCL. These high level languages act as a common interface to the various flavours of computing systems. It is possible to write code in one of these languages (say, C++), that will run on a desktop running linux on x86, on windows running on ARM, or even more esoteric varieties. Some parts of the code will have to be specialised, but in general portable code is now fairly standard. 

Performance portability, however, is a much more difficult problem to solve, as to understand how to write performant code entails understanding the machine that the code will run on, rather than just the interface the machine provides. This means that the same code written in (say) OpenCL will have vastly different performance characteristics when running on an AMD GPU, or on a NVIDIA GPU. This presents a big problem, as our aim with using a GPU (or an other co-processor or accelerator) is to increase the speed of our code. If we cannot guarantee performance, or even achieve it in the first place, then the whole effort is pointless. 

Lift aims to solve the problem of performance portability by compiling code using *algorithmic rewrites*. Put simply, the Lift compiler essentially generates hundreds of thousands of potential program "implementations" for every program compiled, and searches for one that will perform the best on a given piece of hardware. 

Lift is still in early development, and is not production ready, but the hope is that the rewriting process, guided by a sufficiently knowledgeable searching scheme will allow it to solve the performance portability challenge by providing a common interface across hardware, and generating specific implementations for each specific platform. 


### Sycl parallel stl

> Sycl implementation of the C++ Parallel STL

### SYCL-BLAS

> Sycl implementation of the netlib BLAS routines
