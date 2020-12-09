---
title: Gender neutral language is just polymorphism in human communication. 
tags: programming languages, teaching, language, people
date: 2018-09-13
unfinished: true
abstract: 
---

In computer science, there's a concept called "polymorphism". It's a complex topic that discusses methods by which programs can accept (and return) a variety of sets of values, rather than a singular set. In a programmer's day to day life, however, polymorphism can be pretty simple. Let's take a look at a bit of C++ code to help explain it. We're going to try and write the "simplest" program (technically a function) possible - one that takes a value, and returns the same value that it was given: 

```lang=C++
int identity(int value){
	return value; 
}
```

We can use his program like this:

```lang=C++
int main() { 
    int a = 10; 
    int b = identity(a); 
}
```

All this does is assign the value `10` to a variable `a`, then use our `identity` program to assign the value from `a` into `b`. Let's ignore how unnecessary `identity` is in this program. 

Unfortunately, our customer has changed their requirements for our program - they've decided that they no longer want `a` and `b` to be integers, they want full strings now so that they can pass text around their programs: 

```lang=C++
int main() { 
    std::string a = "ten"; 
    std::string b = identity(a); 
}
```

Great! We've changed our program so that we use strings now. Let's try and compile it: 

```
<source>: In function 'int main()':

<source>:9:30: error: cannot convert 'std::string' {aka 'std::__cxx11::basic_string<char>'} to 'int'

    9 |     std::string b = identity(a);

      |                              ^

      |                              |

      |                              std::string {aka std::__cxx11::basic_string<char>}

<source>:3:18: note:   initializing argument 1 of 'int identity(int)'

    3 | int identity(int value){

      |              ~~~~^~~~~

Compiler returned: 1
```

Drat! Errors! These errors are extra C++-ish, but (in brief) they state that we can't use a `std::string` (aka a string) when the function `identity` is expecting an `int`. 

This is because our `identity` function is too "monomorphic" for our use case. The way it's currently written it, it can only accept or return `int` values, so when we try and use it with a string value our type system rejects it. We can fix this, however, by modifying our identity function to be "polymorphic" and accept and return sets of values: 


```lang=C++
template <typename T>
T identity(T value){
	return value; 
}
```

Although this function looks a little more complicated, it's really just syntax. What is says is that our identity function can now take a value of "type" `T`. In our first example, `T` would be set to `int`, while in our refactored code, it would be set to `string`, and our function would still work in both cases. 

So how does that relate to gender neutral language? 

Well, gender neutral language is language that is /polymorphic/, i.e. it's language that works with any gender, and is not constrained to any gender in particular. 


.....
