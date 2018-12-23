---
title: Tests and Types, Science and Maths. 
tags: maths, types, science, testing
date: 2018-12-06
unfinished: true
abstract: It occurs that the oft professed dichotomy between types and tests is anything but. In fact, their relationship bears quite a close resemblance to the relationship between maths and science. This post gives a brief overview of the similarities, and argues (as many do), that neither tests, nor types, are sufficient on their own. 
---
I imagine that it would take no more than about ten minutes of searching on twitter to find strong arguments for and against either type or test driven development. The former being the development of software guided and checked by powerful type systems, and the latter the development of software guided and checked by exhaustive test suites. Of course, in many cases, those expounding those opinions will often go further, and claim that their way is the *only* way to achieve software correctness (or reliability), and that the other way is not just ineffective, but *slow* or *bad* or *dangerous*. 

After you're done searching, I want you to do another search, and see if you can find similar levels of emotion and vitriol being expressed by natural scientists about *mathematical models* and *experiments*. I suspect it might be hard. 

I would like to argue that just as the vast majority of natural scientists accept both mathematical models and experimentation without a seconds hesitations, (as well as simulation, of course) us computer scientists should enthusiastically and readily adopt both strong static types, and tests. Furthermore, I think there is a natural correspondence between models and experiments, and types and tests.  Examining this correspondence should hopefully give rise to some good arguments for accepting testing and types together, in the same way that models and experiments are accepted.[^why]

## Prediction.
The purpose of science is to make falsifiable predictions[^science]. "This ball shall fall to the ground if I let go of it" might be one, or "Bacteria of the strain E. Coli when grown under such-and-such a condition, with some-specific antibiotic will exhibit a growth curve that looks like[....]". 
In the former case, this is an experimental prediction: by repeating some experiments a number of times and observing the result we can (to some degree of accuracy) make an educated guess as to what will happen when we repeat the experiment. In the latter case, we have some predictive *model* that allows us to make educated guesses: it gives a guide to what will happen in some set of circumstances where we may not have prior observations, nor complete ones. 
The two cases (in science) are considerably intertwined. It is profoundly difficult to create meaningful models without experiments, and it is just as difficult to draw useful conclusions from experiments without some sort of model that you wish to test. In some cases, simply a model, or a set of experiments may suffice. The vast majority of the time, though, both experimental methods and modelling are intertwined and reliant upon one another. 

### Software prediction. 
I would argue that tests and types serve a similar purpose when writing software. They serve to provide the means by which we can make falsifiable predictions about a given piece of code. 
"This method shall return the average number '4' when given the array of numbers '[2,3,4,5,6]'" and "When given any two vectors Xs and Ys, of length N and M, this method shall return another vector of length N+M" are both falsifiable predictions we could potentially make about code. 
In the former, we would generally use a *test* to assert whether our prediction is true or false. This might look something like this: 

```C++
TEST(AverageTest, ResultCorrect) {
  std::vector<int> v{2,3,4,5,6};
  int average = std::accumulate(v.begin(), v.end(), 0) / v.size();
  EXPECT_EQ(average, 4);
}
```

The second prediction is one that could be validated using a powerful enough *type system*. An example, from the *Idris* language is given as follows: 

```Idris
append : Vect n a -> Vect m a -> Vect (n + m) a
append Nil ys = ys 
append (x :: xs) ys = x :: append xs ys
```

Tests and types, I believe, are roughly analogous to experiments and models in natural sciences. Tests, like experiments, give a *guarantee* that given some specific set of circumstances will result in another set of circumstances in a given system. Types, like models, give a *guide* as to what will happen over a complete set of possible circumstance.

The ability to make falsifiable predictions about software is vital for developing safe, secure systems. If we are unable to predict what our software will *do* in some set of circumstances then letting it loose on the world would be irresponsible and unethical. The question, therefore, is by what means shall we try to make falsifiable predictions? This is arguably a question best answered by a philosopher of science, but we can look towards science to see the methods that have served it well for roughly half a millennia. 

## The power of predictions. 

Let us look at experimentation and through that, examine the power and role of tests. Experiments are incredibly powerful, yet incredibly limited in their predictive power. And experiment lets us say definitively (within some statistical degree) that given preconditions X, situation Y *will* (or won't) occur. By analogy, a software test holds the same level of predictive power and limitation: A test asserts that given some preconditions (or parameters) X, a method/function/statement will (or won't) result in value(s) Y. 
Neither tests, nor experiments, give us any indication as to what will happen under some other circumstances Z. A test/experiment is tied to its specific set of preconditions, X, and if removed from them will lose it's predictive power. Tests and experiment, if not designed well, are brittle. This is the power and limitation of tests and experiments. they are definitive, but inflexible. Glass cannons. 

A mathematical model, by contrast, is an incredibly flexible predictor. A model divorces itself from a specific set of preconditions, instead allowing the questioner to substitute their own, and arrive at a corresponding prediction. 
Type systems are similar in their flexibility: rather than tied to a specific set of preconditions, they (by design) range over specific sets of preconditions, and predict the outcome of each. 

A model, however, is less definite. It may only predict within the constraints of its complexity, and will always be an abstraction of a real system. 

Given the apparent power of models and type systems, one might wonder why anyone would ever write tests, or perform experiments! It would seem that their predictive power is absolute: with a sufficiently advanced typesystem (tm), one could express any set of constraints and predictions, just as one could model the universe from the largest galaxies to the smallest particles with a sufficiently advanced model (tm). 

Models and typesystems, however, are not *definite* in the same way that tests are. A model may only predict within the constraints of its own complexity, and will always be an abstraction of some "real" system. (Write something about physics here?)

As you might have guessed, the idea of a sufficiently advanced model (a theory of everything) is at present ludicrous. By analogy, a sufficiently advanced typesystem would also be ludicrous. (More on this). 

## Predictions in a vacuum.





Maths = types - Create models, verify that the model is consistent with itself, predictive, but cannot capture the relationship with hugely complex systems without becoming overly complex in itself, or becoming a high level abstraction. 

Science = tests - Make assumptions clear, create hypothesis, test hypothesis, describe high level interaction and outcome of the system. Cannot function without _some_ mathematical basis in order to create a model. 

[^science]: I'm sure others will disagree, but I don't care. 
[^why]: One might ask why I'm even writing this. Put succinctly, I enjoy shouting into the void, especially when I'm trying to unite disparate warring tribes that are both as pigheadedly dogmatic as the other, and just **sure** that they are right, and the other is wrong. Life is an exercise in futility, and this post is part of life.  