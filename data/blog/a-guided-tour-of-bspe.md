# A Guided Tour of "Bayesian Spectrum Analysis and Parameter Estimation"

For too many years than I would like to admit I have tried to use Bretthorst's Phd disseration ["Bayesian Spectrum Analysis and Parameter Estimation"](https://bayes.wustl.edu/glb/book.pdf) to implement a music visulizer. Ultimately I never get the point where the code is usable and then I move onto something else. By the time I come back I forget how most of it works and end up starting over.

Sigh. Once I am starting over.

However this time I hope that by publishing notes on the disseration I will burn it into my brain. I might even have the patience to reread my notes but if the past is an indication of the future, that is a rare occurrence.

## What is the Question

Suppose you have a some data and it appears to be at least quasi-periodic. You also know that there is some noise in the data that is probably not periodic.

We can start by specifying that our data at time `t` is given by the some model function `f` offset by a noise function `e`

```
d(t) = f(t) + e(t)
```

Picking a reasonable error functions can be important but for now we will just assume as is customary that is gaussian noise.

It would be really cool if there was a general way to figure out what the `f` should be without making any assumptions but sadly that is an open question.

To simplify our problem we will have to make some assumptions.

A not completely insane hypthosesis is the data is generated from a sum of sine and cosine function with some noise.

```
d(t) = a*cos(w*t) + b*sin(w*t) + e(t)
```

If we assume this is an appriopiate model then the next question becomes "given the data and the model, what are the most likely values for the parameters a, b, and w?"

This might seem like a simple question but it has a very fraught philisophical history.

What does it mean for something to be "likely"? Is this even a well formed mathematical question.

This is the question Bretthorst sets out to answer. Well, actually his advisor had already answered it but Bretthorst does so in more detail and generalized this simple case in many interesting ways ... and he shows how to extend the model more generally which is why I am interested in this method.

Before I try to answer the question of what makes something "likely" mathematically I think it is best to build our inituition around some choices of `a`, `b`, and `w` make some common sense arguments for why some might be more "likely" then others.

## Knob Twiddling our way to Understanding

Step 1 is we will try a smattering a choices for `a`, `b` and `w`. By playing with the values you can try to eyeball the best fit.

TODO make a widget to find the best values

For me personally I found the following parameters fit the best

```

```

No choice is going to fit the data perfectly because of the noise. The goal is choose parameters that in general overlap the data well but some points are going to be slightly off.

Make no mistake, this is subjective criteria. The reason we think certain values are more likely than others is because certain choices in general "seem" like our data.

One way to start to make these decision more percise is the measure the distance we are off from the actual data at each time

TODO show that

Now if you play with different values you can see that the choices affect the distance. We can now make a percise definition of what we are trying to optimize.

> Choose values such that the sum of distances are the least.

The difference between the actual and expected value is called the "residual". The idea of trying to reduce this value is the key component of the "Least Squares" linear regression technique.

However this is where things get complicated. Least squares tries to minimize the sum of the _squared distance_. Here I am suggesting we can merely minimize the absolute value of the differences.

The two approaches are not equivalent. The least squares approach is the correct one but it is far from obvious why.

Historical the least squares approach was used in linear regression because it could be computed efficently and seemed like a good idea. There were many attempts to explain why it was a good idea over the next 150 years after it was invented.

I believe the "Bayesian" approach probably provides the best explaination.

## The Bayesian Way

The bayesian way involves creating a likihood function that assigns a value to the residuals based on the error function:

```
p(d|a, b, w) = product of Normal distribution of differences
```

So the reason we want to minimize the square difference is because we _assumed_ are noise was normal distributed.

If we assumed our noise was exponential distributed we would want to sum the absolute differences.

This is the advantage of the bayesian approach from my perspective is that it makes it clear what the assumptions are. Additional we are free to change those assumptions and then get a different liklihood function.

## Least Squares is to line fitting as the FFT is sinusidial fitting.

Least Squares is the workhorse for finding the best line to fit some data. We can start from more complex bayesian models are rederive it as a special case.

In the periodic world there is a similar phenomon with the FFT.

The simplest way to find a good sine and cosine function that approximates a sequence is to take the FFT and find the frequency band that has the highest magnitude.

This is actually works really well and there was a period of time after James Cooley and John Tukey published their results on the FFT that this was considered the state of the art.

There is nothing wrong with this approach per se. It is usable.

However it is hard to generalize it to more complex models and it is not actually the most accurate way to fit a sinusodial.

In same way one can setup a likihood function after assuming a noise function for line fitting one can do the same sine fitting.

The more complex method reduces to the FFT approach in some simple circumstances but in general works better. Additionally it includes accuracy estimates and the ability to compare the probability of one model to another. In general it is just better, albiet slower.

This is what Bretthorst shows initially. Let's walk through it.
