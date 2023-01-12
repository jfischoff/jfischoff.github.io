In the idealized version of software development, programmers are applied mathematicians. The development process starts with a rigorous specification. Then the software is written and there is a proof that demonstrates the code implements the specification.

This conception of programmers, probably originates from the 60s, when software development practices were disseminated through academic papers and programmers were seen as PhD math geniuses.

As the discipline of programming moved out of academy and was adopted by hobbyists, this view changed. Programmers were no longer mathematicians; they were builders.

Programming went from a science to a craft. Millions learned to program and software transformed the world.

In this transition, something was lost. Programs went from algorithms that could be verified, to large systems, with no specification that could only be understood through their emergent dynamic behavior.

We traded limited functionality we could understand deeply, for wide-reaching, powerful tools so complex we can't ever hope to completely comprehend them fully.

## Neural Networks are a Craft

Piggybacking off fast matrix multiplication via modern GPUs, neural networks have become the state-of-the-art solution to numerous difficult problems. What they lack in fundamental rigor, they have made up in speed and flexibility.

From the outside, AI seems like a complex mathematical endevour. For one, everyone seems to hold a PhD that makes them. They use complex math like tensors and stochastic gradient descent. New developments are published in academic papers, etc, etc. It is all very reminiscent of 1960s software development. There is an academic pretense around AI methods that makes them seem more complex than they really are.

I think what we are seeing now with AI is similar to what happened in the 80s and 90s with programming in general. Instead of having a few PhDs solve a limited number of simple problems very precisely, the floodgates have been opened to a wide swath of the world, to solve a larger number of problems.

Again, we are making a trade. In theory, we can get the best results by carefully deriving a custom mathematical model and then using something like Hamoltian Monte Carlo to fit the parameters, but this difficult and doesn't scale. We trade an understandable model that works incredibly well for simple problems, for a more practical approach we can use to solve a much larger number of ill-posed problems well enough.

## FAFO

Neural networks are less a precise mathematical solution to a problem, and a more fuzzy way to program. They're useful when errors are tolerable, and it is not clear how to write a direct algorithm. Just like with regular programming, there often isn't a right answer. There is merely a solution that might work for your purposes and to know for sure you have to try it out.

[Home](../index.html)
