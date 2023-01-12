# Software is a Craft

In the idealized version of software development, programmers are applied mathematicians. The development process starts with a rigorous specification. Then the software is written and there is a proof that demonstrates the code implements the specification. Dijkstra would be proud.

But in the real world, this is bullshit.

In the real world, maybe there are requirements, then the software is written, maybe it is tested and only after being used for a long time do we learn what the code actually does.

We understand code through its emergent runtime behavior because non-trivial systems are too complicated to statically analyze comprehensively.

In the past, software development ideas were expressed through academic papers, but this changed in the 80s and 90s as software development started to explode. People become expert programmers, not by applying theory, but by doing it. It turns out you can become an expert programmer merely by writing a lot of code.

Programming went from an academic discipline, to a craft. You become a good programmer by doing it. We design and test software to get a sense of what it can do, but we don't really know until it is heavily used by real users.

# Neural Networks are Craft

Neural network development seems like some advanced mathematics. For one, everyone seems to hold a PhD. that makes them. They use complex math like tensors and stochastic gradient descent. New developments are published in academic papers, etc, etc.

I can only imagine this is similar to how software development must have looked from the outside in the 60s and 70s.

Neural networks, which are differentialable pipelines of linear and simple monotonic non-linear functions, are exploding in utility, in part because programmers can try out different pipelines without having to apply difficult mathematical reasoning.

You can play with neural networks and get an intuitive sense for how to use them effectively, even without completely understanding the theory. Neural networks are for hacking, they're not for precise mathematical reasoning.

What we are seeing now, is similar to what happened in the 80s and 90s. Neural networks are escaping the Ivory Tower. You can become a good deep learning practitioner by trying things. If it appears to work, you can see if it works in a wider class of tests. Maybe it works well enough to make money.

Do you know exactly what a neural network can compute for all inputs? No, you don't, which is just like regularly programming. You have deployed it and find out.

# Play First, Understand Later

Neural networks are less a mathematical solution to a problem, and more fuzzy way to program. They're useful when errors are tolerable, and it is not clear how to write a direct algorithm. Just like with regular programming, there often isn't a right answer. There is merely a solution that might work for your purposes and to know for sure you have to try it out.










Neural networks are used to approximate functions. There is a whole cottage industry of function approximation techniques. What is nice about neural networks is their flexibility and speed. It is not that neural networks are fast compared to all approximation techniques, it is more they are fast compared guassian processess and MCMC approaches. If you think of neural networks as approximating a gaussian process, that might help you if you are the one percent of people that understand gaussian processes and not neural networks.

Functions approximation is the backbone of data analysis. Whether linear regression, or whatever, you are trying to

Before we talk about neural networks, we need to talk about the problem they solve.

Some of the trickest problems we would like to solve involve inferring an unknown function. We might not very little about the function other than it is solution to some minimization problem.

This is different then a typical optimization problem, where we have a function and we would like to find it's maximum or minimum.

One of the most famous historical examples of these type of problems is brachistochrone problem. The brachistochrone was a famous 17th century math problem to find the shape of a curve that would cause a ball to roll the fastest down hill.

So in the case of the brachistochrone problem, we have the test to compare solutions, time to for a ball to roll to the bottom, but we don't have the function or curve that maximizes this condition.

These types of problems were increasingly studied in the 18th century and led to develop of the Calculus of Variations. Unlike traditional calculus, which optimizes functions R -> R, the calculus of variations optimizes functionals (R -> R) -> R, which are functions with a mapping to a real number. This allows uses to create optimization problems that return a function, instead of number.

Great so to recap, I said neural networks solve optimizations problems when we need a function, and also we already figured out how to do this several hundred years ago. So why use neural networks? Well the calculus of variations is hard. It involves solving tricky integrals, which is hard to do analytically. Numerically approximating calculus of variation integrals is also tricky, because the space we care about is the space of functions and it is not clear how to best integrate over it.

So the calculus of variations is a great way to solve these types of problems when you can get it work for you, but in practice it is too hard to reliably use. It does produce fantastic answers, and much of modern physics was derived by reframing mechanics as variations problems.

What we need is a work-a-day version of the calculus of variations. Something, which is easier to use even if it is less accurate. Something that uses simple pieces we can put together. This is the gap that neural networks fill.

List some examples.

But wait, neural networks are not alone here as well. We have also developed statistical methods that such as gaussian process that can also infer unknown functions from a objective function.

Okay so why use neural networks? Because they are faster. If we could make other statistical methods as fast as neural networks we would problem use them instead.

So to recap, we have many unknown function optimization problems we would like to solve, and many ways to solve these problems, but only neural networks are efficent enough and simple enough that we can reliably apply them to a wide swath of problems.

Great, so what are neural networks.

Going based on the name, you are problem imagining they are network of artificial neurons, and this was the initial inspiration for neural networks. However, this is not the clearest way to describe them, at least not for me.

Defining neural networks is trickier than you might imagine. Most generally they are computational graph of differential elements. In practice, they are  pipelines of linear transformations interwoven with monotonic pointwise non-linear functions.

L o sigma o L o sigma ...

This is a fancy way of saying a neural network is a series of matrix vector multiplication followed by mapping over elements of vector with a simple function.

matMult m . map f . matMult m . map f

where f is typically `f x = max 0 x` in practice, but could be one of several simple functions.

We call the pair of operations, matrix multiplication and non-linear mapping a "layer". The non-linear part is called an "activation". In general, if there are more than three layers we say the neural network is "deep" otherwise "shallow". If you haven't picked up on it yet, neural network people love to name things.

When building layers, we tend to constrain the linear transformation to some subclass of transformation. For instance if we restrict the transformations to affine transformations we call the layer a "linear layer", a "dense layer" or a "fully connected layer". There are handful of other layer types worth noting. In general, neural network design is the process of connecting up various well known layer types in a pipeline to accomplish a task.

Practioners get an inituitive sense for how the different layer types work and when it is appropiate to use one or the other. In the same way a programmer learns to decompose a algorithm into a langauges statements and expressions, a neural network designer learns to decompose a task into a vocabularly of well known neural networks layers.

So neural network design, like programming in general, is a craft or practice. You get good at it by doing it, not by reading, which is similar to programming in general.

Let's show a simple example.

So, who cares? Like why do this. Why make this construction. Why sandwhich a non-linear function in between linear transformations.

If it was just a pipeline of linear transformation, we could combine all the linear transformations into one. So the non-linear parts enlarge the space of functions the neural network can approximate. An important result shows that with at least two layers, a neural network can approximate any function.

This result is theoritical, and doesn't give any instructions on how to construct these magical two layer neural networks. In practice, deeper networks are often necessary to accruately approximate functions.

Approximating functions with neural networks is a two stage affair. First a architecture needs to be created. This specifies the the shape of the matrices and other constraints on their construction. For instance it can be useful to increase the dimension of the vectors in intermediate layers. Additionally, we might place constraits on the matrices to perform affine transformations or convolutions. We make all these choices a head of time, and then fit the parameters of our neural net. The parameters being the elements of transformation matrices.

Neural networks are pipelines of affine transformations interwoven with monotonic pointwise non-linear functions.

Affine -> Non-linear -> Affine -> Non-linear -> ...

We call a affine non-linear pair a "layer". If we have more than three layers we call the network deep.

For a long time it was difficult to have many layers and neural networks were rather limited and produced disappointing results. The convoluence of many techinologies, automatic differeniation libraries and the rise of fast matrix gpu comuting open the door to more complex neural network architectures that could

Neural networks are pipelines of linear systems like, `Ax + b = y` and a non-linear step, typically `max(0, x)`. The linear part is called the "weights" and the non-linear part is called the "activation." In general we seek an approximation of `A` and `b`, the "weights" and "biases" that minimize and object function, like means squared error.

One of the hardest parts of learning neural networks are the myriad of names for everything. For instance the equation `Ax + b = y` is not exotic and already has a name, it is a linear equation. However, when followed by a element wise clamp to zero, it becomes something more exotic. It is now a "fully connect network", a "dense network", or a "linear block" and probably something other names as well I'm not familiar with.

Also the idea of finding an approximation that minimizes the squared error is a common problem. If we didn't have the `max(0, x) part, it is called the "least squares approximation" and is how you find a best fit line through a scatter plot.

My point so far is that neural networks math sounds more complicated than it is.

That said it is more complicated than vanilla least squares. Because of the non-linear parts, we don't have a simple algorithm to find the answer. Instead we have to "hill climb" to find a local maximum and hope that it in fact a global maximum.

In general neural networks are optimized by finding the derivative of the loss function in terms of the parameters, e.g. the weights and biases. We take small steps in the direction of gradient trying to make the loss smaller, until we decide we have done a good enough job and stop.

Fitting the parameters, or in neural network parlance, learning the parameters or training the network, is a fraught activity. Besides the problem that we can't tell a local maximum from a global maximum (actually not a huge problem in practice), we have to be careful not to train the network too long or we will get a worse result in practice.

In general, mathematical optimization techniques converge to an answer. If you leave the running longer, you get a more accurate answer. This is not how neural networks work. There is some optimal stopping condition and there isn't really a way to know when you have gone past it, without trying different checkpoints in the real world and making a judgement call.

Neural networks don't produce an answer so much as they produce a thing that may or may not be useful. It might sound like I am saying that neural networks are bad, that's not my point. You have to have a different mentality when using them. It is more of an art or a craft than a science. This doesn't mean they can't produce very accurate predications they can. You just won't know in advance if this is the case just from training. The training loss could be low and the real world accurancy could also be low. They're fickle.

The great thing about neural networks is they are built from well understood pieces mainly matrices. We have got very good at optiiziming matrix multiplication, and can do it very quickly on the GPU. So although neural networks have many disadvantages, the are flexible and fast. It is worth a shot to see if they can solve a problem, because if they can, the solution might be usable because it is quick.
