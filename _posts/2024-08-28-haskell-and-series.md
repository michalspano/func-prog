---
title: Haskell and Mathematical Series
mathjax: true
layout: post
categories: haskell, maths
---

Recently, in a university course, I was fiddling with **mathematical series**
and their definitions in either a **recursive** or an **explicit** form. Sure,
sounds like some textbook, rather boring stuff, you may say. However, I found
it quite interesting (and fun too) and above all, I leveraged the power of
**Haskell** to play around with these series.

A recursive definition of a series consists of a "base case" and a recursive
formula to compute the remaining terms of the series by using the value of the
previous term or terms. An explicit definition, on the other hand, is simply a
mathematical function that computes the value of the term given its index.

Hm, I thought, perhaps Haskell may be a viable tool to play around with these
series, and, indeed, it was. All programmers are *lazy* in some way (recall that
Haskell is too a *lazy* language), and I'm no exception. I've scribbled a few
lines in my textbook and solved the transformations of the series by hand quite
swiftly; however, I felt unsure if my thinking was correct. Similarly,
programmers tend to be quite paranoid in nature, and there I'm not any different
either. Haskell, I recalled again, I could consider to be a fun tool to play with
this mathematical phenomenon, so I decided to write a small-ish program that
would verify whether my thinking was correct or not, hoping to gain some
confidence in my mathematical skills.

One could be wondering: "Why Haskell, though? I reckon I can compose a Python
code much easily and get the job done." Well, sure, that's a valid point.
However, the elegance that Haskell provides in expressing mathematical
concepts is fundamental. Furthermore, given my appeal to ["Literate
Programming"][literate-programming] (a concept so interesting that I'd
recommend every programmer to read about it), I realized that Haskell is
perhaps the most suitable from my toolbox to achieve the literate fashion
of the program. In any case, consider it yourself if my reasoning is valid or
nonsensical. Write a program in Python if you wish, but I'll stick to good
Haskell, at least for now, unless I'm forced to do otherwise.

I remember starting with the exercises from the textbook, and the following one
was perhaps the one that I stumbled upon first. We are given a recursive
formula (a definition) of series, and we were asked to find an explicit
definition. The recursive formula was given mathematically as follows:

$$
(a_n) = 
\begin{cases}
    1, & \text{if } n = 0 \\
    6, & \text{if } n = 1 \\
    4 a_{n-1} - 4 a_{n-2}, & \text{otherwise}
\end{cases} 
$$

Which can be elegantly expressed in Haskell as follows:

```haskell
a 0 = 1
a 1 = 6
a n = 4 * a (n-1) - 4 * a (n-2)
```

I reckon you understand the choice of Haskell for this task now more clearly,
or don't you, you Pythonista? Anyhow, jokes aside. The task was now to find
an explicit definition of $$(a_n)$$. I'll not bother too much with the
step-by-step derivation, but, if you want to try this exercise yourself, you
may consider the following helpful. Recall $$(a_n)$$, we may reorganize the
terms and receive:

$$
a_n - 4 a_{n-1} + 4 a_{n-2} = 0
$$

There's a well-known method to solve such equations, and it's called the
[**characteristic equation**][characteristic-equation]. We write the previous
in terms of $$r$$ which denotes the roots of the equation:

$$
r^2 - 4 r + 4 = 0
$$

That is, $$a_n = r^2$$, $$a_{n-1} = r$$, and $$a_{n-2} = 1$$. We solve the
quadratic equation and receive $$r = 2$$. For a single-root equation, the
characteristic equation is given as follows:

$$
a_n = r^n (\alpha_1 + \alpha_2 n)
$$

Herein, $$\alpha_1$$ and $$\alpha_2$$ are constants that we need to find. We
use the fact that $$a_0 = 1$$ and $$a_1 = 6$$ to find the constants. We receive
that $$\alpha_1 = 1$$ and $$\alpha_2 = 2$$. Thus, the explicit formula (labeled
$$a^{\prime}$$) is:

$$
a^{\prime}(n) = 2^n(1 + 2n)
$$

Yet again, transcribing this into Haskell is quite straightforward:

```haskell
a' n = (1 + 2*n) * 2^n
```

Cool, I thought, but how correct? I thought that I'd better verify this
somehow. But how? Perhaps, I may compute the results of $$a(n)$$ and
$$a^{\prime}(n)$$, assign them to pairs, and compare them. If all the pairs
from such a sequence are equal, then I can confidently say that I should
be correct. So, I'd have a list of pairs in the form of:

$$
{[(a(1), a^{\prime}(1)), (a(2), a^{\prime}(2), \dots, (a(n), a^{\prime}(n)]}
$$

Then, we simply assert that all the pairs are equal. This sounds rather trivial
in human language (and mathematics too): "If all the pairs are equal, then the
explicit formula is correct." So we call it a day, and we're done. Not truly,
let's try to write something similar in Haskell now, and see if we can verify
our thinking.

```haskell
comparator :: Int -> (Int -> Int) -> (Int -> Int) -> Bool
comparator n f g = all (uncurry (==)) (zip xs ys)
  where
    xs = map f [0..n]
    ys = map g [0..n]
```

I suppose this is perhaps not what you may have expected initially (especially
if you're more familiar with non-declarative programming), but there's not so
much magic, frankly, so do not worry too much. Let's break it down a bit. The
function is titled `comparator` (for obvious reasons), and it takes three
arguments: an integer, our $$n$$, and two functions, $$f$$ and $$g$$, which are
functions that take an integer and also return an integer (these should be our
recursive and explicit functions, respectively). The function then computes
`xs` and `ys` which is an application of the two functions to the range
$$[0..n]$$. Taking `zip xs ys`, we compute the list of pairs described above.
Lastly, the `all` function is used to verify each pair in the list (i.e. every
element of the list), *uncurries* it (i.e. applies the pair as an argument to
the function `==`, hence comparing the two elements of the pair), and returns a
single boolean value. This is because each comparison returns a boolean value,
and the `all` functions returns `True` if all the comparisons are `True`, and
`False` otherwise (does this remind you of the mathematical formulation said
above?).

I fired up `ghci` and, initially, `comparator` yields `False`. I was a bit
perplexed and I couldn't quite fire out what was wrong. I updated the function
a bit and I let it display the sequence `zip xs ys` to see what was going on.
I found a small mistake in my derivation, computed the correct explicit formula,
fired up `ghci` again, and *voilà*, `comparator` returned `True`. I was quite
relieved, I must admit.

I would like to make a remark here; yes, I'm aware that my home-made function
`comparator` is not the most performant one, but it did offer me what I needed.
I'm fully aware that it could have been written perhaps more elegantly with
tools like `QuickCheck` (specifically aimed at testing properties of functions),
but I'll leave that for another day.

In conclusion, I hope you found this little exercise interesting. I certainly
did, and I'm excited to see where next I can apply Haskell in my day-to-day
tasks.

<!-- LINKS -->
[literate-programming]: https://www.wikiwand.com/en/articles/Literate_programming
[characteristic-equation]: https://www.wikiwand.com/en/articles/Characteristic_equation_(calculus)
