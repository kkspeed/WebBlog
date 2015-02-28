---
title: Algebra Applied to Algorithms: A Simple Example
tags: Algorithm, Functional programming, Haskell, Mathematics
---
I'm reading Pearls of Functional Algorithm Design, which is just a
wonderful book on algorithms and functional programming. I'm also
playing around with Category Theory and I enjoy the feeling of
abstraction.  I try to apply some of the ideas to some basic algorithm
problems, and it seems to work. Here is a very simple example.

<!--more-->

**Sample Problem: Deriving Mergesort**

To sort 2 list, one way is to use mergesort. However, let's pretend
we don't know it at the moment. All we know is insertion sort. Can we
get merge sort?

First, we discover that List is a Monoid and Sorted(List) is also a
Monoid and there is a homomorphism. So we have:

- $In List:   List1 ++ List2$
- $In Sorted: Sorted(List1) \oplus Sorted(List2)$

The problem is: how to find the $\oplus$ operator (in fact, it's the
merge function in Mergesort)?

We know insertion sort, and it can be written in the format:

~~~~ {#mycode .haskell .numberLines startFrom="1"}
insert i []     = [i]
insert i t@(x:xs)
   | i < x      = i : t
   | otherwise  = x : insert i xs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And one way to "merge" the two sorted lists is to insert everything
in the first list into the second list:

~~~~ {#mycode .haskell .numberLines startFrom="1"}
merge xs ys = foldr insert xs ys
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here the **merge** is just one implementation of the $\oplus$
operator. However, such operation still takes quadratic time.

Recall that, xs and ys are sorted. We have the following property:

**Property 1**

If we insert $x_i$ into $j$-th position of $ys$, then $\forall y_t
(t\leq j)$ and $x_k\ (k \gt i), x_k > y_t$.

We then modify insert to return the rest of the list.

~~~~ {#mycode .haskell .numberLines startFrom="1"}
insert ([], acc)       i  = ([], i : acc)
insert (t@(x:xs), acc) i
       | i < x            = (t, i : acc)
       | otherwise        = insert (xs, (x : acc)) i
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now each time we insert an element, it will return the rest of the
list and a reversed list of elements already inserted:

~~~~ {#mycode .haskell .numberLines startFrom="1"}
λ> insert ([1, 2, 4, 5], []) 3
([4,5],[3,2,1])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and we can write merge as:

~~~~ {#mycode .haskell .numberLines startFrom="1"}
merge xs ys = reverse rev ++ rest
    where (rest, rev) = foldl' insert (xs, []) ys
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This way, each insertion is "memorized" and the following insertion
just happends to go from currently inserted position as guaranteed by
Property 1. Now merge takes only linear time (of course, it can be
further improved to reduce constant factors, but I will not discuss it
here)
