---
title: On Y Combinator
---
Recently I was reading a book on programming languages design and came
across something interesting related to Y combinator.

The Question: how to write recursion in lambda function without side
effects and built-in recursion support?

First, let's talk about Russell Paradox: Assume P is the set of all
the set that does not contain itself. Does P belongs to P?

Under classical set theory, this is a paradox. The type system for
dynamic language more or less resemble such set theory. Let's say, F
is a characteristic function of a set. F(x) = true if x is in the set
and F(x) = false otherwise. Under such representation, {1, 2, 3} can
be represented as F(x) = (x==1 OR x==2 OR x==3).

<!--more-->

Then the characteristic function for set P described above would be
P(x) = Not (x x). The program P(P) will run forever, which means such
question is undecidable. In computational theory, x $\notin$ Wx is not
computable (x is the Godel Number), which, from my point of view, more
or less is similar to P.  On the other hand, for languages like
Haskell, the type system prevents circular types. Hence, such P cannot
be defined in these languages.

Now let's talk about the recursion problem. Assume sum is the function
to sum up from n to 0. Normally, the function would look like:

~~~~ {#mycode .clojure .numberLines startFrom="1"}
(defn sum [arg]
    (if (= arg 0)
        0
        (+ arg (sum (- arg 1)))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One possible implicit recursive solution for the question would be:

~~~~ {#mycode .clojure .numberLines startFrom="1"}
(def sum (fn [this]
    (fn [arg]
        (if (= arg 0)
            0
            (+ arg ((this this) (- arg 1)))))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Then call:  ((sum sum) 10) to get the result.

We can abstract the definition further with Y-combinator. One possible
definition would be:

~~~~ {#mycode .clojure .numberLines startFrom="1"}
(def Y (fn [body]
    (let [f (fn [this]
              (fn [arg]
                  ((body this) arg)))]
         (f f))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
