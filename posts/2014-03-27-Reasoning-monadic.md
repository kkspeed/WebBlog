---
title: Reasoning the Monadic Code
tags: Functional programming, Haskell
---
I was always afraid of Haskell Monads. But... I like Haskell, which
means, Monads are inevitable. Well, so let me have a taste.

Let's say, the code is:

    ((++) =<<) (const "hello") "world"

What's that? Let's get the clue from type.

<!--more-->

    ghci>> :t (=<<)
    (=<<) :: (a -> m b) -> m a -> m b

The =<<  is  >>= with parameters flipped. It takes in a function that
returns a monad, a monad and returns another monad.

OK. How about (++) ?

    ghci>> :t (++)
    (++) :: [a] -> [a] -> [a]

It's a binary function. What has it to do with monad after all?
Well... the type [a] -> [a] -> [a] is right associative and can be
written as [a]->([a]->[a]), due to curry. The (a -> b) is a syntax
sugar of ((->) a b). Now, let's rewrite (++).

    (++) :: [a] -> ((->) [a] [a])

If we see (->)[a] as the m in (=< (m b).

In fact (->) a is a monad, it's definition is:

    instance Monad ((->) a) where
        g >>= f = \x -> f (g x) x
        return = const

Now we can see the type for ((++) =<<):

    (=<< m b) -> m a -> m b
let m = (->) [a], a = [a], b = [a] in ((++) =<<):
    ((++) =<<) = ([a] -> [a]) -> [a] -> [a]

So what it does after all?

We can reason about this:

     f =<< g = f (g x) x
     {f = (++)}
    =(++) =<< g = \x -> (g x) ++ x
     {curry}
    =((++) =<< g) = \x -> (g x) ++ x
    = \g x -> (g x) ++ x

So ((++) =<<) takes in a function that first process the list, then
concatenate the processed list with the original one. The following
example demonstrates this:

    ghci>> ((++) =<<) (\x-> if x == "hello" then "foo" else "bar") "hello"
    "foohello"
