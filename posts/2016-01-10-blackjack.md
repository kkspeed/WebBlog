---
title: Vegas BlackJack Rant
tags: Games, Functional programming
---

Blackjack is one of the most played games in casinos and in fact, one
of the most fair games. Theoretically, if you follow the
[basic strategy](http://www.smarterbet.com/bjbasics.html), the house's
advantage is about 0.5%. Unfortunately, the basic strategy seems
clumsy for a first-time visitor like me. So I decided to opt for the
[simple strategy](http://wizardofodds.com/games/blackjack/basics/),
which would give the house a bit more advantage but it should not be
obvious to notice.

However, not all casinos implement the assumed blackjack rule in basic
strategy. In fact, most of them don't, which give them a bit more
advantage. The rules for the casinos in Vegas can be found
[here](http://wizardofvegas.com/guides/blackjack-survey/).

<!--more-->

I would very much like to go for a test with strategies under
different rules, but unfortunately, it's economically infeasible to
me. So, I did a simple experiment with a blackjack game console, which
only implemented the simplest twenty-one game:

1. The dealer keeps hitting until soft 17
2. The player only chooses between stand and hit

And the result? You can guess, I lost all the money, which is, 4
bucks. The simple strategy does not seem to work well for me. So... I
decide to find out why. And a simulation would look a thousand times
easier than pure math to me. I'm going to use Haskell for this task.

# The Modeling
The key part for the simulation is to encode the strategy. The
strategy depicted in the following picture (taken from
[Wizard of Odds](http://wizardofodds.com/games/blackjack/basics/))
clearly indicates a decision-tree-like structure.

![Simple strategy](http://wizardofodds.com/games/blackjack/wizard_strategy.gif)

To adapt it to the simple rule, we conclude the deck as in following
image.

<span class="img-large-post">
![Strat image](/images/strat.png)
</span>

The game is divided into several actions. An action is basically a
judgement based on the game status, which decides to proceed with the
next actions or no or a movement that updates the game status.

~~~~~~~~{.haskell}
newtype GameAction s a = GameAction { getAction :: StateT s Maybe a }
    deriving (Monad, Applicative, Functor, MonadState s, Alternative)
~~~~~~~~

Such action carries a game state s, and a possible result. It's
Nothing if the action will not proceed.

Now we implement 3 combinators. The simplest is bail, which always
fails.

~~~~~~~~{.haskell}
bail :: GameAction s a
bail = GameAction $ lift Nothing
~~~~~~~~

A second one is condition. It bails if the condition of the game state
is false. Otherwise, it succeeds and does nothing.

~~~~~~~~{.haskell}
condition :: (s -> Bool) -> GameAction s ()
condition f = do
  b <- f <$> get
  if b then return () else bail
~~~~~~~~

The last one is called choice, which takes in a series of actions and
tries them one-by-one until succeeds.

~~~~~~~~{.haskell}
choice :: (Alternative m) => [m a] -> m a
choice = foldl1 (<|>)
~~~~~~~~

The game state consists of the dealer's deck, the player's deck, whose
going to be the next move and the cards pool. For the sake of
simplicity, I'm going to assume they are playing an infinite card pool
game.

~~~~~~~~{.haskell}
data Turn = Dealer | Player | Final
          deriving (Show, Eq)

data BJState = BJState { dealer :: [Int]
                       , player :: [Int]
                       , turn :: Turn
                       , cards :: [Int] }
             deriving Show
~~~~~~~~

The dealer's action is fairly simple -- keep drawing cards until soft
17, then stand.

~~~~~~~~{.haskell}
dealerStrat = choice [ holdOnSoft17, dealerDraw ]

holdOnSoft17 :: GameAction BJState ()
holdOnSoft17 = condition ((>=17) . softSum . dealer) >> dealerStand

dealerStand :: GameAction BJState ()
dealerStand = modify $ \s -> s { turn = Final }

dealerDraw :: GameAction BJState ()
dealerDraw = modify $ \s -> s { dealer = head (cards s) : dealer s
                              , cards = tail (cards s) }
~~~~~~~~

The player's strategies are encoded according to the previous image.

~~~~~~~{.haskell}
playerStrat :: GameAction BJState ()
playerStrat = choice [ playerHard >>
                       choice [ inInterval player (2, 11) >> playerDraw
                              , choice [ inInterval dealer (2, 6) >> playerStand
                                       , inInterval player (12, 16) >> playerDraw
                                       , playerStand ] ]
                     , inInterval player (2, 18) >> playerDraw
                     , playerStand ]

playerHard :: GameAction BJState ()
playerHard = ensureHard player

inInterval :: (BJState -> [Int]) -> (Int, Int) -> GameAction BJState ()
inInterval character (a, b) = condition (\s -> cardSum s <= b && cardSum s >= a)
    where cardSum = softSum . character
~~~~~~~

A bunch of auxiliary functions are listed below:

~~~~~~~{.haskell}
ensureHard :: (BJState -> [Int]) -> GameAction BJState ()
ensureHard role = condition (isHard . role)

isSoft :: [Int] -> Bool
isSoft cs = any (==1) cs && sum (aceAs11 cs) <= 21

isHard :: [Int] -> Bool
isHard = not . isSoft

aceAs11 :: [Int] -> [Int]
aceAs11 [] = []
aceAs11(1:xs) = 11 : xs
aceAs11 (x:xs) = x : aceAs11 xs

softSum :: [Int] -> Int
softSum xs | isSoft xs = sum (aceAs11 xs)
           | otherwise = sum xs
~~~~~~~

To model the final gain. Assume player starts with 0 points. A win
would grant +1 score, a loss would give -1 score. I've run 10000
plays and accumulates the final gain. Then I took the average of the
gain of 100 runs. The result is: -745.0202. If I try with the naive
mimic dealer strategy. It's going to be around -790. Well, in such
simple rule, the simple strategy is just marginally better than the
naive strategy. Shame!

Note: The code snippet is pasted
 (here)[https://gist.github.com/kkspeed/09a62db9538de94bb4df].
 Be advised that the code is not in any ways written to conform high
 software engineering standards.
