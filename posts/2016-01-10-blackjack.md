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
decide to find out why.

# The Modeling
