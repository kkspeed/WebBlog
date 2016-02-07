---
title: Defining XMonad New Layout for Ultrawide Screens
tags: XMonad, Functional programming
---

XMonad has been my go-to window manager for many years. In its most
popular tiling layout, XMoand splits the window into 2
panes. Recently, I got a 21:9 ultrawide monitor. It's when traditional
two-pane looks unsatisfactory.

<!--more-->

As shown in the following picture, if XMoand splits a 21:9 monitor
into 2 pane, it really wastes a lot of space. The 10.5:9 window is
hardly satisfactor. It's too wide.

<span class="img-large-post">
![XMoand 2 Pane](/media/2-pane-too-wide.png)
</span>

Ideally, I would like the screen to be composed of 3 panes. They
operate in the following scheme:

- If there are only 2 windows, 1 window would take up 1/3 while the
  other would take up the rest 2/3.
- If there are more than 2 windows, the screen would show 3 panes,
  with width of 1:1:1.

I implemented an XMonad layout to achieve this. It's called
TriPaneTall as shown below:

Two windows are split as 1:2. Very suitable for editor / console +
browser combination.

<span class="img-large-post">
![1:2 Splitting](/media/2-pane-1-2.png)
</span>

More than two windows will be organized in equally wide panes. More
windows will be vertically tiled up in the last pane.

<span class="img-large-post">
![1:1:1 Splitting](/media/more-windows.png)
</span>

I also implemented 2 messages to increase (decrease) the number of
windows in the left and mid pane:

~~~~~ {.haskell}
data IncMaster1N = IncMaster1N !Int deriving Typeable

-- | Increse the number of windows in the master 2 pane
data IncMaster2N = IncMaster2N !Int deriving Typeable

instance Message IncMaster1N
instance Message IncMaster2N

instance LayoutClass TriPaneTall a where
    pureLayout (TriPaneTall nm1 mr1 nm2 mr2) r s = zip ws rs
      where ws = W.integrate s
            rs = triTile nm1 mr1 nm2 mr2 r (length ws)
    pureMessage (TriPaneTall nm1 mr1 nm2 mr2) m =
        msum [ fmap incMaster1n (fromMessage m)
             , fmap incMaster2n (fromMessage m) ]
        where incMaster1n (IncMaster1N d) =
                  TriPaneTall (max 1 (nm1 + d)) mr1 nm2 mr2
              incMaster2n (IncMaster2N d) =
                  TriPaneTall nm1 mr1 (max 1 (nm2 + d)) mr2
    description _ = "TriPaneTall"
~~~~~

The actual split code is also very simple:

~~~~~~ {.haskell}
-- | Actually computes the tiling:
-- | 1. If the number of windows <= maximum number of windows in pane 1, do
-- |    vertical split
-- | 2. If the number of windows > maximum number of windows in pane 1 and <=
-- |    maximum number of windows in pane 2, do horizontal 1:2 split. Then split
-- |    windows in each pane
-- | 3. If the number of windows > sum of maximum windows in both panes, do
-- |    a 1:1:1 horizontal split. Then
triTile :: Int -> Rational -> Int -> Rational -> Rectangle -> Int -> [Rectangle]
triTile nm1 f1 nm2 f2 r n
    | n <= nm1 || nm1 == 0 = splitVertically n r
    | n > nm1 && n <= (nm1 + nm2) = splitVertically nm1 r1 ++
                                    splitVertically (n - nm1) r2
    | otherwise = splitVertically nm1 r1 ++ splitVertically nm2 r2' ++
                  splitVertically (n - nm1 - nm2) r3
    where (r1, r2) = splitHorizontallyBy f1 r
          (r2', r3) = splitHorizontallyBy f2 r2
~~~~~~

The whole code listing can be found at
[this gist](https://gist.github.com/anonymous/42ab8e7a384569c39368).
