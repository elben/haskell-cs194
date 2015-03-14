Gloss
=====

CIS 194 Week 10\
5 November 2014

Suggested reading:

-   [The gloss Package](https://hackage.haskell.org/package/gloss)
-   [The gloss GitHub repo, with lots of
    examples](https://github.com/benl23x5/gloss)

Gloss is a popular library for graphics work in Haskell with a
straightforward interface, and quite a few nice examples. As we are
approaching the end of the semester, it’s time for you students to start
spreading your wings and flying on your own, and working with Gloss is
great practice. So, I leave you to the materials above.

> import Graphics.Gloss
> import Data.Monoid
>
> main = display (InWindow "Hello, world!" (200, 200) (200, 200))
>                white
>                (circle 50 <>
>                 (translate (-20) 10    $ circle 10) <>
>                 (translate 20    10    $ circle 10) <>
>                 (translate 0     (-15) $ scale 1 0.7 $ arc 180 360 20))

* * * * *

`Generated 2014-12-04 13:37:43.709769`
