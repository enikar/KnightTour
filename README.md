# The knight tour problem

A few years ago I wrote a program in C to explore the positions taken
by a knight on a chessboard.

It took me a few weeks to come up with a program that not only lets
you explore all the positions so that you only have to pass through each
square once, but also lets you change the dimensions of the chessboard
and even use rectangular ‘chessboards’.

This time I've rewritten it in Haskell. I didn't put in all the
optimisations I'd made in C. In any case, the C program makes extensive
use of pointers, whereas the Haskell program is written in a purely
functional style (although I also used a lot of foldl', which is a
slightly imperative way of doing things in a certain sense).

Anyway, here's this little toy. I was delighted to rediscover this old
friend, ‘the knight's tour of the chessboard’.
