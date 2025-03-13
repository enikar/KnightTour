# The knight tour problem

## Introduction

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

## Usage

There are some options:
- -h or --help, a summarize of the usage.
- -s or --size, to set the size of the board.
- -l or --list, to set the initial list.

The option --size expects a pair of Int: `(5,4)`. As the parentheses are
characters used by the shell, we need to quote the pair: \'(5,4)\'

The option --list expects a list of string shaped as: `["a1","b3"]`.
Obviously, we need to quote the expression in the shell, as for --size.

The default size is `(5,5)` and the default list is `["a1", "c2"]`

The arguments are checked. The size must be between `(1,1)` and `(9,9)`.
The list must be a valid list of jumps inside the board.
For example, from the shell, we can use:
    ```KnightTour -s '(5,4)' -l '["a1"]'```

The default for the size is not the standard size of a chessboard. There
are too many solutions to hope to list all of them, there are
13,267,364,410,532 tours on a chessboard (from [wikipedia](https://en.wikipedia.org/wiki/Knight%27s_tour)).

## Build

To build KnightTour:
```cabal build```
## Run
To run the program:
```cabal run KnightTour -- --size='(5,4)' --list='["a1"]'```
