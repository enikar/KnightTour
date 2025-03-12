-- 2025/03/11 -- The knight tour

-- Usage: cabal run KnightTour -- --size='(5,4)' --list='["a1"]'

{-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE PackageImports #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{- HLINT ignore "Eta reduce" -}

import Data.Maybe (fromMaybe)
-- import Text.Read (readMaybe)
import Data.Foldable (forM_)
import Data.List (foldl')
import Data.List.Extra (nubOrd)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as S
import Data.Char (intToDigit)

import Data.Ix (inRange)
import Data.Vector
  (Vector
  ,(!)
  ,(//)
  )

import Data.Vector qualified as V
--  modules for parsing
import Options.Applicative
  (Parser
  ,option
  ,auto
  ,long
  ,short
  ,help
  ,showDefault
  ,value
  ,metavar
  ,execParser
  ,info
  ,helper
  ,progDesc
  ,header
  ,fullDesc
  ,(<**>)
  )

default(Int)

-- Rules is a Table that map for each position the possible
-- successors. Positions are represented as an Int.
-- position =  colum + (number of columns) * row
type Rules = Vector IntSet

-- width = number of columns
-- height = number of rows
type Dim = (Int, Int) -- width x height

-- Board maps the depth to a square in the board
-- depth 0 is the first square occupied
-- depth 1 is the second…
-- Maybe we should use a Vector, here
type Board = IntMap Int

-- Squares map position to the coordinate in the chess board
-- 0 is a1, 1 is a2…
type Squares = IntMap String

main :: IO ()
main = do
  (start, dim) <- parseOptions
  let maxdepth = uncurry (*) dim
      start' = IM.fromList (zip [0..] start)
      rules = buildRules dim
  printSolutions dim (solutions rules maxdepth (length start') start')

-- solutions is a back-tracking algorithm.
solutions :: Rules -> Int -> Int -> Board -> [Board]
solutions rules maxdepth depth board
  |depth == maxdepth = [board]
  |otherwise = concatMap (solutions rules maxdepth (depth+1))
                         (successors rules depth board)

-- for the last inserted square select the successors
-- which are not already in the board.
successors :: Rules -> Int -> Board -> [Board]
successors rules depth board = S.foldl' f [] nextSquares
  where
    -- gets the successors
    lastsquare = board IM.! (depth - 1)
    possibleSquares = rules ! lastsquare
    -- removes squares already in the board
    nextSquares = S.difference possibleSquares (S.fromList (IM.elems board))
    -- builds a new board for each new square
    f acc prop = IM.insert depth prop board : acc

-- The trickier part is to build the Rules…
-- Maybe it should be better to fill the vector inside runST
-- or to start with V.empty and use V.cons but we need that
-- (rules ! 0) are the successors of the possition 0 ("a1")
-- (rules ! 1) are the successors of the position 1 ("a2")…
buildRules :: Dim -> Rules
buildRules (w, h) = V.replicate (w*h) S.empty // rules
  where
    rules = foldl' f [] squares
    squares = [(x,y) | x <- [0..w-1], y <- [0..h-1]]

    range = ((0,0), (w-1, h-1))
    deltas = [1, 2, -2, -1]
    jumps = [(i,j) | i <- deltas, j <- deltas, abs i /= abs j]

    f acc (col, row) = (col+w*row, S.fromList possibleJumps) : acc
      where
        possibleJumps = foldl' g [] jumps
        g acc' (dx, dy)
          |inRange range (col', row') = (col' + w*row') : acc'
          |otherwise                  = acc'
           where
             row' = row + dy
             col' = col + dx

-- Build the maping between positions (Ints) and square names.
buildSquares :: Dim -> Squares
buildSquares (w, h) = foldl' f IM.empty rows
  where
    columns = zip ['a'..] [0..w-1]
    rows = zip (map intToDigit [1..]) [0..h-1]

    f acc (crow, nrow) = foldl' g acc columns
      where
        g acc' (ccol, ncol) = IM.insert (ncol + w*nrow) [ccol, crow]  acc'


-- utilities for printing
printSolutions :: Dim -> [Board] -> IO ()
printSolutions dim boards =
  forM_ (zip boards [1..]) $ \(board,n) -> do
    putStr (show n <> ") ")
    printBoard dim board

printBoard :: Dim -> Board -> IO ()
printBoard dim board = do
  let squares = buildSquares dim
  forM_ [0..length board - 1] $ \n ->
    let square = positionToSquare squares (board IM.! n)
    in putStr (square <> " ")
  putChar '\n'

positionToSquare :: Squares -> Int -> String
positionToSquare squares pos = squares IM.! pos

-- squareToPosition :: Squares -> String -> Int
-- squareToPosition squares str = IM.foldrWithKey search (-1) squares
--   where
--     search n s acc
--       |s == str = n
--       |otherwise = acc

-- utillities for parsing the command line
data OptTour = OptTour
  {initialOpt :: [String]
  ,dimOpt :: (Int, Int)
  }


-- Maybe we should write custom readers to use in place of auto
-- We'll have a better syntax in the cmdline from the shell.
parseTour :: Parser OptTour
parseTour = OptTour
  <$> option auto
      (long "list"
       <> short 'l'
       <> help "Inital state of the board as a list of square"
       <> showDefault
       <> value ["a1", "c2"]
       <> metavar "LIST OF SQUARE"
      )
  <*> option auto
      (long "size"
      <>short 's'
      <> help "Dimension of the board"
      <> showDefault
      <> value (5,5)
      <> metavar "PAIR OF INT"
      )

parseOptions :: IO ([Int], Dim)
parseOptions = do
  let options = info
        (parseTour <**> helper)
        (fullDesc
         <> progDesc "Compute solutions for the knight parseTour"
         <> header "KnightTour --list=[\"a1\",\"b3\"] --size=(5,5)"
        )
  tour <- execParser options
  let size = dimOpt tour
      initial = parseInitial size (initialOpt tour)
  pure (initial, size)

-- There are many checks to accomplish.
-- First, we need to check that each square is uniq in the list.
-- Second: check that each square is composed by a letter from 'a'
-- and a digit from 1
-- Third: Check these column and row are in the board
-- Finally: Check that are valid jumps.

-- Very ugly. Rewrite it! We have to think to a better way
-- to accomplish that. There are too many calls to the error "function"
parseInitial :: Dim -> [String] -> [Int]
parseInitial (w, h) squares
  |w > 9
   || h > 9
   || w < 1
   || h < 1 = error ("Error: parseInitial: too high dimension (max (9,9)): " <> show (w, h))
  |otherwise = map pairToPos ls'
  where
    -- build a position as a single Int
    pairToPos (x, y) = x + w * y

    -- build a list of jumps
    deltas = [1,2,-2, -1]
    jumps = [(i, j)| i <- deltas, j <- deltas, abs i /= abs j]
    -- build two lists of pairs within the size of the board
    -- so we can check squares are in the board
    colums = zip ['a'..] [0..w-1]
    rows = zip ['1'..] [0..h-1]
    -- Get the valid columns and  valid rows
    vcols = map fst colums
    vrows = map fst rows

    -- build a list of coordinate (Int, Int) from the list of string.
    ls = foldr f [] (check squares)
    f str acc = strToPair str : acc
    -- check there aren't duplicate squarr
    check sqs
      |sqs == nubOrd sqs = sqs
      |otherwise = error ("Error: parseInitial: there is duplicate squares: "
                           <> show squares)

    -- check the validity of jumps
    ls' |all (uncurry valid) (zip ls (tail ls)) = ls
        |otherwise = error ("Error: parseInitial: invalid inital jumps: "
                              <> show squares)

    valid (x, y) (x', y') = (x' - x, y' - y) `elem` jumps

    -- Build a pair from a string if valid
    -- "a1" becomes (0,0)
    -- "b1" becomes (1,0)
    strToPair [col, row]
      |col `elem` vcols
       && row `elem` vrows = (selectCol col, selectRow row)
      |otherwise = error ("Error: parseInitial: square outside the board: "
                          <> [col, row])
    strToPair str  = error ("Error: parseInitial: invalid square: " <> str)

    -- returns colum or row as a number
    selectCol col = fromMaybe errParse (lookup col colums)
      where
        errParse = error ("Error: parseInitial: invalid colum: " <> show col)

    selectRow row = fromMaybe errParse (lookup row rows)
      where
        errParse = error ("Error: parseInitial: invalid row: " <> show row)
