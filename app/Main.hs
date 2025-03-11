-- 2025/03/11 -- The knight tour


-- *** UNDER CONSTRUCTION
{-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE PackageImports #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{- HLINT ignore "Eta reduce" -}

-- import Data.Maybe (fromMaybe)
-- import Text.Read (readMaybe)
import Data.Foldable (forM_)
import Data.List (foldl')
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as S

import Data.Ix (inRange)
import Data.Vector
  (Vector
  ,(!)
  ,(//)
  )

import Data.Vector qualified as V

import Data.Char (intToDigit)
--  modules for parsing
-- import Data.Char
--   (isDigit
--   ,isSpace
--   )
-- import Data.Functor
--   (void
--   ,($>)
--   )
-- import Text.ParserCombinators.ReadP
--   (ReadP
--   ,readP_to_S
--   ,endBy1
--   ,munch1
--   ,satisfy
--   ,many1
--   ,char
--   ,string
--   ,eof
--   ,optional
--   )

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
  let dim = (5,5) -- start with a small "chess" board
      maxdepth = uncurry (*) dim
      start = IM.singleton 0 0 -- start on a1
      rules = buildRules dim
      squares = buildSquares dim
  printSolutions squares (solutions rules maxdepth 1 start)

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
    delta = [1, 2, -2, -1]
    jumps = [(i,j) | i <- delta, j <- delta, abs i /= abs j]

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

printSolutions :: Squares -> [Board] -> IO ()
printSolutions squares boards =
  forM_ (zip boards [1..]) $ \(board,n) -> do
    putStr (show n <> ") ")
    printBoard squares board

printBoard :: Squares -> Board -> IO ()
printBoard squares board = do
  forM_ [0..length board - 1] $ \n ->
    let square = positionToSquare squares (board IM.! n)
    in putStr (square <> " ")
  putChar '\n'

positionToSquare :: Squares -> Int -> String
positionToSquare squares pos = squares IM.! pos
