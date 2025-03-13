-- 2025/03/11 -- The knight tour

-- Usage: cabal run KnightTour -- --size='(5,4)' --list='["a1"]'

{-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{- HLINT ignore "Eta reduce" -}

import Data.Foldable (forM_, foldrM)
import Data.List (foldl')
import Data.List.Extra (nubOrd)
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

--  modules for parsing
-- we use Attoparsec to write custom Options readers.
-- import Data.Text qualified as T
-- import Data.Attoparsec.Text as A

-- We use Except to manage errors during parseInitial
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
  (ExceptT
  ,runExceptT
  ,liftEither
  )
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

-- Tour is a Vector of square position taken by
-- the knight.
-- at index 0 is the first square occupied
-- at index 1 is the second…
type Tour = Vector Int

-- Squares map position to the coordinate in the chess tour
-- 0 is a1, 1 is a2…
-- It's used for printing solutions.
type Squares = IntMap String

main :: IO ()
main = do
  r <- runExceptT parseOptions
  case r of
    Left e -> printError e
    Right (dim, start) ->
      let maxdepth = uncurry (*) dim
          start' = V.fromList start
          rules = buildRules dim
      in printSolutions dim (solutions rules maxdepth start')

-- This a back-tracking algorithm with two functions:
-- solutions and successors.
solutions :: Rules -> Int -> Tour -> [Tour]
solutions rules maxdepth tour
  |length tour == maxdepth = [tour]
  |otherwise = concatMap (solutions rules maxdepth)
                         (successors rules tour)

-- for the last inserted square select the successors
-- which are not already in the tour.
successors :: Rules -> Tour -> [Tour]
successors rules tour = S.foldl' f [] nextSquares
  where
    -- gets the successors
    lastsquare = V.last tour
    possibleSquares = rules ! lastsquare
    -- removes squares already in the tour
    nextSquares = S.difference possibleSquares (S.fromList (V.toList tour))
    -- builds a new tour for each new square (next)
    f acc next = V.snoc tour next : acc

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
    rows = zip ['1'..] [0..h-1]

    f acc (crow, nrow) = foldl' g acc columns
      where
        g acc' (ccol, ncol) = IM.insert (ncol + w*nrow) [ccol, crow]  acc'


-- utilities for printing
printSolutions :: Dim -> [Tour] -> IO ()
printSolutions dim tours = do
  let squares = buildSquares dim
  forM_ (zip tours [1..]) $ \(tour,n) -> do
    putStr (show n <> ") ")
    printTour squares tour

printTour :: Squares -> Tour -> IO ()
printTour squares tour = do
  forM_  tour $ \pos ->
    let square = positionToSquare squares pos
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
  {initialTour :: [String]
  ,boardDim :: (Int, Int)
  }

-- Maybe we should write custom readers to use in place of auto
-- We'll have a better syntax in the cmdline from the shell.
parseTour :: Parser OptTour
parseTour = OptTour
  <$> option auto
      (long "list"
       <> short 'l'
       <> help "Inital state of the tour as a list of square"
       <> showDefault
       <> value ["a1", "c2"]
       <> metavar "LIST OF SQUARE"
      )
  <*> option auto
      (long "size"
      <>short 's'
      <> help "Dimension of the tour between (1,1) and (9,9)"
      <> showDefault
      <> value (5,5)
      <> metavar "PAIR OF INT"
      )

parseOptions :: ExceptT ParseError IO (Dim, [Int])
parseOptions =
  let options = info
        (parseTour <**> helper)
        (fullDesc
         <> progDesc "Compute solutions for the knight's tour"
         <> header "KnightTour --list=[\"a1\",\"c2\"] --size=(5,5)"
        )
  in
    do OptTour{..} <- liftIO (execParser options)
       let initial = parseInitial boardDim initialTour
       liftEither (fmap (boardDim,) initial)

-- There are many checks to accomplish.
-- First, we checkInitial the request size of the board
-- Second: checkInitial that each square is uniq in the list.
-- Third: checkInitial that each square is composed by a letter from 'a'
--        and a digit from 1
-- Fourth: Check these column and row are in the board
-- Finally: Check jumps are valid.

-- We have to think to a better way to accomplish this. It's better
-- since we use the Either Monad, though.
-- Maybe split it in several functions is a good option.
parseInitial :: Dim -> [String] -> Either ParseError [Int]
parseInitial (w, h) squares
  |w > 9
   || h > 9
   || w < 1
   || h < 1 = Left  (InvalidDimension (show (w, h)))
  |otherwise =
 let
   -- build two lists of pairs within the size of the board
   -- so we can checkInitial squares are in the board
   colums = zip ['a'..] [0..w-1]
   rows = zip ['1'..] [0..h-1]
   -- Get the valid columns and valid rows
   vcols = map fst colums
   vrows = map fst rows

   -- build a list of coordinate (Int, Int) from the list of String.
   reduce :: String -> [(Int,Int)] -> Either ParseError [(Int, Int)]
   reduce str acc = do
     r <- strToPair str -- : acc
     pure (r:acc)

   -- Build a pair from a string if valid
   -- "a1" becomes (0,0)
   -- "b1" becomes (1,0)
   strToPair :: String -> Either ParseError (Int, Int)
   strToPair [col, row]
     |col `elem` vcols
      && row `elem` vrows =
      do
        ncol <- selectCol col
        nrow <- selectRow row
        pure (ncol, nrow)
     |otherwise = Left (OutsideBoard [col, row])
   strToPair str = Left (InvalidSquare str)

   -- returns colum or row as a number or an error
   selectCol :: Char -> Either ParseError Int
   selectCol col = maybe errParse Right (lookup col colums)
     where
       errParse = Left (InvalidCol [col])

   selectRow :: Char -> Either ParseError Int
   selectRow row = maybe errParse Right (lookup row rows)
     where
       errParse = Left (InvalidRow [row])

   -- build a position as a single Int
   pairToPos (x, y) = x + w * y
 in do
   -- checks they aren't duplicate square
   sq <- checkInitial squares
   -- builds a [(Int,Int)] from the [String]
   -- and performs many checks
   ls <- foldrM reduce [] sq
   -- Checks validity of jumps
   ls' <- checkJumps ls
   -- map (Int, Int) to position
   pure (map pairToPos ls')

-- checkInitial: check there aren't duplicate square
checkInitial :: [String] -> Either ParseError [String]
checkInitial sqs
  |sqs == nubOrd sqs = Right sqs
  |otherwise = Left (DuplicateSquare (show sqs))


checkJumps :: [(Int, Int)] -> Either ParseError [(Int,Int)]
checkJumps xs
  |all (uncurry validJump) (zip xs (tail xs)) = Right xs
  |otherwise = Left (InvalidJumps (show (map toSquare xs)))
  where
    validJump (x, y) (x', y') = (x' - x, y' - y) `elem` jumps
    -- build a list of jumps
    deltas = [1,2,-2, -1]
    jumps = [(i, j)| i <- deltas, j <- deltas, abs i /= abs j]
    toSquare (x, y) = [c,r]
      where
        c = ['a'..] !! x
        r = ['1'..] !! y

-- utilities to manage errors during parsing the initial list of jumps
data ParseError = DuplicateSquare String
                  |InvalidJumps String
                  |OutsideBoard String
                  |InvalidSquare String
                  |InvalidCol String
                  |InvalidRow String
                  |InvalidDimension String

printError :: ParseError -> IO ()
printError err = putStrLn ("Error: parseInitial: " <> strError)
  where
    strError = case err of
      DuplicateSquare str -> "There are duplicate squares: " <> str
      InvalidJumps str -> "Invalid inital jumps: " <> str
      OutsideBoard str -> "Square outside of the board: " <> str
      InvalidSquare str -> "Invalid square name: " <> str
      InvalidCol str -> "Invalid column: " <> str
      InvalidRow str -> "invalid row: " <> str
      InvalidDimension str -> "invalid dimension. It must be between (1,1) and (9,9) : " <> str
