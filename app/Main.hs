-- 2025/03/11 -- The knight tour

-- Usage: cabal run KnightTour -- --size='(5,4)' --list='["a1"]'

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{- HLINT ignore "Eta reduce" -}

module Main (main) where

import Data.Foldable
  (forM_
  ,foldrM
  )
import Data.List (foldl')
import Data.List.Extra (nubOrd)
import Data.Tuple.Extra (dupe)
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
import Control.Monad.Extra (eitherM)
--  modules for parsing
import Control.Applicative
  ((<|>)
  ,(<**>)
  )

--import Data.Text (Text)
import Data.Text qualified as T
import Data.Attoparsec.Text
  (parseOnly
  ,char
  ,decimal
  ,sepBy1
  ,takeWhile1
  )
import Data.Attoparsec.Text qualified as A
import Data.Char (isDigit)

-- We use Except to manage errors during parseInitial
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
  (ExceptT
  ,runExceptT
  ,liftEither
  )
import Options.Applicative
  (Parser
  ,ReadM
  ,option
  ,eitherReader
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

-- Squares map position to the coordinate in the chessboard
-- 0 is a1, 1 is a2…
-- It's used for printing solutions.
type Squares = IntMap String

main :: IO ()
main = eitherM terminate continue (runExceptT parseOptions)
  where
    terminate e = putStrLn ("Error: parseInital: " <> show e)

    continue (start, dim) =
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
-- (well, parsing the initial list from the command line was
-- probably worst).
-- There is another way to do it with two nested foldl' and using
-- V.snoc. Whether it is better or not, is a matter of  taste, I believe.
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
  forM_ tour $ \pos ->
    let square = positionToSquare squares pos
    in putStr (square <> " ")
  putChar '\n'

positionToSquare :: Squares -> Int -> String
positionToSquare squares pos = squares IM.! pos

-- utillities for parsing the command line
data OptTour = OptTour
  {initialTour :: [String]
  ,boardDim :: (Int, Int)
  }

-- Well we remove all spaces from the input. It's not neat but it
-- simplifies the parsing with Attoparsec and spaces are meaningless for
-- this program.
attoReader :: A.Parser a -> ReadM a
attoReader p = eitherReader $ \s ->
  let s' = T.concat (T.splitOn " " (T.pack s))
  in parseOnly p s'

parseTour :: Parser OptTour
parseTour = OptTour
  <$> option (attoReader parseList)
      (long "list"
       <> short 'l'
       <> help "Inital state of the tour as a list of square"
       <> showDefault
       <> value ["a1", "c2"]
       <> metavar "LIST OF SQUARE"
      )
  <*> option (attoReader parseSize)
      (long "size"
      <>short 's'
      <> help "Dimension of the tour between (1,1) and (9,9)"
      <> showDefault
      <> value (5,5)
      <> metavar "PAIR OF INT"
      )

parseOptions :: ExceptT ParseError IO ([Int], Dim)
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
       liftEither ((,boardDim) <$> initial)

-- Here we use Attoparsec to make option readers
-- All parsers are written in Applicative style
-- though it is not mandatory. It was just for fun.
parseSize :: A.Parser (Int,Int)
parseSize = parsePair
            <|> parseCrux
            <|> dupe <$> decimal

-- parseCrux parses the size as 5x4
parseCrux :: A.Parser (Int,Int)
parseCrux = do
  liftA2 (,)
         (decimal <* char 'x')
         decimal

-- parses a pair of Int as (4,5)
parsePair :: A.Parser (Int, Int)
parsePair =
  liftA2 (,)
         (char '(' *> decimal <* char ',')
         (decimal <* char ')')

parseList :: A.Parser [String]
parseList = parseHsList <|> parseSimpleList

-- parses a List of string with haskell syntax
parseHsList :: A.Parser [String]
parseHsList =
  char '['
  *> parseStr `sepBy1` char ','
  <* char ']'

-- parses a string enclosed within " ", with no escaping with \
parseStr :: A.Parser String
parseStr =
  T.unpack <$> (char '"'
                *> takeWhile1 (/= '"')
                <* char '"')

-- parses a list of strings separate by a ','
parseSimpleList :: A.Parser [String]
parseSimpleList =
  map T.unpack <$> takeWhile1 (/=',') `sepBy1` char ','


-- There are many checks to perform.
-- First, we check the request size of the board
-- Second: check that each square is uniq in the list.
-- Third: check that each square is composed by a letter from 'a'
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
    -- so we can check the squares are in the board
    colums = zip ['a'..] [0..w-1]
    rows = zip ['1'..] [0..h-1]

    -- build a list of coordinate (Int, Int) from the list of String.
    reduce :: String -> [(Int,Int)] -> Either ParseError [(Int, Int)]
    reduce str acc = do
      r <- strToPair str
      pure (r:acc)

    -- Builds a pair from a string if valid
    -- "a1" becomes (0,0)
    -- "b1" becomes (1,0)
    strToPair :: String -> Either ParseError (Int, Int)
    strToPair square@[col, row] = do
      ncol <- parseCol square col
      nrow <- parseRow square row
      pure (ncol, nrow)
    strToPair str = Left (InvalidSquare str)

    -- Returns colum or row as a number or an error
    parseCol :: String -> Char -> Either ParseError Int
    parseCol square col = maybe errParse Right (lookup col colums)
      where
        errParse |col `elem` ['a'..'z'] = Left (OutsideBoard square)
                 |otherwise = Left (InvalidCol [col] square)

    parseRow :: String -> Char -> Either ParseError Int
    parseRow square row = maybe errParse Right (lookup row rows)
      where
        errParse |isDigit row = Left (OutsideBoard square)
                 |otherwise = Left (InvalidRow [row] square)

    -- Builds a position as a single Int
    pairToPos (x, y) = x + w * y
  in do
   sq <- checkInitial squares
   -- builds a [(Int,Int)] from the [String] and performs some checks…
   ls <- foldrM reduce [] sq
   -- Checks validity of jumps
   ls' <- checkJumps squares ls
   -- map (Int, Int) to position
   pure (map pairToPos ls')

-- checkInitial: checks there aren't duplicate squares
checkInitial :: [String] -> Either ParseError [String]
checkInitial sqs
  |sqs == nubOrd sqs = Right sqs
  |otherwise = Left (DuplicateSquare (show sqs))

checkJumps :: [String] -> [(Int, Int)] -> Either ParseError [(Int,Int)]
checkJumps squares xs
  |all (uncurry validJump) (zip xs (tail xs)) = Right xs
  |otherwise = Left (InvalidJumps (show squares))
  where
    validJump (x, y) (x', y') = (x' - x, y' - y) `elem` jumps
    -- build a list of jumps
    deltas = [1,2,-2, -1]
    jumps = [(i, j)| i <- deltas, j <- deltas, abs i /= abs j]

-- Utilities to manage errors during the parsing of the initial list of
-- jumps. We use a custom error data type in Either
data ParseError = DuplicateSquare String
                  |InvalidJumps String
                  |InvalidSquare String
                  |InvalidCol String String
                  |InvalidRow String String
                  |OutsideBoard String
                  |InvalidDimension String

instance Show ParseError where
  show = \case
    DuplicateSquare str -> "There are duplicate squares: " <> str
    InvalidJumps str -> "Invalid inital jumps: " <> str
    InvalidSquare str -> "Invalid square name: " <> str
    InvalidCol str sq -> "Invalid column: " <> str <> " in square: " <> sq
    InvalidRow str sq -> "invalid row: " <> str <> " in square: " <> sq
    OutsideBoard str  -> "The square is outside of the board: " <> str
    InvalidDimension str -> "invalid dimension. It must be between (1,1) and (9,9) : " <> str
