module Main where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

main :: IO ()
main = do
  contents <- readFile "data"
  let result1 = partOne contents
  putStrLn $ "Part 1: " ++ (show result1)
  let result2 = partTwo contents
  putStrLn $ "Part 2: " ++ (show result2)

type Pos = (Int, Int)

partOne :: String -> Int
partOne contents =
  let
    lineContents = Seq.fromList $ lines contents
    gridSeq = foldr1 (Seq.><) $ Seq.mapWithIndex (\i x -> lineIndex i x) lineContents
    grid = Map.fromList $ toList gridSeq
  in
    Map.foldr (+) 0 $ Map.mapWithKey (\k _ -> xmasAtPos grid k) grid

lineIndex :: Int -> String -> Seq (Pos, Char)
lineIndex y str =
  let
    chars = Seq.fromList str
  in
    Seq.mapWithIndex (\i x -> ((i, y), x)) chars


partTwo :: String -> Int
partTwo contents =
  let
    lineContents = Seq.fromList $ lines contents
    gridSeq = foldr1 (Seq.><) $ Seq.mapWithIndex (\i x -> lineIndex i x) lineContents
    grid = Map.fromList $ toList gridSeq
  in
    length $ Map.filter id $ Map.mapWithKey (\k _ -> crossMasAtPos grid k) grid

crossMasAtPos :: Map Pos Char -> Pos -> Bool
crossMasAtPos grid pos =
  if cellContent == Just 'M' then
    isLetterAt grid (x+1, y+1) 'A' && isLetterAt grid (x+2, y+2) 'S' && ((isLetterAt grid (x, y+2) 'S' && isLetterAt grid (x+2, y) 'M') || (isLetterAt grid (x, y+2) 'M' && isLetterAt grid (x+2, y) 'S'))
  else if cellContent == Just 'S' then
    isLetterAt grid (x+1, y+1) 'A' && isLetterAt grid (x+2, y+2) 'M' && ((isLetterAt grid (x, y+2) 'M' && isLetterAt grid (x+2, y) 'S') || (isLetterAt grid (x, y+2) 'S' && isLetterAt grid (x+2, y) 'M'))
  else
    False
  where
    (x, y) = pos
    cellContent = Map.lookup pos grid

isLetterAt :: Map Pos Char -> Pos -> Char -> Bool
isLetterAt grid pos x = Map.lookup pos grid == Just x

xmasAtPos :: Map Pos Char -> Pos -> Int
xmasAtPos grid pos =
  if Map.lookup pos grid == Just 'X' then
    length $ filter id [xmasUp, xmasDown, xmasRight, xmasLeft, xmasUpLeft, xmasUpRight, xmasDownLeft, xmasDownRight]
  else
    0
  where
    (x, y) = pos
    xmasUp = xmasStr grid pos (\(x, y) -> (x, y - 1))
    xmasDown = xmasStr grid pos (\(x, y) -> (x, y + 1))
    xmasRight = xmasStr grid pos (\(x, y) -> (x + 1, y))
    xmasLeft = xmasStr grid pos (\(x, y) -> (x - 1, y))
    xmasUpLeft = xmasStr grid pos (\(x, y) -> (x - 1, y - 1))
    xmasUpRight = xmasStr grid pos (\(x, y) -> (x + 1, y - 1))
    xmasDownLeft = xmasStr grid pos (\(x, y) -> (x - 1, y + 1))
    xmasDownRight = xmasStr grid pos (\(x, y) -> (x + 1, y + 1))


xmasStr :: Map Pos Char -> Pos -> (Pos -> Pos) -> Bool
xmasStr grid pos fPos =
  xmasStr' "XMAS" grid pos fPos

xmasStr' :: String -> Map Pos Char -> Pos -> (Pos -> Pos) -> Bool
xmasStr' [] _ _ _ = True
xmasStr' (x:xs) grid pos fPos =
  if Map.lookup pos grid == Just x then
    xmasStr' xs grid (fPos pos) fPos
  else
    False
