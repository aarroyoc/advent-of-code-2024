module Main where

import Data.List
import Text.Regex.Posix

main :: IO ()
main = do
  contents <- readFile "input"
  let result1 = partOne contents
  putStrLn ("Part 1: " ++ (show result1))
  let result2 = partTwo contents
  putStrLn ("Part 2: " ++ (show result2))


partOne :: String -> Int
partOne contents =
  let
    nums = map parseStr (lines contents)
  in
    length $ filter isSafe nums

partTwo :: String -> Int
partTwo contents =
  let
    nums = map parseStr (lines contents)
  in
    length $ filter isSafeWithBadLevel nums


parseStr :: String -> [Int]
parseStr str =
  map read regexResults :: [Int]
  where
    regexResults = getAllTextMatches $ str =~ "([0-9]+)" :: [String]
  

steps :: [Int] -> [Int]
steps [x,y] = (y-x):[]
steps (x:y:xs) = (y-x):(steps (y:xs))
steps _ = error "Pattern unreachable"

isSafe :: [Int] -> Bool
isSafe nums =
  let
    steps_nums = steps nums
  in
    all (\x -> x > 0 && x < 4) steps_nums || all (\x -> x > -4 && x < 0) steps_nums

isSafeWithBadLevel :: [Int] -> Bool
isSafeWithBadLevel nums =
  let
    length_alts = (length nums) - 1 
    alternatives = filter (\x -> length x >= length_alts) (subsequences nums)
  in
    any isSafe alternatives
