module Main where

import qualified Data.List as List
import Text.Regex.Base
import Text.Regex.PCRE
import qualified Data.Text as T

main :: IO ()
main = do
  contents <- readFile "data"
  let result1 = partOne contents
  putStrLn $ "Part 1: " ++ (show result1)
  let result2 = partTwo contents
  putStrLn $ "Part 2: " ++ (show result2)

type Rule = (Int, Int)

partOne :: String -> Int
partOne contents =
  let
    (rules, lines) = parseFile contents
  in
    sum $ map getMiddleNumber $ filter (checkOrderRules rules) lines

partTwo :: String -> Int
partTwo contents =
  let
    (rules, lines) = parseFile contents
    invalidLines = filter (not . checkOrderRules rules) lines
  in
    sum $ map (getMiddleNumber . (fixLine rules)) invalidLines

fixLine :: [Rule] -> [Int] -> [Int]
fixLine _ [] = []
fixLine rules line =
  let
    cleanedRules = filter (\x -> elem (fst x) line) rules
    leftSide = map fst cleanedRules
    rightSide = map snd cleanedRules
    freeToUse = filter (\x -> notElem x rightSide) line
    selectedNum = head freeToUse
    newLine = filter (\x -> x /= selectedNum) line
  in
    selectedNum:(fixLine cleanedRules newLine)

checkOrderRules :: [Rule] -> [Int] -> Bool
checkOrderRules rules nums =
  all (checkOrderRule nums) rules 

checkOrderRule :: [Int] -> Rule -> Bool
checkOrderRule nums rule =
  let
    (before, after) = rule
    a = List.elemIndex before nums
    b = List.elemIndex after nums
  in
    case (a, b) of
      (Just a, Just b) -> a < b
      _ -> True

getMiddleNumber :: [Int] -> Int
getMiddleNumber nums =
  nums !! index
  where
    index = (length nums) `div` 2

parseFile :: String -> ([Rule], [[Int]])
parseFile contents =
  let
    rulesRegex = (contents =~ "([0-9]+)\\|([0-9]+)" :: [[String]])
    numsRule = getAllTextMatches $ (contents =~ "([0-9]+,)+[0-9]+") :: [String]
    rules = map (\x -> (read $ x !! 1, read $ x !! 2)) rulesRegex :: [Rule]
    numsText = map (T.splitOn (T.pack ",") . T.pack) numsRule :: [[T.Text]]
    numsStr = map (map T.unpack) numsText
    nums = map (map read) numsStr
  in
    (rules, nums)
