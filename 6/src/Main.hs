{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.Split ( splitOn )
import Data.List ( nub, sort)

main :: IO ()
main = do
  groups <-  splitOn "\n\n" <$> readFile "input.txt"
  let res1 = sum $ map (length . sort . nub . filter (/= '\n')) groups  
  let res2 = sum $ map solve2 groups
  print res1 
  print res2 

solve2 :: String -> Int
solve2 s = length common 
  where
    xs        = splitOn "\n" s 
    sets      = map nub xs 
    allUnique = concat sets 
    common = nub [y | y <- allUnique, all (y `elem`) sets]
