{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List 

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let numbers = map (\ a -> read a :: Int) input
  let res1    = solve1 numbers
  print res1 
  print (sort numbers)
  print (length numbers)
 
solve1 :: [Int] -> Int 
solve1 xs = solve xs 0 0 0 
  where
    deviceJoltRating = maximum xs + 3
    solve xs currentRating amount1 amount3 
      | any (\ a -> (currentRating - a) == -1) xs = solve xs (currentRating + 1) (amount1 + 1) amount3
      | any (\ a -> (currentRating - a) == -2) xs = solve xs (currentRating + 2) amount1 amount3
      | any (\ a -> (currentRating - a) == -3) xs = solve xs (currentRating + 3) amount1 (amount3 + 1)
      | otherwise                                 = amount1 * (amount3 + 1) 

