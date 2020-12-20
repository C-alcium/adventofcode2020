{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Set as Set

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let numbers = map (\ a -> read a :: Integer) input
  let res1 = solve1 numbers
  print res1 


solve1 :: [Integer] -> Integer 
solve1 xs = loop xs preamble
  where
    loop xs i 
      | anySumMatches (take preamble xs) (xs!!i) = loop (tail xs) i
      | otherwise = xs!!i 

preamble :: Int 
preamble = 25 
  
anySumMatches :: [Integer] -> Integer -> Bool
anySumMatches xs x = x `Set.member` preSet 
  where
    preSet = Set.fromList [ z + y | z <- xs, y <- xs] 


