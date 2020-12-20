{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Set as Set
import Data.List ( subsequences, find, isInfixOf, findIndices, sort) 
import Data.List.Split.Internals ( splitPlaces )
import Data.Maybe ( fromJust )

preamble :: Int 
preamble = 25 

answer1 :: Integer
answer1 = 105950735 

main :: IO ()
main          = do
  input <- lines <$> readFile "input.txt"
  let numbers = map (\ a -> read a :: Integer) input
  let res1    = solve1 numbers
  print res1 
  let res2    = solve2 numbers
  print res2


solve1 :: [Integer] -> Integer 
solve1 xs = loop xs preamble
  where
    loop xs i 
      | anySumMatches (take preamble xs) (xs!!i) = loop (tail xs) i
      | otherwise                                = xs!!i

solve2 :: [Integer] -> Integer
solve2 xs = findFastContigualSum ys answer1
  where
    ys = takeWhile (<(answer1 `div` 2)) xs
  
anySumMatches :: [Integer] -> Integer -> Bool
anySumMatches xs x = x `Set.member` preSet 
  where
    preSet = Set.fromList [ z + y | z <- xs, y <- xs] 


findFastContigualSum :: [Integer] -> Integer -> Integer
findFastContigualSum xs x = head ys + last ys 
  where 
    ys = sort (findSubArray xs x 0 1) 
    findSubArray xs x p1 p2 
      | sum (take p2 (drop p1 xs)) == x = take p2 (drop p1 xs)
      | otherwise                       = if p2 + 1 /= length xs 
                                          then findSubArray xs x p1 (p2 + 1)
                                          else findSubArray xs x (p1 + 1) 1 
