{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Void 
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative hiding (many, some )

type Parser = Parsec Void String
type Acc = Int
type Instruction = (String, Int -> Int)

main :: IO ()
main = do 
  input <- lines <$> readFile "input.txt"
  let instructions = map (fromJust . parseMaybe pLine) input 
  let res1 = solve1 instructions
  print res1 
  let possibilities = permutations instructions 
  let res2 = solve2 instructions  
  print res2 

solve1 :: [Instruction] -> Acc
solve1 xs = step 0 0 xs []  
  where 
    step i a xs is = 
      if i `elem` is 
      then a 
      else 
      if i < length xs then case fst (xs!!i) of
        "nop" -> step (i + 1) a xs (is ++ [i]) 
        "acc" -> step (i + 1) (applySecond (xs!!i) a) xs (is ++ [i])
        "jmp" -> step (applySecond (xs!!i) i) a xs (is ++ [i]) 
      else -a

-- Basically a hack, if the index on the permutation doesn't exist solve1 will return 
-- a negative accumulator. 
solve2 :: [Instruction] -> [Acc] 
solve2 xs = res 
  where 
    allP = permutations xs 
    res  = filter (<0) $ map solve1 allP

permutations :: [Instruction] -> [[Instruction]]
permutations xs = modifyAfter [[]] ps 0  
  where
    ps                          = replicate (length xs) xs  
    modifyAfter ds [] _         = ds
    modifyAfter ds (y:ys) index = 
        modifyAfter (append ds (changeOne matches invert y index)) ys (index + 1)  

changeOne :: (a -> Bool) -> (a -> a) -> [a] -> Int -> [a]
changeOne p f xs i = (reverse . drop (length xs - i) . reverse) xs ++ changeOne' p f zs []
  where 
    zs                       = drop i xs 
    changeOne' p f []     ys = ys 
    changeOne' p f (x:xs) ys 
      | p x       = ys ++ [f x] ++ xs
      | otherwise = changeOne' p f xs (ys ++ [x])

invert :: Instruction -> Instruction 
invert (a, b) 
  | a == "jmp" = ("nop", b)
  | a == "nop" = ("jmp", b)
  | otherwise  = (a, b)

matches :: Instruction -> Bool
matches (a, b)
  | a == "jmp" = True 
  | a == "nop" = True 
  | otherwise  = False

pLine :: Parser (String, Int -> Int)
pLine = do
  command  <- choice (map string ["nop", "acc", "jmp"]) :: Parser String 
  _        <- spaceChar 
  sign     <- char '+' <|> char '-'
  rawQuantity <- manyTill digitChar eof
  return (command, f sign rawQuantity)
  where 
    f :: Char -> String -> (Int -> Int)
    f s q = case s of
      '+' -> (+ r)
      '-' -> \ a -> a - r  
      _   -> error ""
      where
        r = read q :: Int

applySecond :: (a, t1 -> t2) -> t1 -> t2
applySecond (_, f) = f

append :: [[a]] -> [a] -> [[a]]
append xs ys = xs ++ [ys] 
