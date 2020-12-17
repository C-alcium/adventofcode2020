{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Void 
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative hiding (many, some )

type Parser = Parsec Void String
type Acc = Int
type Index = Int
type Instructions = [(String, Int -> Int)]

main :: IO ()
main = do 
  input <- lines <$> readFile "input.txt"
  let instructions = map (fromJust . parseMaybe pLine) input 
  let res1 = solve1 instructions
  print res1 


solve1 :: Instructions -> Acc
solve1 xs = step 0 0 xs []  
  where 
    applySecond (_, f) a = f a 
    step i a xs is = if i `elem` is 
                     then a 
                     else case fst (xs!!i) of
                       "nop" -> step (i + 1) a xs (is ++ [i]) 
                       "acc" -> step (i + 1) (applySecond (xs!!i) a) xs (is ++ [i])
                       "jmp" -> step (applySecond (xs!!i) i) a xs (is ++ [i]) 
    

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