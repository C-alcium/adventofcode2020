{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Void
import Control.Monad.Combinators
import Text.Megaparsec
import Data.Maybe

type Parser = Parsec Void String

rows :: [Integer]
rows = [0..127]

columns :: [Integer]
columns = [0..7]

main :: IO ()
main = do
  inputs <- lines <$> readFile "input.txt"
  let parsedInputs = map directionalResult inputs 
  let res1 = solve1 parsedInputs
  print res1 

dropBack :: [a] -> [a]
dropBack [] = []
dropBack xs = take (length xs `div` 2) xs 

dropFront :: [a] -> [a]
dropFront [] = []
dropFront xs = reverse $ dropBack (reverse xs) 

directionalResult :: String -> (String, String)
directionalResult s = fromJust $ parseMaybe parseDirections s 

parseDirections :: Parser (String, String)
parseDirections = do 
  rowInstructions    <- count 7 $ oneOf ['F', 'B'] :: Parser String
  columnInstructions <- count 3 $ oneOf ['R', 'L'] :: Parser String
  _                  <- eof 
  return (rowInstructions, columnInstructions)


convertRowToNumber :: String -> Int
convertRowToNumber xs = read ( (show . head) (convert xs rows)) :: Int 
  where
    step a rs = case a of
      'F' -> dropBack rs 
      'B' -> dropFront rs 
    convert [] rs     = rs 
    convert (x:xs) rs = convert xs (step x rs) 
    

convertColumnToNumber :: String -> Int
convertColumnToNumber xs = read ( (show . head) (convert xs columns)) :: Int 
  where
    step a rs = case a of
      'L' -> dropBack rs 
      'R' -> dropFront rs 
    convert [] rs     = rs 
    convert (x:xs) rs = convert xs (step x rs) 

solve1 :: [(String, String)] -> Int
solve1 xs = maximum $ map toID xs
  where
    toID (a, b) = convertRowToNumber a * 8 + convertColumnToNumber b