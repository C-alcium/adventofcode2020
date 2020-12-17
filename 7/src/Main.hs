{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Void ( Void )
import Data.Maybe ( fromJust, catMaybes )
import Data.List.Split ( splitOn ) 
import Data.List ( isInfixOf, all )
import Control.Applicative hiding (many, some )
import Text.Megaparsec 
import Text.Megaparsec.Char 

import qualified Data.Map.Strict as Map  


type BagMap = Map.Map String [Maybe (Int, String)] 
type BetterBagMap = Map.Map String [(Int, String)]
type Parser = Parsec Void String 

myBag :: String
myBag = "shinygold"

main :: IO ()
main = do
  raw <- lines <$> readFile "input.txt"
  let input = map parseLine raw
  let rules = map (\(a, b) -> (a, map (parseMaybe pRule) b)) input
  let rulesMap = Map.fromList rules 
  let keys = Map.keys rulesMap 
  let res1 = length $ filter (/= False) $ map (canContainMyBag rulesMap) keys
  print res1 
  let betterMap = Map.fromList (map (\ (a,b) -> (a, catMaybes b)) rules)
  let res2 = allBagsInBag betterMap myBag 1 
  print res2 

allBagsInBag :: BetterBagMap -> String -> Int -> Int  
allBagsInBag m k a = sumBagContents current 
    where 
      current = fromJust (Map.lookup k m)
      sumBagContents []    = 0 
      sumBagContents rules = sum $ map (\ (q, s) -> (q * a) + allBagsInBag m s q * a) rules

canContainMyBag :: BagMap -> String -> Bool
canContainMyBag m k = case Map.lookup k m of
  Nothing    -> False 
  Just rules -> any (\ (_, b) -> b == myBag) (values rules) 
             || any (canContainMyBag m) (keys (values rules))
  where
    values r = catMaybes r 
    keys     = map snd 

parseLine :: String -> (String, [String])
parseLine s = fromJust $ parseMaybe pLine s 

pLine :: Parser (String, [String])
pLine = do
  key   <- manyTill anySingle (string "contain")
  raw   <- manyTill anySingle eof 
  let left = contents raw 
  return (toKey key, left)
  where 
    toKey s = (reverse . drop 4 . reverse . clean) s 
    contents :: String -> [String]
    contents s  
      | "," `isInfixOf` s = splitOn "," s  
      | otherwise         = [s]

pRule :: Parser (Int, String)
pRule = do
  _        <- try $ some (spaceChar <|> char ',' <|> spaceChar) 
  quantity <- manyTill digitChar spaceChar 
  bag      <- manyTill anySingle (string " bag")
  _        <- manyTill anySingle eof 
  return (read quantity :: Int, clean bag)

clean :: String -> String 
clean = filter (\a -> a /= ' ' && a /= ',' && a /= '.')