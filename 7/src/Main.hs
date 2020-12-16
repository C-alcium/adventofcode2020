{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Void ( Void )
import Data.Maybe ( fromJust, catMaybes )
import Data.List.Split ( splitOn ) 
import Data.List ( isInfixOf )
import Control.Applicative hiding (many, some )
import Text.Megaparsec 
import Text.Megaparsec.Char 

import qualified Data.Map.Strict as Map  



type Parser = Parsec Void String 

myBag :: String
myBag = "shinygold"

main :: IO ()
main = do
  raw <- lines <$> readFile "input.txt"
  let input = map parseLine raw
  let rules = map (\(a, b) -> (a, map (parseMaybe pRule) b)) input
  let rulesMap = Map.fromList rules 
  let res1 = length $ filter (/= False) $ map (canContainMyBag rulesMap) (Map.keys rulesMap) 
  print res1 

canContainMyBag :: Map.Map String [Maybe (Int, String)] -> String -> Bool
canContainMyBag m k = case Map.lookup k m of
  Nothing    -> False 
  Just rules -> if any (\ (_, b) -> b == myBag) (values rules)
                then True 
                else any (canContainMyBag m) (keys (values rules))
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
  return (clean key, left)
  where 
    clean s = reverse $ drop 4 $ reverse $ filter (\a -> a /= ' ' && a /= ',' && a /= '.') s 
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
  where
    clean = filter (\a -> a /= ' ' && a /= ',' && a /= '.')
