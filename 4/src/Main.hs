{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import Data.List.Split ( splitOn )
import Data.List ( isPrefixOf )
import Data.Maybe ( isJust )
import Data.Either ( isRight )
import Data.Void 

import Lens.Micro.Platform ( view, makeLenses )
import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec Void String 
type Metric = String

data Passport = Passport
                { _birthYear :: Maybe String
                , _issueYear :: Maybe String
                , _expirationYear :: Maybe String
                , _height :: Maybe String
                , _hairColor :: Maybe String
                , _eyeColor :: Maybe String
                , _passportID :: Maybe String
                , _countryID :: Maybe String
                } deriving(Show, Read, Eq)

makeLenses ''Passport

main :: IO ()
main = do
  inputs :: [String] 
    <- splitOn "\n\n" <$> readFile "input.txt"
  let passports :: [Passport] = parsePassports inputs
  let amountOfValidPassports1 = length $ filter isValidPassport passports
  print amountOfValidPassports1
  let amountOValidPassports2 = length $ filter isValidPassport2 passports 
  print amountOValidPassports2

parsePassports :: [String] -> [Passport]
parsePassports = map parsePassport
  where
    parsePassport :: String -> Passport
    parsePassport p = Passport byr iyr eyr hgt hcl ecl pid cid
      where
        chunks :: [String]
        chunks = splitOn " " $ map (\ a -> if a == '\n' then ' ' else a) p 
        f      = parseDataPoint chunks 
        g :: String -> Maybe String
        g a    = Just $ tail $ dropWhile (/= (':' :: Char)) a 
        byr    = f "byr" >>= g 
        iyr    = f "iyr" >>= g
        eyr    = f "eyr" >>= g
        hgt    = f "hgt" >>= g
        hcl    = f "hcl" >>= g
        ecl    = f "ecl" >>= g
        pid    = f "pid" >>= g
        cid    = f "cid" >>= g

parseDataPoint :: [String] -> String -> Maybe String
parseDataPoint chunks search = 
  case length res of 
    1 -> Just $ head res  
    _ -> Nothing 
    where
      res :: [String]
      res         = filter (isPrefixOf search) chunks

isValidPassport :: Passport -> Bool
isValidPassport p = isJust $ sequence res
  where
    bp = view birthYear p 
    ip = view issueYear p 
    ep = view expirationYear p
    hp = view height p
    hc = view hairColor p
    ec = view eyeColor p  
    pp = view passportID p
    res = [bp, ip, ep, hp, hc, ec, pp]

isValidPassport2 :: Passport -> Bool
isValidPassport2 p = all (== True) res
  where
    stringInRange r = maybe False (inRange r)
    get a = view a p 
    bv = stringInRange [1920..2002] $ get birthYear 
    iv = stringInRange [2010..2020] $ get issueYear
    ev = stringInRange [2020..2030] $ get expirationYear
    hgv = maybe False validHeight $ get height  
    hcv = maybe False validHairColor $ get hairColor
    ecv = maybe False validEyeColor $ get eyeColor  
    piv = maybe False validPassportID $ get passportID
    res = [bv, iv, ev, hgv, hcv, ecv, piv] 

inRange :: [Int] -> String -> Bool
inRange r s = s `elem` map show r 

validHeight :: String -> Bool 
validHeight s = case res of 
    Left _       -> False
    Right (m, i) -> case m of 
      "in" -> i `elem` [59..76]
      "cm" -> i `elem` [150..193]
  where
    pHeight :: Parser (Metric, Int)
    pHeight = do
      value  <- many digitChar 
      cmOrIN <- string' "in" <|> string' "cm"
      _      <- eof 
      let conversion = read value :: Int 
      return (cmOrIN, conversion) 
    res = parse pHeight "" s 

validHairColor :: String -> Bool
validHairColor s = isRight $ parse pHairColor "" s 
  where
    pHairColor :: Parser (Char, String) 
    pHairColor = do
      prefix <- char '#'
      suffix <- count 6 hexDigitChar
      _      <- eof 
      return (prefix, suffix)

validEyeColor :: String -> Bool
validEyeColor s = isRight $ parse pEyeColor "" s
  where
    pEyeColor :: Parser String 
    pEyeColor = do
      color <- choice $ map string' choices
      _     <- eof
      return color 
    choices   = [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

validPassportID :: String -> Bool
validPassportID s = isRight $ parse pPassportID "" s
  where
    pPassportID :: Parser String
    pPassportID = do
      pid <- count 9 digitChar 
      _   <- eof
      return pid 

-- Probably glue all of the parsers together like a normal person.
