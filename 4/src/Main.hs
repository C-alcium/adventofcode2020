{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import Data.List.Split (splitOn)
import Data.List (isPrefixOf)
import Data.Maybe ( isJust )

import Lens.Micro.Platform ( view, makeLenses )

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
  let amountOfValidPassports = length $ filter isValidPassport passports
  print amountOfValidPassports 

parsePassports :: [String] -> [Passport]
parsePassports = map parsePassport
  where
    parsePassport :: String -> Passport
    parsePassport p = Passport byr iyr eyr hgt hcl ecl pid cid
      where
        chunks :: [String]
        chunks = splitOn " " $ map (\ a -> if a == '\n' then ' ' else a) p 
        f      = parseDataPoint chunks
        byr    = f "byr"
        iyr    = f "iyr"
        eyr    = f "eyr"
        hgt    = f "hgt"
        hcl    = f "hcl"
        ecl    = f "ecl"
        pid    = f "pid"
        cid    = f "cid"

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