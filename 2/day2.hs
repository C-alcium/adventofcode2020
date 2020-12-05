import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )

type ParsedEntry1 = (Int, Int, Char, String, Int, Bool)
type ParsedEntry2 = (Int, Int, Char, String, Bool)

main :: IO ()
main = do
    input <- openFile "input.txt" ReadMode
    contents <- hGetContents input

    let entries        = lines contents
    let parsedEntries1 = map parseEntry1 entries
    let result1        = solveOne parsedEntries1

    print result1

    let parsedEntries2 = map parseEntry2 entries
    let result2        = solveTwo parsedEntries2
    
    print result2

    hClose input

solveOne :: [ParsedEntry1] -> Int
solveOne entries = length $ filter (\ (_, _, _, _, _, a ) -> a) entries 

parseEntry1 :: String -> ParsedEntry1
parseEntry1 entry = (low, high, character, password, amount, isValid)
    where
        low       = read (takeWhile (/= '-') entry) :: Int 
        high      = read (takeWhile (/= ' ') (tail (dropWhile (/= '-') entry))) :: Int 
        character = last $ takeWhile (/= ':') entry
        password  = (tail . tail) $ dropWhile (/= ':') entry
        amount    = length $ filter (== character) password
        isValid   = amount >= low && amount <= high

solveTwo :: [ParsedEntry2] -> Int
solveTwo entries = length $ filter (\ (_, _, _, _, a ) -> a) entries 

parseEntry2 :: String -> ParsedEntry2
parseEntry2 entry = (low, high, character, password, isValid)
    where
        low       = read (takeWhile (/= '-') entry) :: Int 
        high      = read (takeWhile (/= ' ') (tail (dropWhile (/= '-') entry))) :: Int 
        character = last $ takeWhile (/= ':') entry
        password  = (tail . tail) $ dropWhile (/= ':') entry
        isValid   = (password!!(low - 1) == character && password!!(high - 1) /= character) || (password!!(low - 1) /= character && password!!(high - 1) == character)