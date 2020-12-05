import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )


main :: IO ()
main = do
    input <- openFile "input.txt" ReadMode
    contents <- hGetContents input

    let dataPoints = lines contents
    let numbers = map (\ a -> read a :: Integer) dataPoints
    
    let result = solveOne numbers
    let result2 = solveTwo numbers
    
    print result
    print result2

    hClose input

solveOne :: (Eq a, Num a) => [a] -> a
solveOne input = head [x * y | x <- input, y <- input, x + y == 2020]

solveTwo :: (Eq a, Num a) => [a] -> a
solveTwo input = head [ x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020]