tree :: Char
tree = '#'

main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let result = solve inputs
    print result


solve :: [String] -> Int
solve inputs = solve 0 0 0
    where
        stepLength   = length $ head inputs
        bottomCoord  = length inputs 
        step d r     = (inputs!!d!!mod r stepLength) == tree
        solve d r c
            | d == bottomCoord = c 
            | otherwise        = solve (d + 1) (r + 3) (if step d r then c + 1 else c) 
