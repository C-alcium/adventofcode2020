tree :: Char
tree = '#'

main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let answer1 = solve inputs 1 3
    print answer1
    let result1 = solve inputs 1 1
    let result2 = solve inputs 1 3
    let result3 = solve inputs 1 5
    let result4 = solve inputs 1 7
    let result5 = solve inputs 2 1
    let answer2 = result1 * result2 * result3 * result4 * result5
    print answer2 
    


solve :: [String] -> Int -> Int -> Int
solve inputs di ri = solve 0 0 0
    where
        stepLength   = length $ head inputs
        bottomCoord  = length inputs 
        step d r     = (inputs!!d!!mod r stepLength) == tree
        solve d r c
            | d >= bottomCoord = c 
            | otherwise        = solve (d + di) (r + ri) (if step d r then c + 1 else c) 
