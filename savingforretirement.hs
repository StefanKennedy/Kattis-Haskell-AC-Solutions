main = do
  nums <- getLine
  putStrLn (sol nums)

sol = show . solve . parse

parse::String -> (Int, Int, Int, Int)
parse s = parse2 (map read (words s))

parse2::[Int] -> (Int, Int, Int, Int)
parse2 (a:(b:(c:(d:(e:[]))))) = (((b-a)*c), d, e, 0)

solve::(Int, Int, Int, Int) -> Int
solve (a, b, c, d) | (c*d) > a = b + d
                   | otherwise = solve (a, b, c, (d+1)) 
