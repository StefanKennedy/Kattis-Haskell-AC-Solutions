main = do
  getLine
  contents <- getContents
  mapM putStrLn (sol contents)

sol = solve . parse

parse::String -> [[Int]]
parse a = map (map read) (map words (lines a))

solve::[[Int]] -> [String]
solve ((h:(z:[])):rst) = ((show (h-1)):solvee(z, rst))

solvee::(Int, [[Int]]) -> [String]
solvee (_, []) = []
solvee (a, (h:rst))
    | a == 0 = solve(h:rst)
    | otherwise = solvee((a-1), rst)
