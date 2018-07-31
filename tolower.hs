import Data.Char

main = do
  contents <- getContents
  putStrLn (show (sol contents))

sol = solve . parse

getP::[String] -> (Int, [String])
getP (firstline:rst) = ((read ((words firstline)!!1)), rst)

parse::String -> (Int, [String])
parse a = getP(lines a)

solve::(Int, [String]) -> Int
solve (cases, inputs) = solvee(cases, inputs, 0, 0, 1)

solvee::(Int, [String], Int, Int, Int) -> Int
solvee (_, [], _, ans, c) = ans
solvee (cases, (nxt:rst), on, ans, c)
    | cansolve(nxt) = solveee(cases, rst, on, ans, c)
    | otherwise = solveee(cases, rst, on, ans, 0)

solveee::(Int, [String], Int, Int, Int) -> Int
solveee (cases, l, on, ans, c)
    | (on+1) == cases = solvee(cases, l, 0, (ans+c), 1)
    | otherwise = solvee(cases, l, (on+1), ans, c)

cansolve::String -> Bool
cansolve (_:s) = all isLower s
