import Debug.Trace

main = do
  a <- getLine
  b <- getLine
  putStrLn (sol (a, b))

sol = show . solv . parse

parse::(String, String) -> (Int, [Int])
parse (a, b) = ((read ((words a)!!1)), (map read (words b)))

solv::(Int, [Int]) -> Int
solv (a, b) = solvee ((getpc a), b)

solvee::([[Int]], [Int]) -> Int
solvee ([], _) = 0;
solvee ((a:rst), b) = solve (a, b) + solvee(rst, b)

solve::([Int], [Int]) -> Int
solve (_, []) = 0;
solve (a, (b:rst)) | fits (a, (b:rst)) = 1 + solve (a, rst)
                   | otherwise = solve(a, rst)

fitss::([Int], [Int], Int) -> Bool
fitss ([], _, _) = True
fitss (_, [], _) = False
-- fitss ((a:rsta), (b:rstb), h) | trace ("a: " ++ (show a) ++ " b: " ++ (show b) ++ " h: " ++ (show h)) False = undefined
-- fitss ((a:rsta), (b:rstb), h) | (a+h < b) || (a+h > b) && trace ("stop") False = undefined
fitss ((a:rsta), (b:rstb), h) | a+h == b = fitss ((rsta), (rstb), h)
                              | otherwise = False

fits::([Int], [Int]) -> Bool
fits (a, (b:rst)) = fitss(a, (b:rst), b)

getpc::Int -> [[Int]] -- Get bottom
getpc a | a == 1 = [[0, 0, 0, 0], [0]]
getpc a | a == 2 = [[0, 0]]
getpc a | a == 3 = [[0, 0, 1], [0, -1]]
getpc a | a == 4 = [[0, -1, -1], [0, 1]]
getpc a | a == 5 = [[0, 0, 0], [0, -1], [0, -1, 0], [0, 1]]
getpc a | a == 6 = [[0, 0, 0], [0, 0], [0, -2], [0, 1, 1]]
getpc a | a == 7 = [[0, 0, 0], [0, 0], [0, 0, -1], [0, 2]]
