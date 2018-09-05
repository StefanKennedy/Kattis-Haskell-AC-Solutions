main = do
  inp <- getContents
  putStrLn (sol inp)

sol = format . solve . parse

parsee::[String] -> (Int, Int)
parsee (a:(b:[])) = ((read a), (read b))

parse::String -> [(Int, Int)]
parse s = map (parsee . words) (lines s)

solve::[(Int, Int)] -> [Int]
solve [] = []
solve ((a, b):rst) = ((solvee(a, b, 0)):(solve rst))

solvee::(Int, Int, Int) -> Int
solvee (a, b, c) | (a `mod` 365) == 0 && (b `mod` 687) == 0 = c
                 | otherwise = solvee ((a+1), (b+1), (c+1))

format::[Int] -> String
format f = formatt (f, 1)

formatt::([Int], Int) -> String
formatt ([], _) = ""
formatt ((a:rst), b) = "Case " ++ (show b) ++ ": " ++ (show a) ++ "\n" ++ formatt (rst, b+1)
