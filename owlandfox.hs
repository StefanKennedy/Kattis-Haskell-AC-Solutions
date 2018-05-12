import Data.Char

main = do
  n <- getLine
  nums <- mapM (const getLine) [1..(read n)]
  putStrLn (solveall nums)
  
sumdig = (sum . map digitToInt . show)

searchFor::(Int, Int) -> Int
searchFor (on, for) | (sumdig on) == ((sumdig for) - 1) = on
                    | otherwise = searchFor ((on-1), for)

search::Int -> Int
search a =  searchFor((a-1),a)

solve::[Int] -> [Int]
solve [] = []
solve (a:b) = ((search a):solve(b))

solveall::[String] -> String
solveall = format . solve . parse

parse::[String] -> [Int]
parse [] = []
parse (a:b) = ((read a):(parse b))

format::[Int] -> String
format [] = ""
format (a:b) = (show a)++" "++(format b)
