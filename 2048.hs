main = do
  a <- getLine
  b <- getLine
  c <- getLine
  d <- getLine
  e <- getLine
  (putStrLn . format . solv . parse) (a, b, c, d, e)

parse::(String, String, String, String, String) -> ([[Int]], Int)
parse (a, b, c, d, e) = ([(parsee a), (parsee b), (parsee c), (parsee d)], read e)

parsee::String -> [Int]
parsee s = map read (words s)

solv::([[Int]], Int) -> [[Int]]
solv (a, e) | e == 0 = solve a
            | e == 1 = (rotate . solve) (rotate a)
            | e == 2 = map reverse (solve (map reverse a))
            | e == 3 = (rotatee . solve) (rotatee a)

solve::[[Int]] -> [[Int]]
solve [] = []
solve (a:rst) = ((solvee a):(solve rst))

solvee::[Int] -> [Int]
solvee a = take 4 (merge ((filter (>0) a) ++ (repeat 0)))

merge::[Int] -> [Int]
merge [] = []
merge (a:(b:rst)) | a == b = ((a+b):(merge rst))
                  | otherwise = (a:(merge (b:rst)))

rotate::[[Int]] -> [[Int]]
rotate ((a:(b:(c:(d:[])))):((e:(f:(g:(h:[])))):((i:(j:(k:(l:[])))):(m:(n:(o:(p:[])))):[])))
  = [[a,e,i,m],[b,f,j,n],[c,g,k,o],[d,h,l,p]]

rotatee::[[Int]] -> [[Int]]
rotatee ((a:(b:(c:(d:[])))):((e:(f:(g:(h:[])))):((i:(j:(k:(l:[])))):(m:(n:(o:(p:[])))):[])))
  = [[p,l,h,d],[o,k,g,c],[n,j,f,b],[m,i,e,a]]


format::[[Int]] -> String
format (a:[]) = formatt a
format (a:rst) = (formatt a) ++ "\n" ++ (format rst)

formatt::[Int] -> String
formatt (a:(b:(c:(d:[])))) = (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d)
