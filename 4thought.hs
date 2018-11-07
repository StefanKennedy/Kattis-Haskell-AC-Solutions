main = do
  a <- getContents
  (putStrLn . (foldl (\x y -> x ++ y ++ "\n") "") . solve . (map read) . (drop 1). lines) a

solve::[Int] -> [String]
solve [] = []
solve (a:rst) = [(sollv a)] ++ (solve rst)

sollv::Int -> String
sollv a = sol a (solv a 3 4 [])

sol::Int -> [String] -> String
sol _ [] = "no solution"
sol z a = (a!!0) ++ " = " ++ (show z)

solv::Int -> Int -> Int -> [String] -> [String]
solv n 0 c s | n == (evalmul s s [4, 4, 4, 4] []) = ["4 " ++ (s!!0) ++ " 4 " ++ (s!!1) ++ " 4 " ++ (s!!2) ++ " 4"]
             | otherwise = []
solv n d c s = (solv n (d-1) (c+4) (s++["+"]))
               ++ (solv n (d-1) (c-4) (s ++ ["-"]))
               ++ (solv n (d-1) (c*4) (s ++ ["*"]))
               ++ (solv n (d-1) (div c 4) (s ++ ["/"]))

evalmul::[String] -> [String] -> [Int] -> [Int] -> Int
evalmul a [] (b:[]) [] = b
evalmul a [] b d = evalplus a (d++b)
evalmul a (s:rsts) (i:(j:rsti)) d | s == "*" = evalmul a rsts ((i*j):rsti) d
                                  | s == "/" = evalmul a rsts ((div i j):rsti) d
                                  | otherwise = evalmul a rsts (j:rsti) (d ++ [i])

evalplus::[String] -> [Int] -> Int
evalplus _ (b:[]) = b
evalplus (s:rsts) (i:(j:rsti)) | s == "+" = evalplus rsts ((i+j):rsti)
                               | s == "-" = evalplus rsts ((i-j):rsti)
                               | otherwise = evalplus rsts (i:(j:rsti))
