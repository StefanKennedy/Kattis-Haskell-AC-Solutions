main = do
  a <- getContents
  (putStrLn . (foldl (\x y -> x ++ (foldl (\o p -> o ++ p ++ " ") "" y) ++ "\n") "") . sol . (drop 1) . lines) a

sol::[String] -> [[String]]
sol [] = []
sol (a:rst) = solve a [] rst 

solve::String -> [String] -> [String] -> [[String]]
solve _ _ [] = []
solve a b (c:rst) | c == "what does the fox say?" = [solv (words a) b] ++ (sol rst)
                  | otherwise = solve a (b++[c]) rst

solv::[String] -> [String] -> [String]
solv a [] = a
solv a (b:rst) = solv (filter (/= (aaa (words b))) a) rst

aaa::[String] -> String
aaa (a:(b:(c:[]))) = c
