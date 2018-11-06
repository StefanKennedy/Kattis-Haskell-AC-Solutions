import Data.List
import Data.Maybe

main = do
  a <- getLine
  b <- getLine
  (putStrLn . format . (solve 6) . (map read) . words) b

solve::Int -> [Int] -> Int
solve 0 _ = -1
solve a b | z == 1 = fromJust $ elemIndex a b 
          | otherwise = solve (a-1) b
          where
            z = length (filter (==a) b)

format::Int -> String
format a | a == -1 = "none"
         | otherwise = show (a+1)
