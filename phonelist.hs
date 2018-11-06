import Data.List
import Debug.Trace

main = do
  a <- getContents
  (putStrLn . (foldl (\x y -> x ++ y ++"\n") "") . solv . (drop 1) . lines) a

solv::[String] -> [String]
solv [] = []
solv (b:rst) = [(solve . sort . (take a)) rst] ++ (solv (drop a rst))
              where a = read b

solve::[String] -> String
solve (a:[]) = "YES"
solve (a:(b:[])) | isPrefixOf a b = "NO"
                 | otherwise = "YES"
solve (a:(b:rst)) | isPrefixOf a b = "NO"
                  | otherwise = solve (b:rst)
