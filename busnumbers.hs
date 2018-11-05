import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List

main = do
  a <- getLine
  b <- B.getLine
  (putStrLn . solve . sort . parse) b

parse::B.ByteString -> [Int]
parse a = map (fst . fromJust . B.readInt) (B.words a)

solve::[Int] -> String
solve (a:[]) = (show a)
solve (a:(b:[])) = (show a) ++ " " ++ (show b)
solve (a:(b:(c:[]))) | (a+2) == c = (show a) ++ "-" ++ (show c)
solve (a:(b:(c:[]))) = (show a) ++ " " ++ (show b) ++ " " ++ (show c)
solve (a:(b:(c:rst))) = (show a) ++ (solvee ((b:(c:rst)), ((a+2)==c)))

solvee::([Int], Bool) -> String
solvee ((a:(b:(c:[]))), t)
  | (t == True) && ((a+2) == c) = "-" ++ (show c)
  | (t == True) && ((a+1) == b) = "-" ++ (show b) ++ " " ++ (show c)
  | (t == True) = "-" ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c)
  | (a+2) == c = " " ++ (show a) ++ "-" ++ (show c)
  | otherwise = " " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c)
solvee ((a:(b:(c:rst))), t)
  | ((a+1) < b) && t = "-" ++ (show a) ++ (solvee ((b:(c:rst)), False))
  | ((a+1) < b) = " " ++ (show a) ++ (solvee ((b:(c:rst)), t))
  | (t == False) && ((a+2)==c) = " " ++ (show a) ++ (solvee ((b:(c:rst)), True))
  | (t == False) = " " ++ (show a) ++ (solvee ((b:(c:rst)), False))
  | otherwise = solvee ((b:(c:rst)), t)

rangeend::Int -> String
rangeend a = "-" ++ (show a) ++ " "
