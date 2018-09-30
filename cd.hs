import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List
import Data.Char

main = do
  inp <- B.getContents
  putStrLn (sol inp)

sol = format . solve . parse

parse::B.ByteString -> [([Int], [Int])]
parse a = parsee (map (map (fst . fromJust . B.readInt)) (map B.words (B.lines a)))

parsee::[[Int]] -> [([Int], [Int])]
parsee ((a:(b:[])):rst) = parsee1 (a, [], b, [], rst)

parsee1::(Int, [Int], Int, [Int], [[Int]]) -> [([Int], [Int])]
parsee1 (0, _, 0, _, _) = []
parsee1 (a, b, c, d, ((l:[]):rst)) | a > 0 = parsee1 ((a-1), (l:b), c, d, rst)
                                   | otherwise = parsee2 (a, b, (c-1), (l:d), rst)

parsee2::(Int, [Int], Int, [Int], [[Int]]) -> [([Int], [Int])]
parsee2 (0, a, 0, c, l) = ((a, c):(parsee l))
parsee2 (_, c, a, b, []) = [(c, (a:b))]
parsee2 (z, a, b, c, ((l:[]):rst)) | b > 0 = parsee2 (z, a, (b-1), (l:c), rst)

solve::[([Int], [Int])] -> [Int]
solve [] = []
solve (a:rst) = ((solvee a):solve rst)

solvee::([Int], [Int]) -> Int
solvee ([], _) = 0
solvee (_, []) = 0
solvee ((a:arst), (b:brst)) | a == b = 1 + (solvee (arst, brst))
                            | a > b = solvee (arst, (b:brst))
                            | otherwise = solvee ((a:arst), brst)

format::[Int] -> String
format (a:[]) = show a
format (a:rst) = (show a) ++ "\n" ++ (format rst)


