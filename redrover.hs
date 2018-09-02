import Data.List
import qualified Data.Set as Set
     
main = do
  inp <- getLine
  putStrLn (sol inp)

sol = show . solve

solve::[Char] -> Int 
solve s = ((length s) - best (s, (options s)))

options::[Char] -> Set.Set [Char] 
options a = Set.fromList ((filter (not . null) . concatMap inits . tails) a)

best::([Char], Set.Set [Char]) -> Int
best (s, ops) = Set.foldl(\acc x -> if macr (s, x) > acc then macr (s, x) else acc) 0 ops

macr::([Char], [Char]) -> Int
macr (a, b) = pro (b, (macro (a, b)))

pro::([Char], Int) -> Int
pro (a, i) = ((((length a) * i) - i) - (length a))

macro::([Char], [Char]) -> Int
macro (a, b) | (length a) < (length b) = 0
macro ([], _) = 0
macro ((s:t), op) | match ((s:t), op) = 1 + macro((skip (op, (s:t))), op)
                  | otherwise = macro(t, op)

match::([Char], [Char]) -> Bool
match ([], (_:_)) = False
match ((_:_), []) = True
match ([], []) = True
match ((a:b), (c:d)) | a == c = match (b, d)
                     | otherwise = False

skip::([Char], [Char]) -> [Char]
skip ([], q) = q
skip ((a:b), (c:d)) = skip (b, d)
