import qualified Data.Map as Map

main = do
  inp <- getContents
  putStrLn (sol inp)

sol = format . path . parse

parse::String -> (Int, Map.Map Int Int, [Int])
parse s = parsee (map (map read) (map words (lines s)))

parsee::[[Int]] -> (Int, Map.Map Int Int, [Int])
parsee ((a:[]):rst) = (a, (mapped (rst, Map.empty)), [])

mapped::([[Int]], Map.Map Int Int) -> Map.Map Int Int
mapped ([], m) = m
mapped (((a:b):rst), m) = put (b, a, (mapped (rst, m)))

put::([Int], Int, Map.Map Int Int) -> Map.Map Int Int
put ([], _, m) = m
put ((k:rst), a, m) = Map.insert k a (put (rst, a, m))

path::(Int, Map.Map Int Int, [Int]) -> [Int]
path (c, m, sofar) | (Map.member c m) == False = (c:sofar)
                   | otherwise = path (m Map.! c, m, (c:sofar))

format::[Int] -> String
format (a:[]) = show a
format (a:rst) = format(rst) ++ " " ++ (show a)
