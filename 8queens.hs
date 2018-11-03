main = do
  a <- getLine
  b <- getLine
  c <- getLine
  d <- getLine
  e <- getLine
  f <- getLine
  g <- getLine
  h <- getLine
  (putStrLn . format . solve . parse) ([a, b, c, d, e, f, g, h], 0)

parse::([String], Integer) -> [(Integer, Integer)]
parse ([], _) = []
parse ((a:rst), b) = (parsee (a, b, 0)) ++ (parse(rst, (b+1)))

parsee::([Char], Integer, Integer) -> [(Integer, Integer)]
parsee ([], _, _) = []
parsee ((a:rst), b, c) | a == '*' = [(b, c)] ++ (parsee(rst, b, (c+1)))
                       | otherwise = parsee (rst, b, (c+1))

format::Bool -> String
format b | b = "valid"
         | otherwise = "invalid"

solve::[(Integer, Integer)] -> Bool
solve a | (length a) == 8 = ok (a, checks)
        | otherwise = False

ok::([(Integer, Integer)], [[(Integer, Integer)]]) -> Bool
ok (_, []) = True
ok (pos, (a:rst)) | isok (pos, a) = ok (pos, rst)
                  | otherwise = False

isok::([(Integer, Integer)], [(Integer, Integer)]) -> Bool
isok ([], _) = True
isok ((a:rst), c) | isokk (a, c) = isok (rst, c)
                  | otherwise = issok(rst, c)

issok::([(Integer, Integer)], [(Integer, Integer)]) -> Bool
issok ([], _) = True
issok ((a:rst), c) | isokk (a, c) = issok (rst, c)
                   | otherwise = False

isokk::((Integer, Integer), [(Integer, Integer)]) -> Bool
isokk (_, []) = True
isokk ((a, b), ((c, d):rst)) | (a == c) && (b == d) = False
                             | otherwise = isokk ((a, b), rst)

checks::[[(Integer, Integer)]]
checks = [[(i, j) | i <- [0..7]] | j <- [0..7]]
         ++ [[(j, i) | i <- [0..7]] | j <- [0..7]]
         ++ [[((i+j), i) | i <- [0..(7-j)]] | j <- [0..7]]
         ++ [[((i-j), i) | i <- [j..7]] | j <- [1..7]]
         ++ [[(i, (7-i-j)) | i <- [0..(7-j)]] | j <- [0..7]]
         ++ [[((i+j), (7-i)) | i <- [0..(7-j)]] | j <- [1..7]]
