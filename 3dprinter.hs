main = do
  a <- getLine
  (putStrLn . show . solve . read) a

solve::Integer -> Integer
solve a = (ceiling (logBase 2 (fromIntegral a))) + 1
