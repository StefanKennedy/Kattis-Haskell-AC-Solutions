
main = do
  getLine
  i <- getContents
  putStrLn (sol i)

sol = format . solve . parse

parse::String -> [Integer]
parse a = map toInteger (map read (lines a))

maxsub::(Integer, Integer, Integer) -> Integer
maxsub (a, b, c) = a - (b * (numsub (a, b, c)))

numsub::(Integer, Integer, Integer) -> Integer
numsub (a, b, c) = min (a `div` b) c

step::([Integer], Integer, Integer) -> Integer
step ((now:[]), bank, owned) = bank + (now * owned)
step ((now:(next:rst)), bank, owned) 
  | now > next = step ((next:rst), (bank + (now * owned)), 0) -- Sell
  | otherwise = step ((next:rst), maxsub(bank, now, (100000-owned)), (numsub(bank, now, (100000-owned)) + owned)) -- Invest

solve::[Integer] -> Integer
solve l = step (l, 100, 0)

format = show
