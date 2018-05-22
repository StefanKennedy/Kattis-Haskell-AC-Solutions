import Debug.Trace

main = do
  a <- getLine
  putStrLn (sol a)

sol = format . solve . parse

gofo::[Int] -> String
gofo (a:rst) = (sol (show a)) ++ "\n" ++ ( gofo rst )

parse::String -> Integer
parse = read

step::(Integer, [Integer], Integer) -> Integer
step (b, (a:rst), _) | (a * a) > b = 1
step (_, [], _) = 1
step (a, (b:rst), c) | rem a b == 0 = step ((div a b), (b:rst), c) + 1
                     | otherwise = step(a, rst, c)

solve::Integer -> Integer
solve a = (step (a, [2..31630], a))

format::Integer -> String
format a = show a
