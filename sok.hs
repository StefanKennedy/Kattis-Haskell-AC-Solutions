import Text.Printf

main = do
  a <- getLine
  b <- getLine
  putStrLn (sol (a, b))

sol::(String, String) -> String
sol (amounts, ratio) = frm (getdiff (prsw (amounts, ratio, (
    times (prs (amounts, ratio))))))

prsr::String->[Int]
prsr a = map read (words a)

prs::(String, String) -> ([Int], [Int])
prs (frst, scnd) = ((prsr frst), (prsr scnd))

prsw::(String, String, Float) -> ([Int], [Int], Float)
prsw (frst, scnd, f) = ((prsr frst), (prsr scnd), f)

minn::(Float, Float) -> Float
minn (a, b) | a < b = a
            | otherwise = b

diev::(Int, Int) -> Float
diev (a, b) = (fromIntegral a) / (fromIntegral b)

times::([Int], [Int]) -> Float
times ([], []) = 1000000
times ((a:amounts), (b:ratio)) = minn ((diev (a, b)), times (amounts, ratio))

lediff::(Int, Int, Float) -> Float
lediff (a, b, c) = (fromIntegral a) - ((fromIntegral b) * c)

getdiff::([Int], [Int], Float) -> [Float]
getdiff (a, b, c) = [
   (lediff (a!!0, b!!0, c)),
   (lediff (a!!1, b!!1, c)),
   (lediff (a!!2, b!!2, c))]

frm::[Float]->String
frm a = (printf "%.6f" (a!!0))++" "++(printf "%.6f" (a!!1))++" "++(printf "%.6f" (a!!2))
